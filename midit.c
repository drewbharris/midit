/*
 * midit (formerly aplaymidi as part of alsa-utils)
 *  - play Standard MIDI Files to sequencer port(s)
 *
 * Copyright (c) 2004-2006 Clemens Ladisch <clemens@ladisch.de>
 * Copyright (c) 2009-2016 mwnx <mwnx@gmx.com>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 */

/* TODO: sequencer queue timer selection ??? */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <getopt.h>
#include <unistd.h>
#include <signal.h>
#include <alsa/asoundlib.h>
//#include <limits.h>
//#include "aconfig.h"
//#include "version.h"
#include <termios.h>

#ifndef VERSION
#define VERSION "no_version"
#endif

#define VERBOSE_MAX 4
//#define VERBOSE_DEBUG 10
//#define dprintf(...) verbprintf(VERBOSE_DEBUG, __VA_ARGS__)

#define ALL_QUIET send_channel_mode(120, 0)	/* all_sound_off */
//#define ALL_QUIET send_channel_mode(123, 0)	/* all_notes_off */

/*
 * 31.25 kbaud, one start bit, eight data bits, two stop bits.
 * (The MIDI spec says one stop bit, but every transmitter uses two, just to be
 * sure, so we better not exceed that to avoid overflowing the output buffer.)
 */
#define MIDI_BYTES_PER_SEC (31250 / (1 + 8 + 2))

/*
 * A MIDI event after being parsed/loaded from the file.
 * There could be made a case for using snd_seq_event_t instead.
 */
struct event {
	struct event *next;		/* linked list */
	//struct event *prev;

	unsigned char type;		/* SND_SEQ_EVENT_xxx */
	unsigned char port;		/* port index */
	unsigned int tick;
	union {
		unsigned char d[3];	/* channel and data bytes */
		int tempo;
		unsigned int length;	/* length of sysex data */
	} data;
	unsigned char sysex[0];
};

struct track {
	struct event *first_event;	/* list of all events in this track */

	struct event *current_event;	/* used while loading and playing */
	struct event *last_event;
	int end_tick;			/* length of this track */
};

sig_atomic_t sigio_count;
struct sigaction sigio_sa;

enum {
	REPEAT_SHUFFLE = -2,
	REPEAT_ALL = -1,
	REPEAT_NONE = 0,
	REPEAT_CURRENT = 1
};

static snd_seq_t *seq;
static int client;
static int port_count;
static snd_seq_addr_t *ports;
static int queue;
static int end_delay = 2;
static const char *file_name;
static int file_index = -1;
static int file_index_increment = 0;	// 0=standard to next file without skipping
static int file_count = 0;
static int repeat_type = REPEAT_NONE;
static FILE *file;
static int file_offset;		/* current offset in input file */
static int num_tracks;
static struct track *tracks = NULL;
static int smpte_timing;
static int ppq;
static int original_tempo;
static int tempo;
static int tempo_percentage;
static int delta_time;
static struct termios *tio;
static int num_channels;
static int max_tick;
static int verbosity;
static int start_seek;
static long long time_from_start; //(seconds)
static long long max_time;		//(seconds)
static int max_time_s;		//max time minutes
static int max_time_m;		//max time seconds
static long long time_passed;
static struct track *tempo_track = NULL;
static int redirect_channel = -1;
static char no_pgmchange = 0;

static void interactiveUsage(void);

int verbprintf(int required_verbosity , char* s, ...)
{
	int ret = 1;
	va_list args;
	va_start (args, s);
	if ( verbosity >= required_verbosity )
		vprintf(s, args);
	else ret = 0;
	va_end(args);
	return ret;
}

void unbuffer_stdin()
{
	struct termios new_tio;
	//unsigned char c;

	/* get the terminal settings for stdin */
	tcgetattr(STDIN_FILENO,tio);

	/* we want to keep the old setting to restore them a the end */
	new_tio=*tio;

	/* disable canonical mode (buffered i/o) and local echo */
	new_tio.c_lflag &=(~ICANON & ~ECHO);

	/* set the new settings immediately */
	tcsetattr(STDIN_FILENO,TCSANOW,&new_tio);
}

void rebuffer_stdin()
{
	/* restore the former settings */
	tcsetattr(STDIN_FILENO,TCSANOW,tio);
}

void unblock_stdin()
{
	fcntl(0, F_SETFL, O_NONBLOCK);
}
void block_stdin()
{
	fcntl(0, F_SETFL, O_ASYNC);
}

void quit(int q)
{
	snd_seq_event_t ev;
	snd_seq_ev_set_queue_stop(&ev, queue);
	ALL_QUIET;
	snd_seq_close(seq);
	//put terminal back to normal:
	rebuffer_stdin();
	block_stdin();
	exit(q);
}

/* prints an error message to stderr */
static void errormsg(const char *msg, ...)
{
	va_list ap;

	va_start(ap, msg);
	vfprintf(stderr, msg, ap);
	va_end(ap);
	fputc('\n', stderr);
}

/* prints an error message to stderr, and dies */
static void fatal(const char *msg, ...)
{
	va_list ap;

	va_start(ap, msg);
	vfprintf(stderr, msg, ap);
	va_end(ap);
	fputc('\n', stderr);
	quit(EXIT_FAILURE);
	//exit(EXIT_FAILURE);
}

/* memory allocation error handling */
static void check_mem(void *p)
{
	if (!p)
		fatal("Out of memory");
}

/* error handling for ALSA functions */
static void check_snd(const char *operation, int err)
{
	if (err < 0)
		fatal("Cannot %s - %s", operation, snd_strerror(err));
}

static void init_seq(void)
{
	int err;

	/* open sequencer */
	err = snd_seq_open(&seq, "default", SND_SEQ_OPEN_DUPLEX, 0);
	check_snd("open sequencer", err);

	/* set our name (otherwise it's "Client-xxx") */
	err = snd_seq_set_client_name(seq, "aplaymidi");
	check_snd("set client name", err);

	/* find out who we actually are */
	client = snd_seq_client_id(seq);
	check_snd("get client id", client);
}

/* parses one or more port addresses from the string */
static void parse_ports(const char *arg)
{
	char *buf, *s, *port_name;
	int err;

	/* make a copy of the string because we're going to modify it */
	buf = strdup(arg);
	check_mem(buf);

	for (port_name = s = buf; s; port_name = s + 1) {
		/* Assume that ports are separated by commas.  We don't use
		 * spaces because those are valid in client names. */
		s = strchr(port_name, ',');
		if (s)
			*s = '\0';

		++port_count;
		ports = realloc(ports, port_count * sizeof(snd_seq_addr_t));
		check_mem(ports);

		err = snd_seq_parse_address(seq, &ports[port_count - 1], port_name);
		if (err < 0)
			fatal("Invalid port %s - %s", port_name, snd_strerror(err));
	}

	free(buf);
}

static void create_source_port(void)
{
	snd_seq_port_info_t *pinfo;
	int err;

	snd_seq_port_info_alloca(&pinfo);

	/* the first created port is 0 anyway, but let's make sure ... */
	snd_seq_port_info_set_port(pinfo, 0);
	snd_seq_port_info_set_port_specified(pinfo, 1);

	snd_seq_port_info_set_name(pinfo, "aplaymidi");

	snd_seq_port_info_set_capability(pinfo, 0); /* sic */
	snd_seq_port_info_set_type(pinfo,
				   SND_SEQ_PORT_TYPE_MIDI_GENERIC |
				   SND_SEQ_PORT_TYPE_APPLICATION);

	err = snd_seq_create_port(seq, pinfo);
	check_snd("create port", err);
}

static void create_queue(void)
{
	queue = snd_seq_alloc_named_queue(seq, "aplaymidi");
	check_snd("create queue", queue);
	/* the queue is now locked, which is just fine */
}

static void connect_ports(void)
{
	int i, err;

	/*
	 * We send MIDI events with explicit destination addresses, so we don't
	 * need any connections to the playback ports.  But we connect to those
	 * anyway to force any underlying RawMIDI ports to remain open while
	 * we're playing - otherwise, ALSA would reset the port after every
	 * event.
	 */
	for (i = 0; i < port_count; ++i) {
		err = snd_seq_connect_to(seq, 0, ports[i].client, ports[i].port);
		if (err < 0)
			fatal("Cannot connect to port %d:%d - %s",
			      ports[i].client, ports[i].port, snd_strerror(err));
	}
}

static int read_byte(void)
{
	++file_offset;
	return getc(file);
}

/* reads a little-endian 32-bit integer */
static int read_32_le(void)
{
	int value;
	value = read_byte();
	value |= read_byte() << 8;
	value |= read_byte() << 16;
	value |= read_byte() << 24;
	return !feof(file) ? value : -1;
}

/* reads a 4-character identifier */
static int read_id(void)
{
	return read_32_le();
}

#define MAKE_ID(c1, c2, c3, c4) ((c1) | ((c2) << 8) | ((c3) << 16) | ((c4) << 24))

/* reads a fixed-size big-endian number */
static int read_int(int bytes)
{
	int c, value = 0;

	do {
		c = read_byte();
		if (c == EOF)
			return -1;
		value = (value << 8) | c;
	} while (--bytes);
	return value;
}

/* reads a variable-length number */
static int read_var(void)
{
	int value, c;

	c = read_byte();
	value = c & 0x7f;
	if (c & 0x80) {
		c = read_byte();
		value = (value << 7) | (c & 0x7f);
		if (c & 0x80) {
			c = read_byte();
			value = (value << 7) | (c & 0x7f);
			if (c & 0x80) {
				c = read_byte();
				value = (value << 7) | c;
				if (c & 0x80)
					return -1;
			}
		}
	}
	return !feof(file) ? value : -1;
}

/* allocates a new event */
static struct event *new_event(struct track *track, int sysex_length)
{
	struct event *event;

	event = malloc(sizeof(struct event) + sysex_length);
	check_mem(event);

	event->next = NULL;
	//event->prev = track->current_event;

	/* append at the end of the track's linked list */
	if (track->current_event)
		track->current_event->next = event;
	else
		track->first_event = event;
	track->current_event = event;

	return event;
}

static void skip(int bytes)
{
	while (bytes > 0)
		read_byte(), --bytes;
}

/* reads one complete track from the file */
static int read_track(struct track *track, int track_end)
{
	int tick = 0;
	unsigned char last_cmd = 0;
	unsigned char port = 0;

	/* the current file position is after the track ID and length */
	while (file_offset < track_end) {
		unsigned char cmd;
		struct event *event;
		struct event *tempo_event;
		int delta_ticks, len, c;

		delta_ticks = read_var();
		if (delta_ticks < 0)
			break;
		tick += delta_ticks;

		c = read_byte();
		if (c < 0)
			break;

		if (c & 0x80) {
			/* have command */
			cmd = c;
			if (cmd < 0xf0)
				last_cmd = cmd;
		} else {
			/* running status */
			ungetc(c, file);
			file_offset--;
			cmd = last_cmd;
			if (!cmd)
				goto _error;
		}

		switch (cmd >> 4) {
			/* maps SMF events to ALSA sequencer events */
			static const unsigned char cmd_type[] = {
				[0x8] = SND_SEQ_EVENT_NOTEOFF,
				[0x9] = SND_SEQ_EVENT_NOTEON,
				[0xa] = SND_SEQ_EVENT_KEYPRESS,
				[0xb] = SND_SEQ_EVENT_CONTROLLER,
				[0xc] = SND_SEQ_EVENT_PGMCHANGE,
				[0xd] = SND_SEQ_EVENT_CHANPRESS,
				[0xe] = SND_SEQ_EVENT_PITCHBEND
			};

		case 0x8: /* channel msg with 2 parameter bytes */
		case 0x9:
		case 0xa:
		case 0xb:
		case 0xe:
			event = new_event(track, 0);
			event->type = cmd_type[cmd >> 4];
			event->port = port;
			event->tick = tick;
			event->data.d[0] = ((redirect_channel >= 0) ? redirect_channel : cmd) & 0x0f;
			event->data.d[1] = read_byte() & 0x7f;
			event->data.d[2] = read_byte() & 0x7f;
			break;

		case 0xc: /* channel msg with 1 parameter byte */
		case 0xd:
			event = new_event(track, 0);
			event->type = cmd_type[cmd >> 4];
			event->port = port;
			event->tick = tick;
			event->data.d[0] = ((redirect_channel >= 0) ? redirect_channel : cmd) & 0x0f;
			event->data.d[1] = read_byte() & 0x7f;
			break;

		case 0xf:
			switch (cmd) {
			case 0xf0: /* sysex */
			case 0xf7: /* continued sysex, or escaped commands */
				len = read_var();
				if (len < 0)
					goto _error;
				if (cmd == 0xf0)
					++len;
				event = new_event(track, len);
				event->type = SND_SEQ_EVENT_SYSEX;
				event->port = port;
				event->tick = tick;
				event->data.length = len;
				if (cmd == 0xf0) {
					event->sysex[0] = 0xf0;
					c = 1;
				} else {
					c = 0;
				}
				for (; c < len; ++c)
					event->sysex[c] = read_byte();
				break;

			case 0xff: /* meta event */
				c = read_byte();
				len = read_var();
				if (len < 0)
					goto _error;

				switch (c) {
				case 0x21: /* port number */
					if (len < 1)
						goto _error;
					port = read_byte() % port_count;
					skip(len - 1);
					break;

				case 0x2f: /* end of track */
					track->end_tick = tick;
					track->last_event = track->current_event;
					skip(track_end - file_offset);
					return 1;

				case 0x51: /* tempo */
					if (len < 3)
						goto _error;
					if (smpte_timing) {
						/* SMPTE timing doesn't change */
						skip(len);
					} else {
						event = new_event(track, 0);
						event->type = SND_SEQ_EVENT_TEMPO;
						event->port = port;
						event->tick = tick;
						event->data.tempo = read_byte() << 16;
						event->data.tempo |= read_byte() << 8;
						event->data.tempo |= read_byte();

						tempo_event = new_event(tempo_track, 0);
						tempo_event->type = SND_SEQ_EVENT_TEMPO;
						tempo_event->port = port;
						tempo_event->tick = tick;
						tempo_event->data.tempo = event->data.tempo;

						skip(len - 3);
					}
					break;

				default: /* ignore all other meta events */
					skip(len);
					break;
				}
				break;

			default: /* invalid Fx command */
				goto _error;
			}
			break;

		default: /* cannot happen */
			goto _error;
		}
	}
_error:
	errormsg("%s: invalid MIDI data (offset %#x)", file_name, file_offset);
	return 0;
}

/* reads an entire MIDI file */
static int read_smf(void)
{
	int header_len, type, time_division, i, err;
	snd_seq_queue_tempo_t *queue_tempo;

	/* the curren position is immediately after the "MThd" id */
	header_len = read_int(4);
	if (header_len < 6) {
invalid_format:
		errormsg("%s: invalid file format", file_name);
		return 0;
	}

	type = read_int(2);
	if (type != 0 && type != 1) {
		errormsg("%s: type %d format is not supported", file_name, type);
		return 0;
	}

	num_tracks = read_int(2);
	if (num_tracks < 1 || num_tracks > 1000) {
		errormsg("%s: invalid number of tracks (%d)", file_name, num_tracks);
		num_tracks = 0;
		return 0;
	}
	tempo_track = malloc( sizeof(struct track) );
	tracks = calloc(num_tracks, sizeof(struct track));
	if (!tracks || !tempo_track) {
		errormsg("out of memory");
		num_tracks = 0;
		return 0;
	}
	memset(tempo_track, 0, sizeof(struct track));
	memset(tracks, 0, (num_tracks * sizeof(struct track)));

	time_division = read_int(2);
	if (time_division < 0)
		goto invalid_format;

	/* interpret and set tempo */
	snd_seq_queue_tempo_alloca(&queue_tempo);
	smpte_timing = !!(time_division & 0x8000);
	if (!smpte_timing) {
		/* time_division is ticks per quarter */
		original_tempo = 500000;
		snd_seq_queue_tempo_set_tempo(queue_tempo, 500000); /* default: 120 bpm */
		snd_seq_queue_tempo_set_ppq(queue_tempo, time_division);
	} else {
		/* upper byte is negative frames per second */
		i = 0x80 - ((time_division >> 8) & 0x7f);
		/* lower byte is ticks per frame */
		time_division &= 0xff;
		/* now pretend that we have quarter-note based timing */
		switch (i) {
		case 24:
			original_tempo = 500000;
			snd_seq_queue_tempo_set_tempo(queue_tempo, 500000);
			snd_seq_queue_tempo_set_ppq(queue_tempo, 12 * time_division);
			break;
		case 25:
			original_tempo = 400000;
			snd_seq_queue_tempo_set_tempo(queue_tempo, 400000);
			snd_seq_queue_tempo_set_ppq(queue_tempo, 10 * time_division);
			break;
		case 29: /* 30 drop-frame */
			original_tempo = 100000000;
			snd_seq_queue_tempo_set_tempo(queue_tempo, 100000000);
			snd_seq_queue_tempo_set_ppq(queue_tempo, 2997 * time_division);
			break;
		case 30:
			original_tempo = 500000;
			snd_seq_queue_tempo_set_tempo(queue_tempo, 500000);
			snd_seq_queue_tempo_set_ppq(queue_tempo, 15 * time_division);
			break;
		default:
			errormsg("%s: invalid number of SMPTE frames per second (%d)",
				 file_name, i);
			return 0;
		}
	}
	err = snd_seq_set_queue_tempo(seq, queue, queue_tempo);
	if (err < 0) {
		errormsg("Cannot set queue tempo (%u/%i)",
			 snd_seq_queue_tempo_get_tempo(queue_tempo),
			 snd_seq_queue_tempo_get_ppq(queue_tempo));
		return 0;
	}
	ppq = snd_seq_queue_tempo_get_ppq(queue_tempo);
	tempo = snd_seq_queue_tempo_get_tempo(queue_tempo);

	/* read tracks */
	for (i = 0; i < num_tracks; ++i) {
		int len;

		/* search for MTrk chunk */
		for (;;) {
			int id = read_id();
			len = read_int(4);
			if (feof(file)) {
				errormsg("%s: unexpected end of file", file_name);
				return 0;
			}
			if (len < 0 || len >= 0x10000000) {
				errormsg("%s: invalid chunk length %d", file_name, len);
				return 0;
			}
			if (id == MAKE_ID('M', 'T', 'r', 'k'))
				break;
			skip(len);
		}
		if (!read_track(&tracks[i], file_offset + len))
			return 0;
	}
	return 1;
}

static int read_riff(void)
{
	/* skip file length */
	read_byte();
	read_byte();
	read_byte();
	read_byte();

	/* check file type ("RMID" = RIFF MIDI) */
	if (read_id() != MAKE_ID('R', 'M', 'I', 'D')) {
invalid_format:
		errormsg("%s: invalid file format", file_name);
		return 0;
	}
	/* search for "data" chunk */
	for (;;) {
		int id = read_id();
		int len = read_32_le();
		if (feof(file)) {
data_not_found:
			errormsg("%s: data chunk not found", file_name);
			return 0;
		}
		if (id == MAKE_ID('d', 'a', 't', 'a'))
			break;
		if (len < 0)
			goto data_not_found;
		skip((len + 1) & ~1);
	}
	/* the "data" chunk must contain data in SMF format */
	if (read_id() != MAKE_ID('M', 'T', 'h', 'd'))
		goto invalid_format;
	return read_smf();
}

void free_event_list(struct event *event)
{
	//event = tracks[i].first_event;
	while (event) {
		struct event *next = event->next;
		free(event);
		event = next;
	}
}

static void cleanup_file_data(void)
{
	int i;

	for (i = 0; i < num_tracks; ++i)
		free_event_list(tracks[i].first_event);
	if (tempo_track != NULL)
	{
		free_event_list(tempo_track->first_event);
		free(tempo_track);
		tempo_track = NULL;
	}
	num_tracks = 0;
	if (tracks != NULL)
	{
		free(tracks);
		tracks = NULL;
	}
}

static void handle_big_sysex(snd_seq_event_t *ev)
{
	unsigned int length;
	ssize_t event_size;
	int err;

	length = ev->data.ext.len;
	if (length > MIDI_BYTES_PER_SEC)
		ev->data.ext.len = MIDI_BYTES_PER_SEC;
	event_size = snd_seq_event_length(ev);
	//if (event_size + 1 > snd_seq_get_output_buffer_size(seq)) {
	if ((unsigned int)event_size + 1 > snd_seq_get_output_buffer_size(seq)) {
		err = snd_seq_drain_output(seq);
		check_snd("drain output", err);
		err = snd_seq_set_output_buffer_size(seq, event_size + 1);
		check_snd("set output buffer size", err);
	}
	while (length > MIDI_BYTES_PER_SEC) {
		err = snd_seq_event_output(seq, ev);
		check_snd("output event", err);
		err = snd_seq_drain_output(seq);
		check_snd("drain output", err);
		err = snd_seq_sync_output_queue(seq);
		check_snd("sync output", err);
		if (sleep(1))
			fatal("aborted");
		ev->data.ext.ptr += MIDI_BYTES_PER_SEC;
		length -= MIDI_BYTES_PER_SEC;
	}
	ev->data.ext.len = length;
}

void set_tempo(snd_seq_event_t *ev, int new_tempo)
{
//Don't forget to snd_seq_event_output() or change will not be applied!
	snd_seq_ev_set_fixed(ev);
	ev->type = SND_SEQ_EVENT_TEMPO;
	ev->dest.client = SND_SEQ_CLIENT_SYSTEM;
	ev->dest.port = SND_SEQ_PORT_SYSTEM_TIMER;
	ev->data.queue.queue = queue;
	ev->data.queue.param.value = new_tempo;
	tempo = new_tempo;
	delta_time = tempo / ppq;
	//dprintf("\nTEMPO: %d\n", ev->data.queue.param.value);
	if ( verbosity >= 2 ) printf("original tempo: %dbpm | play tempo: %d%% => %dbpm\n",
		60000000/original_tempo, tempo_percentage, 60000000/tempo);
}

int set_tempo_direct(int new_tempo)
{
	snd_seq_event_t ev;
	snd_seq_ev_clear(&ev);
	snd_seq_ev_set_direct(&ev);
	ev.type = SND_SEQ_EVENT_TEMPO;
	ev.source.port = 0;
	ev.dest.client = SND_SEQ_CLIENT_SYSTEM;
	ev.dest.port = SND_SEQ_PORT_SYSTEM_TIMER;
	ev.data.queue.queue = queue;
	ev.data.queue.param.value = new_tempo;
	tempo = new_tempo;
	delta_time = tempo / ppq;
	//dprintf("TEMPO: %d\n", ev->data.queue.param.value);
	if ( verbosity >= 1 ) printf("original tempo: %dbpm | play tempo: %d%% => %dbpm\n",
		60000000/original_tempo, tempo_percentage, 60000000/tempo);
	return snd_seq_event_output_direct(seq, &ev);
}

int set_tempo_percentage(int percent)
{
	if ( percent > 0 )
	{
		tempo_percentage = percent;
		return 1;
	}
	else return 0;
}

int send_channel_mode(int param, int value)
{
	int i, j, err;
	//wait for all notes to be played:
	err = snd_seq_sync_output_queue(seq);
	check_snd("sync output", err);

	snd_seq_event_t ev;
	snd_seq_ev_clear(&ev);
	snd_seq_ev_set_direct(&ev);
	ev.source.port = 0;
	//!!! WHAT ABOUT OTHER PORTS?:
	ev.type = SND_SEQ_EVENT_CONTROLLER;
	//All notes off:
	ev.data.control.param = param;
	ev.data.control.value = value;
	for (j=0; j < port_count; j++)
	{
		ev.dest = ports[j];
		for (i=0; i < num_channels; i++)
		{
			ev.data.control.channel = i;
			//!!! NO ERROR CHECK :
			snd_seq_event_output_direct(seq, &ev);
		}
	}
	return 1;
}

long long time_of_tick(int tick)
{
	struct event *event = tempo_track->first_event;
	int previous_event_tick = 0;
	long long previous_tempo = original_tempo;
	long long total_time = 0;
	while ( 1 )
	{
		total_time += previous_tempo * (event->tick - previous_event_tick);
		if ( !(event->next) || ((int)(event->next->tick) >= tick) )
		{
			total_time += (long long)(event->data.tempo) * (tick - event->tick);
			break;
		}
		previous_event_tick = event->tick;
		event = event->next;
		previous_tempo = event->data.tempo;
	}
	total_time /= ppq;
	total_time /= (float)tempo_percentage * 0.01;
	return total_time; //(microseconds)
}

void make_event_from(struct event *event, snd_seq_event_t *ev)
{
	ev->type = event->type;
	ev->time.tick = event->tick;
	//dprintf("%d ",ev->time.tick);
	if (verbosity >= 4)
		printf("tick: %d\t/ %d| ", ev->time.tick, max_tick);
	else if (verbosity == 3 || verbosity == 2 || verbosity == 1)
	{
		//puts("       ");
		if ( verbosity == 2 || verbosity == 1 ) //save cursor position
			printf("\0337");

		int total_seconds = (time_passed/1000000);
		int minutes = total_seconds / 60;
		int seconds = total_seconds % 60;
		if ( minutes < 10 ) 	printf("0%d:", minutes);
		else 					printf("%d:", minutes);
		if ( seconds < 10 ) 	printf("0%d / ", seconds);
		else 					printf("%d / ", seconds);
		if ( max_time_m < 10 ) 	printf("0%d:", max_time_m);
		else					printf("%d:", max_time_m);
		if ( max_time_s < 10 ) 	printf("0%d", max_time_s);
		else 					printf("%d", max_time_s );

		if ( verbosity == 2 || verbosity == 1 ) //restore cursor position
			printf("\0338");
		else printf(" | ");
	}
	ev->dest = ports[event->port];
	switch (ev->type)
	{
		case SND_SEQ_EVENT_NOTEON:
		case SND_SEQ_EVENT_NOTEOFF:
		case SND_SEQ_EVENT_KEYPRESS:
			snd_seq_ev_set_fixed(ev);
			ev->data.note.channel = event->data.d[0];
			ev->data.note.note = event->data.d[1];
			ev->data.note.velocity = event->data.d[2];
			if ( verbosity >= 3 ) printf("ch:%d\t| note:%d\t| vel:%d\n",
				ev->data.note.channel,
				ev->data.note.note, ev->data.note.velocity);
			break;
		case SND_SEQ_EVENT_CONTROLLER:
			snd_seq_ev_set_fixed(ev);
			ev->data.control.channel = event->data.d[0];
			ev->data.control.param = event->data.d[1];
			ev->data.control.value = event->data.d[2];
			if ( verbosity >= 3 ) printf("ch:%d\t| ctrl:%d\t| val:%d\n",
				ev->data.control.channel,
				ev->data.control.param, ev->data.control.value);
			break;
		case SND_SEQ_EVENT_PGMCHANGE:
		case SND_SEQ_EVENT_CHANPRESS:
			if (no_pgmchange) 
			{
				ev->type = SND_SEQ_EVENT_NONE;
				break;
			}
			snd_seq_ev_set_fixed(ev);
			ev->data.control.channel = event->data.d[0];
			ev->data.control.value = event->data.d[1];
			if ( verbosity >= 3 ) printf("ch:%d\t| prog change:%d\n",
				ev->data.control.channel, ev->data.control.value);
			break;
		case SND_SEQ_EVENT_PITCHBEND:
			snd_seq_ev_set_fixed(ev);
			ev->data.control.channel = event->data.d[0];
			ev->data.control.value =
				((event->data.d[1]) |
				 ((event->data.d[2]) << 7)) - 0x2000;
			if ( verbosity >= 3 ) printf("ch:%d\t| pitchbend:%d\n",
				ev->data.control.channel, ev->data.control.value);
			break;
		case SND_SEQ_EVENT_SYSEX:
			snd_seq_ev_set_variable(ev, event->data.length,
						event->sysex);
			handle_big_sysex(ev);
			if ( verbosity >= 3 ) printf("sysex\n");
			break;
		case SND_SEQ_EVENT_TEMPO:
			original_tempo = event->data.tempo;
			tempo = original_tempo * (100.0/(float)tempo_percentage);
			set_tempo(ev, tempo);
			break;
		default:
			fatal("Invalid event type %d!", ev->type);
	}
}

int midi_seek(int ticks, int actual_tick)
{
//returns new tick
	int i, err;
	unsigned int min_tick = 0; // max_tick+1;
	unsigned int previous_event_tick = 0;
	//int actual_tick = event->tick;
	struct track *track;
	snd_seq_event_t ev_direct;
	snd_seq_ev_clear(&ev_direct);
	snd_seq_ev_set_direct(&ev_direct);

	//We don't want to print all the events if verbosity is only at 2 or 3:
	int backup_verbosity = verbosity;
	if ( verbosity == 3 || verbosity == 2 )
		verbosity = 1;

	if (ticks > 0)
	{
		for (i=0; i < num_tracks; i++)
		{
			track = &tracks[i];
			if ( track->current_event )
			{
				while ( ((int)(track->current_event->tick) - (int)actual_tick) < ticks )
				{
					switch ( track->current_event->type )
					{
						case SND_SEQ_EVENT_NOTEON:
						case SND_SEQ_EVENT_NOTEOFF:
						case SND_SEQ_EVENT_KEYPRESS:
							break;
						default:
							make_event_from(track->current_event, &ev_direct);
							snd_seq_event_output_direct(seq, &ev_direct);
					}
					previous_event_tick = track->current_event->tick;
					//if ( track->current_event->next )
					//{
					track->current_event = track->current_event->next;
					//}
					if ( !(track->current_event) ) break;
				}
				if ( previous_event_tick > min_tick )
					min_tick = previous_event_tick;
			}
		}
	}
	else
	{
		//We replay all the file from the beginning because we want
		//to take into account events like tempo change or program change
		struct event *cursor;
		for (i=0; i < num_tracks; i++)
		{
			cursor = tracks[i].first_event;
			track = &tracks[i];
			if ( cursor ) //(don't really need this check)
			{
				while ( (int)actual_tick > (int)(cursor->tick) - ticks )
				{
					switch ( cursor->type )
					{
						case SND_SEQ_EVENT_NOTEON:
						case SND_SEQ_EVENT_NOTEOFF:
						case SND_SEQ_EVENT_KEYPRESS:
							break;
						default:
							make_event_from(cursor, &ev_direct);
							snd_seq_event_output_direct(seq, &ev_direct);
					}
					//old_cursor = cursor;
					previous_event_tick = cursor->tick;
					cursor = cursor->next;
					if ( !(cursor) )
					{
						//old_cursor = NULL;
						break;
					}
				}
				track->current_event = cursor;
				if ( previous_event_tick > min_tick )
					min_tick = previous_event_tick; //track->current_event->tick;
			}
		}
	}
	snd_seq_ev_set_queue_pos_tick(&ev_direct, queue, min_tick);
	err = snd_seq_event_output_direct(seq, &ev_direct);
	check_snd("output event", err);

	verbosity = backup_verbosity;
	return min_tick;
}


static void play_midi(void)
{
	snd_seq_event_t ev, ev_direct;
	//int backup_verbosity = verbosity;
	int i, err;
	int ch;
	int old_ev_tick = 0;
	struct track *track, *longest_track;
	struct event *event = NULL;
	delta_time = tempo / ppq;
	time_passed = 0; //(microseconds)


	/* calculate length of the entire file */
	time_from_start = 0;
	max_tick = -1;
	for (i = 0; i < num_tracks; ++i)
	{
		if (tracks[i].end_tick > max_tick)
		{
			max_tick = tracks[i].end_tick;
			longest_track = &tracks[i];
		}
	}
	max_time = time_of_tick(longest_track->end_tick);
	max_time_m = max_time / 60000000;
	max_time_s = (max_time / 1000000) % 60;

	/* initialize current position in each track */
	for (i = 0; i < num_tracks; ++i)
		tracks[i].current_event = tracks[i].first_event;

	//For events not treated as part of the midi file:
	snd_seq_ev_clear(&ev_direct);
	snd_seq_ev_set_direct(&ev_direct);


	/* common settings for all our events */
	snd_seq_ev_clear(&ev);
	ev.queue = queue;
	ev.source.port = 0;
	ev.flags = SND_SEQ_TIME_STAMP_TICK;

	err = snd_seq_start_queue(seq, queue, NULL);
	check_snd("start queue", err);
	/* The queue won't be started until the START_QUEUE event is
	 * actually drained to the kernel, which is exactly what we want. */

	err = snd_seq_drain_output(seq);
	check_snd("output event", err);

	old_ev_tick = midi_seek(start_seek, 0);
	time_passed = time_of_tick(old_ev_tick);

	for (;;)
	{
		struct track* event_track = NULL;
		int i;
		unsigned int min_tick = max_tick + 1;


		event = NULL;

		/* search next event */
		for (i = 0; i < num_tracks; ++i) {
			track = &tracks[i];
			struct event *e2 = track->current_event;
			if (e2 && e2->tick < min_tick) {
				min_tick = e2->tick;
				event = e2;
				event_track = track;
			}
		}
		if (!event)
			break; /* end of song reached */

		/* advance pointer to next event */
		event_track->current_event = event->next;

		/* output the event */
		int sleep_time = (event->tick - old_ev_tick) * delta_time;
		time_passed += sleep_time;
		//printf("%d\t/%d\n", time_passed, max_time);
		old_ev_tick = event->tick;
		usleep(sleep_time);

		make_event_from(event, &ev);

		//!!! THERE MUST BE A BETTER WAY:
		err = snd_seq_event_output(seq, &ev);
		check_snd("output event", err);
		err = snd_seq_drain_output(seq);
		check_snd("output event", err);

		#ifdef DEBUG
		  if ( sigio_count > 0 ) fprintf(stderr, "\n\nsigio_count = %d\n\n", sigio_count);
		#endif

		ch = getc(stdin);
		if ( ch != EOF )
		{
			switch (ch)
			{
				//PAUSE:
				case ' ':
					if ( verbosity >= 3 ) printf("\nPAUSE\n");
					if ( verbosity == 2 ) printf("\033[20CPAUSE");
					block_stdin();
					ALL_QUIET;
					getc(stdin);
					unblock_stdin();
					if ( verbosity == 2 ) printf("\033[1K\033[25D");
					snd_seq_event_t ev_direct;
					memset(&ev_direct, 0, sizeof(snd_seq_event_t));
					snd_seq_ev_set_queue_pos_tick(&ev_direct, queue, old_ev_tick);
					snd_seq_event_output_direct(seq, &ev_direct);
					break;
				//TEMPO +:
				case '>':
					//backup_verbosity = verbosity;
					//if ( verbosity == 1 ) verbosity = 2;
					if (!(set_tempo_percentage(tempo_percentage+1)))
						{ if ( verbosity >= 1 ) printf("can't set tempo percentage (too high)"); }
					else if ( original_tempo > 0 )
					{
						err = set_tempo_direct(original_tempo*(100.0/(float)tempo_percentage));
						check_snd("output event", err);
					}
					goto tempo_change;
				//TEMPO =:
				case '=':
					//backup_verbosity = verbosity;
					//if ( verbosity == 1 ) verbosity = 2;
					if (!(set_tempo_percentage(100)))
					{
						if ( verbosity >= 1 ) printf("can't set tempo percentage");
					}
					else if ( original_tempo > 0 )
					{
						err = set_tempo_direct(original_tempo*(100.0/(float)tempo_percentage));
						check_snd("output event", err);
					}
					goto tempo_change;
				//TEMPO -:
				case '<':
					//backup_verbosity = verbosity;
					//if ( verbosity == 1 ) verbosity = 2;
					if (!(set_tempo_percentage(tempo_percentage-1)))
						{ if ( verbosity >= 1 ) printf("can't set tempo percentage (too low)"); }
					else if ( original_tempo > 0 )
					{
						err = set_tempo_direct(original_tempo*(100.0/(float)tempo_percentage));
						check_snd("output event", err);
					}
				tempo_change:
					max_time = time_of_tick(longest_track->end_tick);
					max_time_m = max_time / 60000000;
					max_time_s = (max_time / 1000000) % 60;
					time_passed = time_of_tick(old_ev_tick);
					//verbosity = backup_verbosity;
					break;
				//FORWARD ~1000 ticks
				case 'f':
					//if playback has just started:
					if ( !(event) ) break;
					//else:
					old_ev_tick = midi_seek(1000, event->tick);
					goto skip;
				//FORWARD ~10000 ticks
				case 'F':
					//if playback has just started:
					if ( !(event) ) break;
					//else:
					old_ev_tick = midi_seek(10000, event->tick);
					goto skip;
				//BEGINNING
				case 'B':
					if ( !(event) ) break;
					old_ev_tick = midi_seek(start_seek, 0);
					goto skip;
				//PREVIOUS FILE
				case 'P':
					file_index_increment = -1;
					goto change_file;
				//NEXT FILE
				case 'N':
					file_index_increment = 1;
				change_file:
					if ( !(event) ) break;
					old_ev_tick = midi_seek(max_tick, event->tick);
					goto skip;
				//REWIND ~1000 ticks:
				case 'r':
					old_ev_tick = midi_seek(-1000, event->tick);
					goto skip;
				//REWIND ~10000 ticks:
				case 'R':
					old_ev_tick = midi_seek(-10000, event->tick);
				skip:
					ALL_QUIET;
					time_passed = time_of_tick(old_ev_tick);
					break;
				//REPEAT: NONE
				case ')':
					repeat_type = REPEAT_NONE;
					if (verbosity >= 3) printf("repeat: none\n");
					break;
				//REPEAT: CURRENT
				case '!':
					repeat_type = REPEAT_CURRENT;
					if (verbosity >= 3) printf("repeat: current\n");
					break;
				//REPEAT: ALL
				case '*':
					repeat_type = REPEAT_ALL;
					if (verbosity >= 3) printf("repeat: all\n");
					break;
				//REPEAT: SHUFFLE
				case '#':
					repeat_type = REPEAT_SHUFFLE;
					if (verbosity >= 3) printf("repeat: shuffle\n");
					break;
				//HELP:
				case 'h':
					interactiveUsage();
					break;
				//QUIT:
				case 'q':
					quit(0);
					break;
				//VERBOSITY -:
				case 'v':
					if ( verbosity > 0 )
						verbosity--;
					if ( verbosity >= 0 ) printf("verbosity : %d\n", verbosity);
					break;
				//VERBOSITY +:
				case 'V':
					if (verbosity < VERBOSE_MAX)
						verbosity++;
					if ( verbosity >= 1 ) printf("verbosity : %d\n", verbosity);
					break;
				default:
					if ( verbosity >= 1 ) printf("unrecognized command: '%c'\n", ch);
					break;
			}
		}
	}


	/* schedule queue stop at end of song */
	snd_seq_ev_set_fixed(&ev);
	ev.type = SND_SEQ_EVENT_STOP;
	ev.time.tick = max_tick;
	ev.dest.client = SND_SEQ_CLIENT_SYSTEM;
	ev.dest.port = SND_SEQ_PORT_SYSTEM_TIMER;
	ev.data.queue.queue = queue;
	err = snd_seq_event_output(seq, &ev);
	check_snd("output event", err);

	/* make sure that the sequencer sees all our events */
	err = snd_seq_drain_output(seq);
	check_snd("drain output", err);

	/*
	 * There are three possibilities how to wait until all events have
	 * been played:
	 * 1) send an event back to us (like pmidi does), and wait for it;
	 * 2) wait for the EVENT_STOP notification for our queue which is sent
	 *    by the system timer port (this would require a subscription);
	 * 3) wait until the output pool is empty.
	 * The last is the simplest.
	 */
	err = snd_seq_sync_output_queue(seq);
	check_snd("sync output", err);

	/* give the last notes time to die away */
	if (end_delay > 0)
		sleep(end_delay);

}

static void play_file(void)
{
	int ok;

	if ( verbosity >= 2 )
	{
		printf("Playing ");
		if (file_count > 1)
		{
			printf("(%d/%d) ", (file_index + 1), file_count);
		}
		printf("\"%s\"\n", file_name);
	}
	if (!strcmp(file_name, "-"))
		file = stdin;
	else
		file = fopen(file_name, "rb");
	if (!file) {
		errormsg("Cannot open %s - %s", file_name, strerror(errno));
		return;
	}

	file_offset = 0;
	ok = 0;

	switch (read_id()) {
	case MAKE_ID('M', 'T', 'h', 'd'):
		ok = read_smf();
		break;
	case MAKE_ID('R', 'I', 'F', 'F'):
		ok = read_riff();
		break;
	default:
		errormsg("%s is not a Standard MIDI File", file_name);
		break;
	}

	if (file != stdin)
		fclose(file);

	if (ok)
		play_midi();

	cleanup_file_data();
	//If there are several files, only the first file will be
	//concerned by the --seek (-s)  parameter:
	start_seek = 0;
}

static void list_ports(void)
{
	snd_seq_client_info_t *cinfo;
	snd_seq_port_info_t *pinfo;

	snd_seq_client_info_alloca(&cinfo);
	snd_seq_port_info_alloca(&pinfo);

	puts(" Port    Client name                      Port name");

	snd_seq_client_info_set_client(cinfo, -1);
	while (snd_seq_query_next_client(seq, cinfo) >= 0) {
		int client = snd_seq_client_info_get_client(cinfo);

		snd_seq_port_info_set_client(pinfo, client);
		snd_seq_port_info_set_port(pinfo, -1);
		while (snd_seq_query_next_port(seq, pinfo) >= 0) {
			/* port must understand MIDI messages */
			if (!(snd_seq_port_info_get_type(pinfo)
			      & SND_SEQ_PORT_TYPE_MIDI_GENERIC))
				continue;
			/* we need both WRITE and SUBS_WRITE */
			if ((snd_seq_port_info_get_capability(pinfo)
			     & (SND_SEQ_PORT_CAP_WRITE | SND_SEQ_PORT_CAP_SUBS_WRITE))
			    != (SND_SEQ_PORT_CAP_WRITE | SND_SEQ_PORT_CAP_SUBS_WRITE))
				continue;
			printf("%3d:%-3d  %-32.32s %s\n",
			       snd_seq_port_info_get_client(pinfo),
			       snd_seq_port_info_get_port(pinfo),
			       snd_seq_client_info_get_name(cinfo),
			       snd_seq_port_info_get_name(pinfo));
		}
	}
}

static void interactiveUsage(void)
{
	printf(
		"Interactive Control\n"
		"<space>                     Pause / play\n"
		"> / = /<                    Increase / normal / decrease tempo\n"
		"f / F                       Forward by approximately 1000 / 10000 ticks\n"
		"r / R                       Rewind by approximately 1000 / 10000 ticks\n"
		"v / V                       Decrease / increase verbosity\n"
		"B / N / P                   Beginning of current file / next / previous file\n"
		") / ! / * / #               Change repeat mode: none / current / all / shuffle\n"
		"h                           This help\n"
		"q                           Quit\n");
}

static void usage(const char *argv0)
{
	printf("Usage: %s -p client:port[,...] [-d delay] midifile ...\n", argv0);
	printf(
		"-h, --help                  this help\n"
		"-V, --version               print current version\n"
		"-l, --list                  list all possible output ports\n"
		"-p, --port=client:port,...  set port(s) to play to\n"
		"-d, --delay=seconds         delay after song ends\n"
		"-i, --index=index           start file index\n"
		"-r, --repeat=type           repeat type: none, current, all, or shuffle\n"
		"-s, --seek=ticks            seek in ticks\n"
		"-T, --tempo=n               tempo in percent\n"
		"-v, --verbosity=n           verbosity from 0 to 10\n"
		"-c, --channel=n             redirect to channel n (from 0 to 15)\n"
		"-n, --no-pgmchange          disallow program changes\n"
		"\n");
	interactiveUsage();
}

static void version(void)
{
	puts("midit version " VERSION);
}

void sigio_handler(int signal_number)
{
	sigio_count++;
}

void set_signals()
{
	sigio_count = 0;
	memset(&sigio_sa, 0, sizeof(struct sigaction));
	sigio_sa.sa_handler = &sigio_handler;
	sigaction(SIGIO, &sigio_sa, NULL);
}

static int rand_under(int size)
{
	return rand() % size;
}

static unsigned int current_timestamp(void)
{
	struct timeval te;
	gettimeofday(&te, NULL);
	return te.tv_sec * 1000u + te.tv_usec / 1000u;
}

int main(int argc, char *argv[])
{
	static const char short_options[] = "hVlp:d:i:r:s:T:v:c:n";
	static const struct option long_options[] = {
		{"help",		0,	NULL,	'h'},
		{"version",		0,	NULL,	'V'},
		{"list",		0,	NULL,	'l'},
		{"port",		1,	NULL,	'p'},
		{"delay",		1,	NULL,	'd'},
		{"index",		1,	NULL,	'i'},
		{"repeat",		1,	NULL,	'r'},
		{"seek",		1,	NULL,	's'},
		{"tempo",		1,	NULL,	'T'},
		{"verbosity",		1,	NULL,	'v'},
		{"channel",		1,	NULL,	'c'},
		{"no-pgmchange",	0,	NULL,	'n'},
		{0,0,0,0}
	};
	int c;
	int do_list = 0;

	set_signals();
	init_seq();
	tempo_percentage = 100;
	num_channels = 16;
	verbosity = 2;
	start_seek = 0;

	srand(current_timestamp());

	while ((c = getopt_long(argc, argv, short_options,
				long_options, NULL)) != -1) {
		switch (c) {
		case 'h':
			usage(argv[0]);
			return 0;
		case 'V':
			version();
			return 0;
		case 'l':
			do_list = 1;
			break;
		case 'p':
			parse_ports(optarg);
			break;
		case 'd':
			end_delay = atoi(optarg);
			break;
		case 'i':
			file_index = atoi(optarg) - 1;
			break;
		case 'r':
			if (strcasecmp(optarg, "none") == 0) {
				repeat_type = REPEAT_NONE;
			} else if (strcasecmp(optarg, "current") == 0) {
				repeat_type = REPEAT_CURRENT;
			} else if (strcasecmp(optarg, "all") == 0) {
				repeat_type = REPEAT_ALL;
			} else if (strcasecmp(optarg, "shuffle") == 0) {
				repeat_type = REPEAT_SHUFFLE;
			} else {
				fprintf(stderr, "Invalid repeat type specified\n");
				return (1);
			}
			break;
		case 's':
			start_seek = atoi(optarg);
			break;
		case 'c':
			redirect_channel = atoi(optarg);
			break;
		case 'n':
			no_pgmchange = 1;
			break;
		case 'T':
			if ( set_tempo_percentage(atoi(optarg)) != 1 )
			{
				fprintf(stderr, "Tempo percentage must be >= 1\n");
				return 1;
			}
			break;
		case 'v':
			verbosity = atoi(optarg);
			if (verbosity < 0)
			{
				verbosity = 0;
			}
			else if (verbosity > VERBOSE_MAX)
			{
				verbosity = VERBOSE_MAX;
			}
			break;
		default:
			usage(argv[0]);
			return 1;
		}
	}

	if (do_list) {
		list_ports();
	} else {
		if (port_count < 1) {
			/* use env var for compatibility with pmidi */
			const char *ports_str = getenv("ALSA_OUTPUT_PORTS");
			if (ports_str)
				parse_ports(ports_str);
			if (port_count < 1) {
				errormsg("Please specify at least one port with --port.");
				return 1;
			}
		}
		if (optind >= argc) {
			errormsg("Please specify a file to play.");
			return 1;
		}

		//make terminal unbuffered and non-blocking:
		tio = malloc(sizeof(struct termios));
		unblock_stdin();
		unbuffer_stdin();

		create_source_port();
		create_queue();
		connect_ports();

		file_count = argc - optind;
		if (file_index < 0) {
			switch (repeat_type) {
			case REPEAT_NONE:
			case REPEAT_CURRENT:
			case REPEAT_ALL:
				file_index = 0;
				break;
			case REPEAT_SHUFFLE:
				file_index = rand_under(file_count);
				break;
			}
		}
		while (file_index < file_count) {
			file_name = argv[optind + file_index];
			play_file();

			switch (repeat_type) {
			case REPEAT_NONE:
				file_index += ((file_index_increment == 0) ? 1 : file_index_increment);
				if (file_index < 0)
					file_index = 0;
				break;
			case REPEAT_CURRENT:
				file_index += file_index_increment;
				if (file_index < 0)
					file_index = 0;
				break;
			case REPEAT_ALL:
				file_index += ((file_index_increment == 0) ? 1 : file_index_increment);
				if ((file_index < 0) || (file_index >= file_count))
					file_index = 0;
				break;
			case REPEAT_SHUFFLE:
				if (file_count > 1) {
					/* Choose a _different_ file at random. */
					long r = rand_under(file_count - 1);
					file_index = r < file_index ? r : r + 1;
				}
				break;
			}
			file_index_increment = 0;
		}

		quit(0);
	}
	snd_seq_close(seq);
	return 0;
}
