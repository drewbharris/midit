/*
 * libmidit (formerly midit) (formerly aplaymidi as part of alsa-utils)
 *  - a library to play Standard MIDI Files to sequencer port(s)
 *
 * Copyright (c) 2004-2006 Clemens Ladisch <clemens@ladisch.de>
 * Copyright (c) 2009-2018 mwnx <mwnx@gmx.com>
 * Copyright (c) 2018-2018 drewbharris <drewbharris@gmail.com>
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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <getopt.h>
#include <unistd.h>
#include <sys/time.h>
#include <signal.h>
#include <alsa/asoundlib.h>
#include <termios.h>
#include <pthread.h>
#include "midit.h"

#define VERBOSE_MAX 4

static int send_channel_mode(int param, int value);

#define ALL_QUIET send_channel_mode(120, 0) /* all_sound_off */

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
    struct event *next;     /* linked list */
    //struct event *prev;

    unsigned char type;     /* SND_SEQ_EVENT_xxx */
    unsigned char port;     /* port index */
    unsigned int tick;
    union {
        unsigned char d[3]; /* channel and data bytes */
        int tempo;
        unsigned int length;    /* length of sysex data */
    } data;
    unsigned char sysex[0];
};

struct track {
    struct event *first_event;  /* list of all events in this track */

    struct event *current_event;    /* used while loading and playing */
    struct event *last_event;
    int end_tick;           /* length of this track */
};

sig_atomic_t sigio_count;
struct sigaction sigio_sa;

enum repeat_type {
    REPEAT_SHUFFLE,
    REPEAT_ALL,
    REPEAT_NONE,
    REPEAT_CURRENT
};

static snd_seq_t      *seq;
static int             client;
static int             port_count;
static snd_seq_addr_t *ports;
static int             queue;
static int             end_delay               = 2;
static const char     *file_name;
static int             file_index              = -1;
static int             file_index_increment    = 0;
static int             file_count              = 0;
static enum            repeat_type repeat_type = REPEAT_NONE;
static FILE           *file;
static int             file_offset; /* current offset in input file */
static int             num_tracks;
static struct track   *tracks                  = NULL;
static int             smpte_timing;
static int             ppq;
static int             original_tempo;
static int             tempo;
static int             tempo_percentage;
static int             delta_time;
static struct termios *tio;
static int             num_channels;
static int             max_tick;
static int             verbosity               = 4;
static int             start_seek;
static long long       time_from_start; /* in seconds */
static long long       max_time;        /* in seconds */
static int             max_time_s;      /* max time seconds */
static int             max_time_m;      /* max time minutes */
static long long       time_passed;
static struct track   *tempo_track             = NULL;
static int             redirect_channel        = -1;
static char            no_pgmchange            = 0;
static int             stopping                = 0;
static pthread_mutex_t mutex1                  = PTHREAD_MUTEX_INITIALIZER;

static void quit(int q)
{
    snd_seq_event_t ev;
    snd_seq_ev_set_queue_stop(&ev, queue);
    ALL_QUIET;
    snd_seq_close(seq);

    exit(q);
}

static void cleanup()
{
    snd_seq_event_t ev;
    snd_seq_ev_set_queue_stop(&ev, queue);
    ALL_QUIET;
    snd_seq_close(seq);
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
}

/* error handling for ALSA functions */
static void check_snd(const char *operation, int err)
{
    if (err < 0)
        fatal("Cannot %s - %s", operation, snd_strerror(err));
}

static int send_channel_mode(int param, int value)
{
    int i, j, err;
    //wait for all notes to be played:
    err = snd_seq_sync_output_queue(seq);
    check_snd("sync output", err);

    snd_seq_event_t ev;
    snd_seq_ev_clear(&ev);
    snd_seq_ev_set_direct(&ev);
    ev.source.port = 0;

    /* XXX WHAT ABOUT OTHER PORTS? */
    ev.type = SND_SEQ_EVENT_CONTROLLER;
    ev.data.control.param = param;
    ev.data.control.value = value;
    for (j=0; j < port_count; j++)
    {
        ev.dest = ports[j];
        for (i=0; i < num_channels; i++)
        {
            ev.data.control.channel = i;
            /* FIXME: NO ERROR CHECK. */
            snd_seq_event_output_direct(seq, &ev);
        }
    }
    return 1;
}

/* Prints an error message to stderr. */
static void errormsg(const char *msg, ...)
{
    va_list ap;

    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
    fputc('\n', stderr);
}

/* Memory allocation error handling. */
static void check_mem(void *p)
{
    if (!p)
        fatal("Out of memory");
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

/* Parses one or more port addresses from the string. */
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
        err = snd_seq_connect_to(seq, 0, ports[i].client,
                     ports[i].port);
        if (err < 0)
            fatal("Cannot connect to port %d:%d - %s",
                  ports[i].client, ports[i].port,
                  snd_strerror(err));
    }
}

static int read_byte(void)
{
    ++file_offset;
    return getc(file);
}

/* Reads a little-endian 32-bit integer. */
static int read_32_le(void)
{
    int value;
    value = read_byte();
    value |= read_byte() << 8;
    value |= read_byte() << 16;
    value |= read_byte() << 24;
    return !feof(file) ? value : -1;
}

/* Reads a 4-character identifier. */
static int read_id(void)
{
    return read_32_le();
}

#define MAKE_ID(c1, c2, c3, c4) ((c1) | ((c2) << 8) | ((c3) << 16) | ((c4) << 24))

/* Reads a fixed-size big-endian number. */
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

/* Reads a variable-length number. */
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

/* Allocates a new event. */
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

/* Reads one complete track from the file. */
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

/* Reads an entire MIDI file. */
static int read_smf(void)
{
    int header_len, type, time_division, i, err;
    snd_seq_queue_tempo_t *queue_tempo;

    /* the current position is immediately after the "MThd" id */
    header_len = read_int(4);
    if (header_len < 6) {
invalid_format:
        errormsg("%s: invalid file format", file_name);
        return 0;
    }

    type = read_int(2);
    if (type != 0 && type != 1) {
        errormsg("%s: type %d format is not supported", file_name,
             type);
        return 0;
    }

    num_tracks = read_int(2);
    if (num_tracks < 1 || num_tracks > 1000) {
        errormsg("%s: invalid number of tracks (%d)", file_name,
             num_tracks);
        num_tracks = 0;
        return 0;
    }
    tempo_track = malloc(sizeof(struct track));
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
        /* default: 120 bpm */
        snd_seq_queue_tempo_set_tempo(queue_tempo, 500000);
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
            snd_seq_queue_tempo_set_ppq(queue_tempo,
                            12 * time_division);
            break;
        case 25:
            original_tempo = 400000;
            snd_seq_queue_tempo_set_tempo(queue_tempo, 400000);
            snd_seq_queue_tempo_set_ppq(queue_tempo,
                            10 * time_division);
            break;
        case 29: /* 30 drop-frame */
            original_tempo = 100000000;
            snd_seq_queue_tempo_set_tempo(queue_tempo, 100000000);
            snd_seq_queue_tempo_set_ppq(queue_tempo,
                            2997 * time_division);
            break;
        case 30:
            original_tempo = 500000;
            snd_seq_queue_tempo_set_tempo(queue_tempo, 500000);
            snd_seq_queue_tempo_set_ppq(queue_tempo,
                            15 * time_division);
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
                errormsg("%s: unexpected end of file",
                     file_name);
                return 0;
            }
            if (len < 0 || len >= 0x10000000) {
                errormsg("%s: invalid chunk length %d",
                     file_name, len);
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
    if (tempo_track != NULL) {
        free_event_list(tempo_track->first_event);
        free(tempo_track);
        tempo_track = NULL;
    }
    num_tracks = 0;
    if (tracks != NULL) {
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
    snd_seq_ev_set_fixed(ev);
    ev->type = SND_SEQ_EVENT_TEMPO;
    ev->dest.client = SND_SEQ_CLIENT_SYSTEM;
    ev->dest.port = SND_SEQ_PORT_SYSTEM_TIMER;
    ev->data.queue.queue = queue;
    ev->data.queue.param.value = new_tempo;
    tempo = new_tempo;
    delta_time = tempo / ppq;
    if (verbosity >= 2)
        printf("original tempo: %dbpm | play tempo: %d%% => %dbpm\n",
               60000000 / original_tempo, tempo_percentage,
               60000000 / tempo);
}

int set_tempo_direct(int new_tempo)
{
    /* FIXME: refactor with 'set_tempo'. */

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
    if (verbosity >= 1)
        printf("original tempo: %dbpm | play tempo: %d%% => %dbpm\n",
               60000000 / original_tempo, tempo_percentage,
               60000000 / tempo);
    return snd_seq_event_output_direct(seq, &ev);
}

int set_tempo_percentage(int percent)
{
    if (percent > 0) {
        tempo_percentage = percent;
        return 1;
    } else {
        return 0;
    }
}

long long time_of_tick(int tick)
{
    struct event *event = tempo_track->first_event;
    int previous_event_tick = 0;
    long long previous_tempo = original_tempo;
    long long total_time = 0;
    while (1) {
        total_time += previous_tempo * (event->tick - previous_event_tick);
        if (!event->next || (int)event->next->tick >= tick) {
            total_time += (long long)event->data.tempo
                    * tick - event->tick;
            break;
        }
        previous_event_tick = event->tick;
        event = event->next;
        previous_tempo = event->data.tempo;
    }
    total_time /= ppq;
    total_time /= (float)tempo_percentage * 0.01;
    return total_time; /* microseconds */
}

void make_event_from(struct event *event, snd_seq_event_t *ev)
{
    ev->type = event->type;
    ev->time.tick = event->tick;
    if (verbosity >= 4) {
        printf("tick: %d\t/ %d| ", ev->time.tick, max_tick);
    } else if (verbosity == 3 || verbosity == 2 || verbosity == 1) {
        if (verbosity == 2 || verbosity == 1) {
            /* save cursor position */
            printf("\0337");
        }

        int total_seconds = time_passed/1000000;
        int minutes = total_seconds / 60;
        int seconds = total_seconds % 60;

        if (minutes < 10)
            printf("0%d:", minutes);
        else
            printf("%d:", minutes);

        if (seconds < 10)
            printf("0%d / ", seconds);
        else
            printf("%d / ", seconds);

        if (max_time_m < 10)
            printf("0%d:", max_time_m);
        else
            printf("%d:", max_time_m);

        if (max_time_s < 10)
            printf("0%d", max_time_s);
        else
            printf("%d", max_time_s );

        if (verbosity == 2 || verbosity == 1) {
            /* restore cursor position */
            printf("\0338");
        } else {
            printf(" | ");
        }
    }
    ev->dest = ports[event->port];
    switch (ev->type) {
    case SND_SEQ_EVENT_NOTEON:
    case SND_SEQ_EVENT_NOTEOFF:
    case SND_SEQ_EVENT_KEYPRESS:
        snd_seq_ev_set_fixed(ev);
        ev->data.note.channel = event->data.d[0];
        ev->data.note.note = event->data.d[1];
        ev->data.note.velocity = event->data.d[2];
        if (verbosity >= 3)
            printf("ch:%d\t| note:%d\t| vel:%d\n",
                   ev->data.note.channel,
                   ev->data.note.note, ev->data.note.velocity);
        break;
    case SND_SEQ_EVENT_CONTROLLER:
        snd_seq_ev_set_fixed(ev);
        ev->data.control.channel = event->data.d[0];
        ev->data.control.param = event->data.d[1];
        ev->data.control.value = event->data.d[2];
        if (verbosity >= 3)
            printf("ch:%d\t| ctrl:%d\t| val:%d\n",
                   ev->data.control.channel,
                   ev->data.control.param, ev->data.control.value);
        break;
    case SND_SEQ_EVENT_PGMCHANGE:
    case SND_SEQ_EVENT_CHANPRESS:
        if (no_pgmchange) {
            ev->type = SND_SEQ_EVENT_NONE;
            break;
        }
        snd_seq_ev_set_fixed(ev);
        ev->data.control.channel = event->data.d[0];
        ev->data.control.value = event->data.d[1];
        if (verbosity >= 3)
            printf("ch:%d\t| prog change:%d\n",
                   ev->data.control.channel,
                   ev->data.control.value);
        break;
    case SND_SEQ_EVENT_PITCHBEND:
        snd_seq_ev_set_fixed(ev);
        ev->data.control.channel = event->data.d[0];
        ev->data.control.value =
            ((event->data.d[1]) |
             ((event->data.d[2]) << 7)) - 0x2000;
        if (verbosity >= 3)
            printf("ch:%d\t| pitchbend:%d\n",
                   ev->data.control.channel,
                   ev->data.control.value);
        break;
    case SND_SEQ_EVENT_SYSEX:
        snd_seq_ev_set_variable(ev, event->data.length,
                    event->sysex);
        handle_big_sysex(ev);
        if (verbosity >= 3)
            printf("sysex\n");
        break;
    case SND_SEQ_EVENT_TEMPO:
        original_tempo = event->data.tempo;
        tempo = original_tempo * (100.0 / (float)tempo_percentage);
        set_tempo(ev, tempo);
        break;
    default:
        fatal("Invalid event type %d!", ev->type);
    }
}

/* Returns new tick. */
int midi_seek(int ticks, int actual_tick)
{
    int i, err;
    unsigned int min_tick = 0;
    unsigned int previous_event_tick = 0;
    struct track *track;
    snd_seq_event_t ev_direct;
    snd_seq_ev_clear(&ev_direct);
    snd_seq_ev_set_direct(&ev_direct);

    /* We don't want to print all the events if verbosity is only 2 or 3: */
    int backup_verbosity = verbosity;
    if (verbosity == 3 || verbosity == 2)
        verbosity = 1;

    if (ticks > 0) {
        for (i=0; i < num_tracks; i++) {
            track = &tracks[i];
            if (track->current_event) {
                while ((int)track->current_event->tick
                     - (int)actual_tick
                     < ticks) {
                    switch (track->current_event->type) {
                    case SND_SEQ_EVENT_NOTEON:
                    case SND_SEQ_EVENT_NOTEOFF:
                    case SND_SEQ_EVENT_KEYPRESS:
                        break;
                    default:
                        make_event_from(track->current_event, &ev_direct);
                        snd_seq_event_output_direct(seq, &ev_direct);
                    }
                    previous_event_tick = track->current_event->tick;
                    track->current_event = track->current_event->next;
                    if (!track->current_event)
                        break;
                }
                if (previous_event_tick > min_tick)
                    min_tick = previous_event_tick;
            }
        }
    } else {
        /* We replay the whole file from the beginning because we want
         * to take into account events like tempo change or program
         * change. FIXME: This is horrible. */
        struct event *cursor;
        for (i=0; i < num_tracks; i++) {
            cursor = tracks[i].first_event;
            track = &tracks[i];
            if (cursor) /* FIXME: don't really need this check. */ {
                while ((int)actual_tick
                     > (int)cursor->tick - ticks) {
                    switch ( cursor->type ) {
                    case SND_SEQ_EVENT_NOTEON:
                    case SND_SEQ_EVENT_NOTEOFF:
                    case SND_SEQ_EVENT_KEYPRESS:
                        break;
                    default:
                        make_event_from(cursor, &ev_direct);
                        snd_seq_event_output_direct(seq, &ev_direct);
                    }
                    previous_event_tick = cursor->tick;
                    cursor = cursor->next;
                    if ( !(cursor) ) {
                        break;
                    }
                }
                track->current_event = cursor;
                if (previous_event_tick > min_tick)
                    min_tick = previous_event_tick;
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
    int i, err;
    int ch;
    int old_ev_tick = 0;
    struct track *track, *longest_track = NULL;
    struct event *event = NULL;
    delta_time = tempo / ppq;
    time_passed = 0; /* microseconds */

    printf("tempo: %d, ppq: %d, delta_time: %d\n", tempo, ppq, delta_time);

    if (num_tracks == 0) {
        fatal("Number of tracks is 0");
    }

    /* calculate length of the entire file */
    time_from_start = 0;
    max_tick = -1;
    for (i = 0; i < num_tracks; ++i) {
        if (tracks[i].end_tick > max_tick) {
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

    for (;;) {
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
        printf("%d\t/%d\n", time_passed, max_time);
        old_ev_tick = event->tick;
        // printf("sleeping for %d\n", sleep_time);
        usleep(sleep_time);

        if (stopping) {
            printf("play_file: got stop\n");
            ALL_QUIET;
            snd_seq_event_t ev_direct;
            memset(&ev_direct, 0, sizeof(snd_seq_event_t));
            snd_seq_ev_set_queue_pos_tick(&ev_direct, queue, old_ev_tick);
            snd_seq_event_output_direct(seq, &ev_direct);
            pthread_mutex_lock(&mutex1);
            stopping = 0;
            pthread_mutex_unlock(&mutex1);
            break;
        }

        make_event_from(event, &ev);

        /* XXX THERE MUST BE A BETTER WAY: */
        err = snd_seq_event_output(seq, &ev);
        check_snd("output event", err);
        err = snd_seq_drain_output(seq);
        check_snd("output event", err);
    }

    printf("exited loop\n");
}

static void play_file(void)
{
    int ok;

    if ( verbosity >= 2 ) {
        printf("Playing ");
        if (file_count > 1) {
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

    /* If there are several files, only the first file will be
     * concerned by the --seek (-s)  parameter: */
    // start_seek = 0;

    cleanup_file_data();
}

// pthread function to call play_file
static void *pthread_playfile(void *pntr) {
    play_file();
    return NULL;
}

// returns an array of midit_port_list_t.
// memory is allocated and must be freed via midit_destroyports
midit_port_list_t* midit_getports(void)
{
    init_seq();

    snd_seq_client_info_t *cinfo;
    snd_seq_port_info_t *pinfo;

    snd_seq_client_info_alloca(&cinfo);
    snd_seq_port_info_alloca(&pinfo);
    snd_seq_client_info_set_client(cinfo, -1);

    int maxports = 100;
    int tmpbufsize = 100;
    int i = 0;
    char tmp_name[tmpbufsize];

    midit_port_list_t *results_list = (midit_port_list_t *)malloc(sizeof(midit_port_list_t));
    midit_port_t *results = (midit_port_t *)(malloc(sizeof(midit_port_t) * maxports));

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

            if (i >= maxports) {
                printf("reached max port limit\n");
                return results_list;
            }

            midit_port_t *curport = &results[i];
            curport->client = snd_seq_port_info_get_client(pinfo);
            curport->port = snd_seq_port_info_get_port(pinfo);
            snprintf(tmp_name, tmpbufsize, snd_seq_port_info_get_name(pinfo));
            curport->name = strdup(tmp_name);
            i++;
        }
    }

    cleanup();

    results_list->nports = i;
    results_list->ports = results;

    return results_list;
}

// free memory allocated for a port list
void midit_destroyports(midit_port_list_t *portlist)
{
    int i;
    for(i = 0; i < portlist->nports; i++) {
        midit_port_t *p = &portlist->ports[i];
        free(p->name);
    }
    free(portlist->ports);
    free(portlist);
}

// open port by string
void midit_openport(char *arg)
{
    init_seq();

    parse_ports(arg);
    create_source_port();
    create_queue();
    connect_ports();
}

// close the post and cleanup
void midit_closeport()
{
    cleanup();
}

// play a file. requires port to be open
void midit_playfile(char *file)
{
    file_name = file;

    tempo_percentage = 100;
    num_channels = 16;
    verbosity = 2;
    start_seek = 0;

    pthread_t thread;
    pthread_create(&thread, NULL, pthread_playfile, NULL);  
}

// stop the playback, cleanup file data but leave the port open
void midit_stop()
{
    pthread_mutex_lock(&mutex1);
    stopping = 1;
    pthread_mutex_unlock(&mutex1);
}

void sigio_handler(int signal_number)
{
    (void)(signal_number);
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