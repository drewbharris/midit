#include <unistd.h>
#include <alsa/asoundlib.h>

#include "midit.h"

int main() {
	midit_openport("14:0");

	printf("playing\n");
	midit_playfile("test.mid");
	printf("thread running");

	usleep(1000 * 1000 * 10);
	printf("stopping\n");
	midit_stop();
	usleep(1000 * 1000 * 5);

	midit_playfile("test.mid");
	printf("thread running");

	usleep(1000 * 1000 * 10);
	printf("stopping\n");
	midit_stop();

	midit_closeport();
    return 0;
}
