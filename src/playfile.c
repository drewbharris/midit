#include <unistd.h>
#include <alsa/asoundlib.h>

#include "midit.h"

int main() {
	midit_openport("14:0");

	printf("playing\n");
	midit_playfile("test.mid");

	usleep(1000 * 1000);
	printf("stopping\n");
	midit_stop();
	
	midit_closeport();
    return 0;
}
