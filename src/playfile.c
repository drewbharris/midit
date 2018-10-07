#include <unistd.h>
#include <alsa/asoundlib.h>

#include "midit.h"

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("usage: %s client:port filename\n", argv[0]);
        return -1;
    }

    midit_openport(argv[1]);

    printf("playing %s on %s\n", argv[2], argv[1]);
    midit_playfile(argv[2]);

    while (1) {
        usleep(1000);
        // todo: check if still playing
    }

    midit_closeport();
    return 0;
}
