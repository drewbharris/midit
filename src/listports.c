#include <unistd.h>
#include <alsa/asoundlib.h>

#include "midit.h"

int main() {
	// memory is allocated in getports
    midit_port_list_t* portlist = midit_getports();

    for (int i = 0; i < portlist->nports; i++) {
        midit_port_t *curport = &portlist->ports[i];
        printf("client: %d, port: %d, name: %s\n", curport->client, curport->port, curport->name);
    }

    // clear allocated memory
    midit_destroyports(portlist);
    return 0;
}
