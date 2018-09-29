#include <alsa/asoundlib.h>

typedef struct {
    int client;
    int port;
    char *name;
} midit_port_t;

typedef struct {
    int nports;
    midit_port_t *ports;
} midit_port_list_t;

void list_ports(void);

midit_port_list_t* midit_getports();
void midit_destroyports(midit_port_list_t *);