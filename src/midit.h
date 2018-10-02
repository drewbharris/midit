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

midit_port_list_t* midit_getports();
void midit_destroyports(midit_port_list_t *);
void midit_openport(char *arg);
void midit_closeport();
void midit_playfile(char *file);
void midit_stop();