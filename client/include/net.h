#ifndef COTITAN_NET_H_
#define COTITAN_NET_H_

typedef struct {
  char *host;
  char *port;
} netinfo_t;

void *net_task(void *info);

#endif

