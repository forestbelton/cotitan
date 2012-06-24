#ifndef COTITAN_NET_H_
#define COTITAN_NET_H_

#include <stdint.h>

typedef struct {
  uint16_t  type;
  uint16_t  len;
  uint8_t  *data;
} cot_packet_t;

typedef enum {
  PKT_AUTH = 0x0000,
  PKT_SAY  = 0x0001 
} cot_pktid_t;

/* Connects to the cotitan daemon on the given host:port. */
int net_init(const char *host, const char *port);

/* Authenticates as a certain user. */
int net_auth(const char *user, const char *pass);

/* Sends a chat message. */
void net_say(char *msg);
void net_sayf(const char *msg, ...);

#endif

