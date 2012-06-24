#ifndef COTITAN_NET_H_
#define COTITAN_NET_H_

#include <stdint.h>

/* Network headers */
#ifndef _WIN32_
#  define __USE_POSIX
#  include <netdb.h>
#  include <sys/types.h>
#  include <sys/socket.h>
   typedef int sock_t;
#else
#  include <winsock.h>
#  define close(s) closesocket(s)
   typedef SOCKET sock_t;
#endif

typedef struct {
  uint16_t  type;
  uint16_t  len;
  uint8_t  *data;
} cot_packet_t;

typedef enum {
  PKT_AUTH = 0x0000,
  PKT_SAY  = 0x0001 
} cot_pktid_t;

int net_send(sock_t sockfd, cot_packet_t *pkt);
int net_init(void);

#endif

