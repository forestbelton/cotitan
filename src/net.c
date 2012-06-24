#include "net.h"

/* Network headers */
#ifndef _WIN32_
#  define __USE_POSIX
#  include <netdb.h>
#  include <sys/types.h>
#  include <sys/socket.h>
#endif

#ifndef NDEBUG
#  include <stdio.h>
#  define DEBUG(msg) printf("%s\n", msg)
#else
#  define DEBUG(msg) do { } while()
#endif

#include <stdarg.h>
#include <string.h>

#ifndef _WIN32_
  int sockfd;
#endif

int net_send(cot_packet_t *pkt);

int net_init(const char *hostname, const char *port) {
  struct addrinfo hints, *res;
  
  memset(&hints, 0, sizeof hints);
  hints.ai_family   = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  
  DEBUG("calling getaddrinfo()");
  getaddrinfo(hostname, port, &hints, &res);
  
  DEBUG("calling socket()");
  sockfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
  
  DEBUG("calling connect()");
  connect(sockfd, res->ai_addr, res->ai_addrlen);
  
  return 0;
}

int net_auth(const char *user, const char *pass) {
  return 0;
}

void net_say(char *msg) {
  cot_packet_t pkt;
  
  /* Prepare packet. */
  pkt.type = PKT_SAY;
  pkt.len  = strlen(msg);
  pkt.data = (uint8_t*)msg;
  
  net_send(&pkt);
}

void net_sayf(const char *msg, ...) {
}

int net_send(cot_packet_t *pkt) {
  send(sockfd, &pkt->len,     sizeof pkt->len,  0);
  send(sockfd, &pkt->type,    sizeof pkt->type, 0);
  send(sockfd, &pkt->data[0], pkt->len,         0);
  
  return 0;
}

