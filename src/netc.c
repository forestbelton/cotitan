#include "net.h"
#include "netc.h"

#ifndef NDEBUG
#  include <stdio.h>
#  define DEBUG(msg) printf("%s\n", msg)
#else
#  define DEBUG(msg) do { } while()
#endif

#include <stdarg.h>
#include <string.h>

sock_t clientfd;

int netc_connect(const char *hostname, const char *port) {
  struct addrinfo hints, *res;
  
  memset(&hints, 0, sizeof hints);
  hints.ai_family   = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  
  DEBUG("calling getaddrinfo()");
  getaddrinfo(hostname, port, &hints, &res);
  
  DEBUG("calling socket()");
  clientfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
  
  DEBUG("calling connect()");
  connect(clientfd, res->ai_addr, res->ai_addrlen);
  
  return 0;
}

int netc_auth(const char *user, const char *pass) {
  return 0;
}

void netc_say(char *msg) {
  cot_packet_t pkt;
  
  /* Prepare packet. */
  pkt.type = PKT_SAY;
  pkt.len  = strlen(msg);
  pkt.data = (uint8_t*)msg;
  
  net_send(clientfd, &pkt);
}

void netc_sayf(const char *msg, ...) {
}

