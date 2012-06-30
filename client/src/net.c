#include "net.h"

#include <pthread.h>
#include <stdio.h>
#include <string.h>

#ifndef _WIN32_
#  define  __USE_POSIX
#  include <netdb.h>
#  include <sys/types.h>
#  include <netinet/in.h>
#  include <sys/socket.h>
#else
#  include <winsock.h>
#endif

static int net_connect(const char *hostname, const char *port);

static int sockfd;

void *net_task(void *info) {
  netinfo_t *ninfo = info;
  
  if(net_connect(ninfo->host, ninfo->port))
    return NULL;
  
  printf("connected to %s:%s\n", ninfo->host, ninfo->port);
  while(1) {
  }
  
  return NULL;
}

int net_connect(const char *hostname, const char *port) {
  int             ret;
  struct addrinfo hints, *info, *p;
  
  memset(&hints, 0, sizeof hints);
  hints.ai_family   = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  
  /* Resolve hostname. */
  ret = getaddrinfo(hostname, port, &hints, &info);
  if(ret != 0) {
    fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(ret));
    return 1;
  }
  
  /* Connect to the first available host. */
  for(p = info; p != NULL; p = p->ai_next) {
    /* Create the socket. */
    sockfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
    if(sockfd == -1) {
      perror("socket");
      continue;
    }
    
    /* Try to connect. */
    ret = connect(sockfd, p->ai_addr, p->ai_addrlen);
    if(ret == -1) {
      perror("connect");
      continue;
    }
    
    /* We're done. Clean up and return. */
    freeaddrinfo(info);
    return 0;
  }
  
  fprintf(stderr, "no available hosts\n");
  freeaddrinfo(info);
  return 1;
}

