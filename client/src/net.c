/* Copyright (c) 2012 Forest Belton (apples)
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
* THE SOFTWARE.
*/
#include "net.h"

#include <pthread.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#ifndef _WIN32_
#  define  __USE_POSIX
#  include <netdb.h>
#  include <sys/types.h>
#  include <netinet/in.h>
#  include <sys/socket.h>
#  include <unistd.h>
#else
#  include <winsock.h>
#endif

static int net_connect(const char *hostname, const char *port);

void *net_task(void *info) {
  int        sockfd;
  netinfo_t *ninfo = info;
  
  sockfd = net_connect(ninfo->host, ninfo->port);
  if(sockfd == -1)
    return NULL;
  
  printf("connected to %s:%s\n", ninfo->host, ninfo->port);
  while(1) {
    
  }
  
  return NULL;
}

int net_connect(const char *hostname, const char *port) {
  int             ret, sockfd;
  struct addrinfo hints, *info, *p;
  
  memset(&hints, 0, sizeof hints);
  hints.ai_family   = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  
  /* Resolve hostname. */
  ret = getaddrinfo(hostname, port, &hints, &info);
  if(ret != 0) {
    fprintf(stderr, "getaddrinfo: %s\n", 
        (ret == EAI_SYSTEM) ? strerror(errno): gai_strerror(ret));
    return -1;
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
      close(sockfd);
      perror("connect");
      continue;
    }
    
    /* We're done. Clean up and return. */
    freeaddrinfo(info);
    return sockfd;
  }
  
  fprintf(stderr, "no available hosts\n");
  freeaddrinfo(info);
  return -1;
}

