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
  
  /* We should really be looping here. */
  DEBUG("calling socket()");
  clientfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
  
  DEBUG("calling connect()");
  connect(clientfd, res->ai_addr, res->ai_addrlen);
  
  freeaddrinfo(res);
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

