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

