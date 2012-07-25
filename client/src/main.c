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
#include "cotitan.h"
#include "fifo.h"
#include "net.h"
#include "ui.h"

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  pthread_t ui, net;
  netinfo_t ninfo = {
    "localhost",
    "48581"
  };
  
  event_queue = fifo_new();
  if(event_queue == NULL) {
    fprintf(stderr, "error: failed to create event queue\n");
    exit(EXIT_FAILURE);
  }

  packet_queue = fifo_new();
  if(packet_queue == NULL) {
    fprintf(stderr, "error: failed to create packet queue\n");
    exit(EXIT_FAILURE);
  }
  
  pthread_create(&ui,  NULL, ui_task,  NULL);
  pthread_create(&net, NULL, net_task, &ninfo);
  
  pthread_join(net, NULL);
  pthread_join(ui,  NULL);

  pthread_exit(NULL);
}

