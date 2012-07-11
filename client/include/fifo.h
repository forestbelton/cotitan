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
#ifndef COTITAN_FIFO_H_
#define COTITAN_FIFO_H_

#include <pthread.h>

typedef struct fifo_msg_t {
  void              *value;
  struct fifo_msg_t *next;
} fifo_msg_t;

typedef struct {
  pthread_mutex_t  m;
  fifo_msg_t      *head;
  fifo_msg_t      *tail;
} fifo_t;

fifo_t *fifo_new    (void);
void    fifo_destroy(fifo_t *f);
void   *fifo_pop    (fifo_t *f);
int     fifo_empty  (fifo_t *f);
int     fifo_push   (fifo_t *f, void *value);

#endif

