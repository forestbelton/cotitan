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
#include "fifo.h"
#include <pthread.h>
#include <stdlib.h>

fifo_t *fifo_new(void) {
  fifo_t *out;
  
  out = malloc(sizeof *out);
  if(out == NULL)
    return NULL;
  
  if(pthread_mutex_init(&out->m, NULL)) {
    free(out);
    return NULL;
  }
  
  out->head = NULL;
  out->tail = NULL;
  
  return out;
}

int fifo_empty(fifo_t *f) {
  int empty;
  
  pthread_mutex_lock(&f->m);
  
  empty = f->head == NULL;
  
  pthread_mutex_unlock(&f->m);
   
  return empty;
}

void *fifo_pop(fifo_t *f) {
  void       *out;
  fifo_msg_t *tmp;
  
  pthread_mutex_lock(&f->m);
  
  /* Check for an empty list. */
  if(f->head == NULL) {
    pthread_mutex_unlock(&f->m);
    return NULL;
  }
  
  /* Retrieve message to pop. */
  tmp = f->head;
  out = tmp->value;
  
  /* Pop element and update tail if necessary. */
  f->head = tmp->next;
  free(tmp);
  if(f->head == NULL)
    f->tail = NULL;
  
  pthread_mutex_unlock(&f->m);
  
  return out;
}

int fifo_push(fifo_t *f, void *value) {
  fifo_msg_t *next = malloc(sizeof *next);
  
  /* Initialize message. */
  if(next == NULL)
    return 1;
  next->value = value;
  next->next  = NULL;
  
  pthread_mutex_lock(&f->m);
  
  /* Add message to FIFO. */
  if(f->head == NULL)
    f->head = next;
  else
    f->tail->next = next;
  
  /* Update tail. */
  f->tail = next;
  
  pthread_mutex_unlock(&f->m);
  
  return 0;
}

