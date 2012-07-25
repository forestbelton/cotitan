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
#include "ui.h"

#include <ncurses.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

fifo_t *event_queue;
static void ui_handle_queue(void);

void *ui_task(void *nothing) {
  initscr();
  
  ui_printf("UI initialized\n");
  while(1) {
    ui_handle_queue();
    refresh();
  }
  
  endwin();
  return NULL;
}

void ui_printf(const char *msg, ...) {
  char     *buf;
  size_t    len;
  va_list   args;
  ui_msg_t *m;
  
  /* Compute buffer length. */
  va_start(args, msg);
  len = vsnprintf(NULL, 0, msg, args) + 1;
  va_end(args);
  
  /* Allocate data. */
  buf = malloc(len);
  if(buf == NULL)
    return;
  
  m = malloc(sizeof *m);
  if(m == NULL) {
    free(buf);
    return;
  }
  
  /* Print into string. */
  va_start(args, msg);
  vsprintf(buf, msg, args);
  va_end(args);
  
  /* Push message to UI queue. */
  m->type = UI_MSG_PRINT;
  m->data = buf;
  fifo_push(event_queue, m);
}

void ui_handle_queue(void) {
  ui_msg_t *m;
  
  /* Pop off all the messages we can. */
  while((m = fifo_pop(event_queue)) != NULL) {
    switch(m->type) {
      case UI_MSG_PRINT:
        printw(m->data);
        free(m->data);
      break;
    }
    
    free(m);
  }
}

