#include "fifo.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  int       *data;
  size_t     i, len;
  fifo_t    *f;
  
  /* Don't care much about real randomness. */
  srand(time(NULL));
  len = rand() % 15;
  
  data = malloc(sizeof *data * len);
  assert(data != NULL);
  
  f = fifo_new();
  assert(f != NULL);
  
  for(i = 0; i < len; ++i) {
    data[i] = rand();
    assert(fifo_push(f, &data[i]) == 0);
  }
  
  for(i = 0; i < len; ++i) {
    int *tmp = fifo_pop(f);
    assert(tmp != NULL);
    assert(data[i] == *tmp);
  }
  
  assert(fifo_empty(f));
  free(data);
  fifo_destroy(f, NULL);
  
  return 0;
}

