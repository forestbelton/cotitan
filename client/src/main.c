#include "net.h"
#include <pthread.h>

extern void *ui_task(void *);

int main(int argc, char *argv[]) {
  pthread_t ui, net;
  netinfo_t ninfo = {
    "localhost",
    "48581"
  };
  
  pthread_create(&ui,  NULL, ui_task,  NULL);
  pthread_create(&net, NULL, net_task, &ninfo);
  
  pthread_exit(NULL);
}

