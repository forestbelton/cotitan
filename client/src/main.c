#include <pthread.h>

extern void *ui_task (void *);
extern void *net_task(void *);

int main(int argc, char *argv[]) {
  pthread_t ui, net;
  
  pthread_create(&ui,  NULL, ui_task,  NULL);
  pthread_create(&net, NULL, net_task, NULL);
  
  pthread_exit(NULL);
}

