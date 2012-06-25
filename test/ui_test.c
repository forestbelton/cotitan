
#include "ui.h"
#include<stdbool.h>

int main(){
  int k;
  k = 0;
  ui_init();
  while(1){
    ui_interact(500);
    k += 500;
    ui_printf(0,"%d MILLI PASSED...",k);
  }
  ui_destroy();
}
