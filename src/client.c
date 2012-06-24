#include "net.h"
#include "netc.h"
#include "ui.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
  char *user, *pass;
  
  ui_init();

  /* Once ui_read_line works, we'll use this instead: */
#if 0
  user = ui_prompt(0, "Username: ");
  pass = ui_prompt(1, "Password: ");
#endif
  
  user = "apples";
  pass = "test";
  
  net_init();
  netc_connect("localhost", "48581");
  netc_auth(user, pass);
  netc_say("This is a test");

  return 0;
}

