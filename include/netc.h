#ifndef COTITAN_NETC_H_
#define COTITAN_NETC_H_

int netc_connect(const char *hostname, const char *port);
int netc_auth(const char *user, const char *pass);
void netc_say(char *msg);
void netc_sayf(const char *msg, ...);

#endif

