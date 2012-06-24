#include "net.h"

int net_init(void) {
#ifdef _WIN32_
  WSADATA wsaData;
  return WSAStartup(MAKEWORD(1,1), &wsaData);
#else
  return 0;
#endif
}

int net_send(int sockfd, cot_packet_t *pkt) {
  send(sockfd, &pkt->len,     sizeof pkt->len,  0);
  send(sockfd, &pkt->type,    sizeof pkt->type, 0);
  send(sockfd, &pkt->data[0], pkt->len,         0);
  
  return 0;
}

