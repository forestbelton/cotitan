CC=gcc
CFLAGS=-Iinclude -Wall -Wextra -Werror -ansi -pedantic -O2 -Wno-unused-parameter -Wno-unused-result
LDFLAGS=

CLIENT_SRC=src/net.c src/netc.c src/ui.c src/client.c
CLIENT_OBJ=$(CLIENT_SRC:.c=.o)

DAEMON_SRC=src/net.c src/daemon.c
DAEMON_OBJ=$(DAEMON_SRC:.c=.o)

cotitan: $(CLIENT_OBJ)
	$(CC) $(LDFLAGS) $(CLIENT_OBJ) -o $@

cotitand: $(DAEMON_OBJ)
	$(CC) $(LDFLAGS) $(DAEMON_OBJ) -o $@

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -rf $(CLIENT_OBJ) $(DAEMON_OBJ) cotitan cotitand
