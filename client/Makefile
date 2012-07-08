CC     =gcc
CFLAGS =-Iinclude -Wall -Wextra -Werror -Wno-unused-parameter -ansi -pedantic
LDFLAGS=-lpthread -lncurses

CFILES=src/main.c src/ui.c src/net.c src/fifo.c
OFILES=$(CFILES:.c=.o)

.PHONY: clean

cotitan: $(OFILES)
	$(CC) $(OFILES) $(LDFLAGS) -o $@

.c.o:
	$(CC) $(CFLAGS) $< -c -o $@

clean:
	rm -rf $(OFILES) cotitan
