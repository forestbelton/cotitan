CC     =gcc
CFLAGS =-Wall -Wextra -Werror -Wno-unused-parameter -ansi -pedantic
LDFLAGS=-lpthread -lncurses

CFILES=src/main.c src/ui.c src/net.c
OFILES=$(CFILES:.c=.o)

.PHONY: clean

cotitan: $(OFILES)
	$(CC) $(OFILES) $(LDFLAGS) -o $@

.c.o:
	$(CC) $(CFLAGS) $< -c -o $@

clean:
	rm -rf $(OFILES) cotitan
