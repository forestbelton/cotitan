export CC      := gcc
export CFLAGS  := -I$(shell pwd)/include -Wall -Wextra -Werror -Wno-unused-parameter -ansi -pedantic
export LDFLAGS := -lpthread -lncurses
export CFILES  := src/main.c src/ui.c src/net.c src/fifo.c
export OFILES  := $(CFILES:.c=.o)

.PHONY: clean test

cotitan: $(OFILES)
	$(CC) $(OFILES) $(LDFLAGS) -o $@

.c.o:
	$(CC) $(CFLAGS) $< -c -o $@

clean:
	rm -rf $(OFILES) cotitan
	cd test; $(MAKE) $(MAKEFLAGS) clean

test:
	cd test; $(MAKE) $(MAKEFLAGS)
