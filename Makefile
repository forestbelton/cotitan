.PHONY: all cotitan cotitand clean

all: cotitan cotitand

cotitan:

cotitand:
	cd daemon; $(MAKE) $(MAKEFLAGS)
clean:
	cd daemon; $(MAKE) $(MAKEFLAGS) clean	
