.PHONY: all cotitan cotitand clean

all: cotitan cotitand

cotitan:
	cd client; $(MAKE) $(MAKEFLAGS)
cotitand:
	cd daemon; $(MAKE) $(MAKEFLAGS)
clean:
	cd client; $(MAKE) $(MAKEFLAGS) clean
	cd daemon; $(MAKE) $(MAKEFLAGS) clean	
