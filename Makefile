.PHONY: all clean

all: cotitan cotitand

cotitan:
	cd client; $(MAKE) $(MAKEFLAGS)
	cp client/cotitan .
cotitand:
	cd daemon; $(MAKE) $(MAKEFLAGS)
	echo -n "#!/bin/sh\ncd daemon; erl -s cotitand start -detached" > cotitand
	chmod +x cotitand
clean:
	cd client; $(MAKE) $(MAKEFLAGS) clean
	cd daemon; $(MAKE) $(MAKEFLAGS) clean	
	rm -rf cotitan cotitand
