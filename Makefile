.PHONY: all clean

all: cotitan cotitand

cotitan:
	cd client; $(MAKE)
	cp client/cotitan .
cotitand:
	cd daemon; $(MAKE)
	echo "#!/bin/sh" > cotitand
	echo "cd daemon; erl -s cotitand start -detached" >> cotitand
	chmod +x cotitand
clean:
	cd client; $(MAKE) clean
	cd daemon; $(MAKE) clean	
	rm -rf cotitan cotitand
