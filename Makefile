.PHONY: all clean test

all: cotitan cotitand test

cotitan:
	cd client; $(MAKE)
	cp client/cotitan .
cotitand:
	cd daemon; $(MAKE)
	echo "#!/bin/sh" > cotitand
	echo "cd daemon; erl -s cotitand start -detached" >> cotitand
	chmod +x cotitand
test:
	cd client; $(MAKE) test

clean:
	cd client; $(MAKE) clean
	cd daemon; $(MAKE) clean	
	rm -rf cotitan cotitand
