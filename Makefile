.PHONY: cotitand clean

cotitand:
	cd daemon; $(MAKE) $(MAKEFLAGS)
clean:
	cd daemon; $(MAKE) $(MAKEFLAGS) clean	
