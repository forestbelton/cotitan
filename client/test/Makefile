OFILES      := $(filter-out %main.o, $(OFILES:%=../%))
TEST_CFILES := fifo-test.c
EXEFILES    := $(TEST_CFILES:.c=)

.PHONY: all clean

all: $(EXEFILES)
clean:
	rm -rf $(EXEFILES)

% : %.c
	$(CC) $(CFLAGS) $(OFILES) $(LDFLAGS) $< -o $@
