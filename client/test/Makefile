OFILES      := $(filter-out %main.o, $(OFILES:%=../%))
TEST_CFILES := fifo-test.c
TEST_OFILES := $(TEST_CFILES:.c=.o)
EXEFILES    := $(TEST_CFILES:.c=)

.PHONY: all clean

all: $(EXEFILES)
clean:
	rm -rf $(EXEFILES) $(TEST_OFILES)

fifo-test: fifo-test.o
	$(CC) $(LDFLAGS) $(OFILES) $(TEST_OFILES) -o $@
.c.o:
	$(CC) $(CFLAGS) -c $< -o $@
