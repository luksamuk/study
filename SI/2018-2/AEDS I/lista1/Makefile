CC       = gcc
CFLAGS   = -Wall -g
LDFLAGS  = -lm
SRCS     = $(wildcard *.cpp)
BINARIES = $(SRCS:%.c=%.out)

.PHONY: all clean
all: $(BINARIES)

%.out: %.c
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

clean:
	rm -f *.out *~

