SRCS    := $(wildcard *.c)
OBJS    := $(patsubst %.c,%.o,$(SRCS))
CC      := gcc
CFLAGS  := -Wall -g
OBJFLAG := -c
OUTFLAG := -o

.PHONY: all clean

all: helloworld

helloworld: $(OBJS)
	$(CC) $(CFLAGS) $(OUTFLAG) $@ $^

%.o: %.c
	$(CC) $(CFLAGS) $(OUTFLAG) $@ $(OBJFLAG) $^

clean:
	rm *.o

