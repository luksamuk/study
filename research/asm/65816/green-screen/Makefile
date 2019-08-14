BIN = green.smc
OBJ = green.obj

.PHONY: all clean

all: $(BIN)

%.obj: %.asm
	wla-65816 -o $@ $^

%.smc: %.link %.obj
	wlalink -r $< $@

clean:
	rm -rf *.smc *.obj

