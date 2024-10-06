SRC := helloworld.asm
OUT := $(addsuffix .bin,$(basename $(SRC)))

.PHONY: all clean run

all: $(OUT)

%.bin: %.asm
	asl $< -CPU SH7600 -o "$(basename $<).bld"
	p2bin "$(basename $@).bld" $@

clean:
	rm -rf *.bin *.bld

run-%: %.bin
	yabause --binary=$<
