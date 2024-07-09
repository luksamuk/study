.PHONY: clean purge

all: ;

%.psexe: src/%.s
	armips $<
	python3 ../bin2exe.py $(subst .psexe,,$@).bin $@

clean:
	rm *.bin

purge:
	rm *.psexe
