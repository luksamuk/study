CXX         := clang++ --std=c++14
CXXFLAGS    := -Wall -pedantic -g -O2
EXTRA_FLAGS := 
OUTFLAG     := -o
BINARY      := huffman
SRC         := huffman.cpp


# Ferramentas externas
DOT         := /usr/bin/dot
XDOT        := /usr/bin/xdot
FEH         := /usr/bin/feh


# Teste pela existência do binário GraphViz
ifeq ($(shell test -s $(DOT) && echo -n ok),ok)
	EXTRA_FLAGS += -DUSE_GRAPHVIZ
endif

# Teste pela existência do binário xdot
ifeq ($(shell test -s $(XDOT) && echo -n ok),ok)
	EXTRA_FLAGS += -DUSE_XDOT
endif

# Teste pela existência do binário feh
ifeq ($(shell test -s $(FEH) && echo -n ok),ok)
	EXTRA_FLAGS += -DUSE_FEH
endif


all: $(BINARY)

$(BINARY): $(SRC)
	$(CXX) $(CXXFLAGS) $(EXTRA_FLAGS) $^ $(OUTFLAG) $@

clean:
	rm -f $(BINARY)
