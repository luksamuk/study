CPP        = clang++
CPPFLAGS   = -Wall -g --std=c++17
LDFLAGS    = -lm
SRCS       = $(wildcard *.cpp)
BINARIES   = $(SRCS:%.cpp=%.out)

.PHONY: all clean
all: $(BINARIES)

%.out: %.cpp
	$(CPP) $(CPPFLAGS) $^ $(LDFLAGS) -o $@

clean:
	rm -f *.out *~

