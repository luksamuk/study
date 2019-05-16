CXX       = g++
WARNFLAGS = -Wall -pedantic
OPTIFLAGS = -O2 -fPIC
CXXFLAGS  = $(WARNFLAGS) $(OPTIFLAGS)
OBJFLAGS  = -c
OUTFLAGS  = -o
SOURCES   = $(wildcard src/*.cpp)
#HEADERS   = $(wildcard *.hpp)
OBJECTS   = $(SOURCES:%.cpp=%.o)
BIN       = tp2-sort

.PHONY: clean delete

all: $(OBJECTS)
	$(CXX) $(CXXFLAGS) $(OBJECTS) $(OUTFLAGS) $(BIN)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) $(OBJFLAGS) $^ $(OUTFLAGS) $@

clean:
	rm -rf $(OBJECTS)

delete:
	rm -f $(BIN)

