CXX       = g++ --std=c++03
CXXFLAGS  = -g -Wall -pedantic
OUTFLAGS  = -o
SOURCES   = $(wildcard *.cpp)
OBJECTS   = $(SOURCES:%.cpp=%.out)
BIN       = tp4

.PHONY: clean delete

all: $(OBJECTS)

%.out: %.cpp
	$(CXX) $(CXXFLAGS) $^ $(OUTFLAGS) $@

clean:
	rm -rf $(OBJECTS)

delete:
	rm -f $(BIN)

