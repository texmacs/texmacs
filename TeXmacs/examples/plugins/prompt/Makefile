
###############################################################################
# MODULE     : Make file for plugin example
# BY         : Joris van der Hoeven
# COPYRIGHT  : This software falls under the GNU general public license;
#              see the file 'LICENSE', which is provided with this package.
###############################################################################

CXX = g++
RM = rm -f

FILES_CPP := $(wildcard src/*.cpp)
FILES_BIN := $(patsubst src/%.cpp,bin/%.bin,$(FILES_CPP))

all: $(FILES_BIN)

bin/%.bin : src/%.cpp
	$(CXX) $< -o $@

clean:
	$(RM) *~
	$(RM) */*~
	$(RM) bin/*
