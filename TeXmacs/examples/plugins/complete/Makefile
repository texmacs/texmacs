
###############################################################################
# MODULE     : Make file for plugin example
# COPYRIGHT  : (C) 1999-2008  Joris van der Hoeven
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################

CXX = g++
RM = rm -f
MKDIR_P = mkdir -p
RMDIR = rmdir --i

FILES_CPP := $(wildcard src/*.cpp)
FILES_BIN := $(patsubst src/%.cpp,bin/%.bin,$(FILES_CPP))

all: $(FILES_BIN)

bin/%.bin : src/%.cpp | bin
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $< -o $@

bin:
	$(MKDIR_P) $@

clean:
	$(RM) *~
	$(RM) */*~
	$(RM) bin/*
	test -d bin && $(RMDIR) bin || true
