
###############################################################################
# MODULE     : Make file for maple plugin
# BY         : Joris van der Hoeven
# COPYRIGHT  : This software falls under the GNU general public license;
#              see the file 'LICENSE', which is provided with this package.
###############################################################################

CXX = g++
RM = rm -f

all: bin/maple_in_filter bin/maple_filter

bin/maple_in_filter: src/maple_in_filter.cpp
	$(CXX) src/maple_in_filter.cpp -o bin/maple_in_filter

bin/maple_filter: src/maple_filter.cpp
	$(CXX) src/maple_filter.cpp -o bin/maple_filter

clean:
	$(RM) bin/maple_in_filter
	$(RM) bin/maple_filter
