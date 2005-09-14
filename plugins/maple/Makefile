
###############################################################################
# MODULE     : Make file for maple plugin
# BY         : Joris van der Hoeven
# COPYRIGHT  : This software falls under the GNU general public license;
#              see the file 'LICENSE', which is provided with this package.
###############################################################################

CC = gcc
CXX = g++
RM = rm -f

all: bin/tm_maple_5

bin/tm_maple_5: src/tm_maple_5.cpp
	$(CXX) src/tm_maple_5.cpp -o bin/tm_maple_5

clean:
	$(RM) *~
	$(RM) */*~
	$(RM) bin/tm_maple_5
	$(RM) $(HOME)/.TeXmacs/system/tm_maple_9.sh
	$(RM) $(HOME)/.TeXmacs/system/tm_maple_9
