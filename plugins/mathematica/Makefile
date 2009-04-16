
###############################################################################
# MODULE     : Make file for the mathematica plugin
# COPYRIGHT : (C) 2005 Joris van der Hoeven
# LICENCE   : This software falls under the GNU general public license;
#             see the file 'LICENSE', which is provided with this package.
###############################################################################

CC = gcc
RM = rm -f

all: bin/realpath

bin/realpath: src/realpath.c
	$(CC) $(CPPFLAGS) $(CFLAGS) src/realpath.c -o bin/realpath

clean:
	rm -f *~
	rm -f */*~
	rm -f */*/*~
	rm -f */*.o
	rm -f bin/realpath
	rm -f bin/tm_mathematica.bin
