
###############################################################################
# MODULE     : Make file for axiom plugin
# BY         : Joris van der Hoeven
# COPYRIGHT  : This software falls under the GNU general public license;
#              see the file 'LICENSE', which is provided with this package.
###############################################################################

CC = gcc
RM = rm -f

all: bin/tm_axiom

bin/tm_axiom: src/tm_axiom.c
	$(CC) src/tm_axiom.c -o bin/tm_axiom

clean:
	$(RM) bin/tm_axiom
