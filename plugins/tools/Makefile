
###############################################################################
# MODULE     : Make file for cross-plugin tools
# BY         : Joris van der Hoeven
# COPYRIGHT  : This software falls under the GNU general public license;
#              see the file 'LICENSE', which is provided with this package.
###############################################################################

CC = gcc
RM = rm -f

all: bin/tm_which

bin/tm_which: src/tm_which.c
	$(CC) src/tm_which.c -o bin/tm_which

clean:
	$(RM) bin/tm_which
