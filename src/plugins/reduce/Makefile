
###############################################################################
# MODULE     : Make file for reduce plugin
# BY         : Joris van der Hoeven
# COPYRIGHT  : This software falls under the GNU general public license;
#              see the file 'LICENSE', which is provided with this package.
###############################################################################

CC = gcc
RM = rm -f

all: bin/reduce_filter

bin/reduce_filter: src/reduce_filter.c
	$(CC) src/reduce_filter.c -o bin/reduce_filter

clean:
	$(RM) bin/reduce_filter
