
###############################################################################
# MODULE     : Make file for maxima plugin
# BY         : Joris van der Hoeven
# COPYRIGHT  : This software falls under the GNU general public license;
#              see the file 'LICENSE', which is provided with this package.
###############################################################################

CC = gcc
RM = rm -f

all: bin/maxima_filter

bin/maxima_filter: src/maxima_filter.c
	$(CC) src/maxima_filter.c -o bin/maxima_filter

clean:
	$(RM) bin/maxima_filter
