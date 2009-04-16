
###############################################################################
# MODULE     : Make file for maxima plugin
# COPYRIGHT  : (C) 1999-2008  Joris van der Hoeven
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################

CC = gcc
RM = rm -f

all: bin/maxima_filter

bin/maxima_filter: src/maxima_filter.c
	$(CC) $(CPPFLAGS) $(CFLAGS) src/maxima_filter.c -o bin/maxima_filter

clean:
	$(RM) bin/maxima_filter
