
###############################################################################
# MODULE     : Make file for axiom plugin
# COPYRIGHT  : (C) 1999-2008  Joris van der Hoeven
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################

CC = gcc
RM = rm -f

all: bin/tm_axiom

bin/tm_axiom: src/tm_axiom.c
	$(CC) $(CPPFLAGS) $(CFLAGS) src/tm_axiom.c -o bin/tm_axiom

clean:
	$(RM) bin/tm_axiom
