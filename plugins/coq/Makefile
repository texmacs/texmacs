
###############################################################################
# MODULE     : Make file for coq plugin
# COPYRIGHT  : (C) 2024  Joris van der Hoeven
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################

CC = gcc
RM = rm -f

all: bin/tm_coq

bin/tm_coq: src/tm_coq.c src/pico_xml.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $^ -o $@ $(LDFLAGS) $(LDADD)

clean:
	$(RM) bin/tm_coq
