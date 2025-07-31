
###############################################################################
# MODULE     : Make file for GNU R plugin
# COPYRIGHT  : (C) 1999-2008  Joris van der Hoeven
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################

CC ?= gcc
RM ?= rm -f

CPPFLAGS += -I../../src/System
LDADD += -lutil

all: bin/tm_r

bin/% : src/%.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $< -o $@ $(LDFLAGS) $(LDADD)

clean:
	$(RM) bin/tm_r
