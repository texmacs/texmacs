
###############################################################################
# MODULE     : Make file for plugin example
# COPYRIGHT  : (C) 1999-2008  Joris van der Hoeven
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################

CC = gcc
RM = rm -f
MKDIR_P = mkdir -p
RMDIR = rmdir --i

FILES_C := $(wildcard src/*.c)
FILES_BIN := $(patsubst src/%.c,bin/%.bin,$(FILES_C))

all: $(FILES_BIN) in out

bin/%.bin : src/%.c | bin
	$(CC) $(CPPFLAGS) $(CFLAGS) $< -o $@

bin:
	$(MKDIR_P) $@

in:
	mkfifo in

out:
	mkfifo out

clean:
	$(RM) *~
	$(RM) */*~
	$(RM) bin/*
	$(RM) in out
	test -d bin && $(RMDIR) bin || true
