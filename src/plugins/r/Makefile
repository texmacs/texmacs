
###############################################################################
# MODULE     : Make file for GNU R plugin
# BY         : Joris van der Hoeven
# COPYRIGHT  : This software falls under the GNU general public license;
#              see the file 'LICENSE', which is provided with this package.
###############################################################################

CC = gcc
RM = rm -f

all: bin/tm_r

bin/tm_r: src/tm_r.c
	$(CC) -lutil src/tm_r.c -o bin/tm_r

clean:
	$(RM) bin/tm_r
