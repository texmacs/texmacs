
###############################################################################
# MODULE     : Make file for graphviz plugin
# BY         : Joris van der Hoeven
# COPYRIGHT  : This software falls under the GNU general public license;
#              see the file 'LICENSE', which is provided with this package.
###############################################################################

CC = gcc
RM = rm -f

all: bin/tm_graphviz

bin/tm_graphviz: src/tm_graphviz.c
	$(CC) src/tm_graphviz.c -o bin/tm_graphviz

clean:
	$(RM) bin/tm_graphviz
