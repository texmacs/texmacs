
###############################################################################
# MODULE     : Make file for shell plugin
# BY         : Joris van der Hoeven
# COPYRIGHT  : This software falls under the GNU general public license;
#              see the file 'LICENSE', which is provided with this package.
###############################################################################

CXX = g++
RM = rm -f

all: bin/tm_shell

bin/tm_shell: src/tm_shell.cpp
	$(CXX) src/tm_shell.cpp -o bin/tm_shell

clean:
	$(RM) bin/tm_shell
