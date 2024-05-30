CC = gcc
CFLAGS = -g `guile-config compile`

all: strings

strings: strings.o testlib.o
	${CC} ${CFLAGS} ${LDFLAGS} -o strings strings.o testlib.o \
		`guile-config link`

strings.o: strings.c testlib.h
testlib.o: testlib.c testlib.h


clean:
	rm -f strings
	rm -f *.o
