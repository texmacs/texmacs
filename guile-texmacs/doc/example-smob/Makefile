CFLAGS=`guile-config compile`
LIBS=`guile-config link`

O_FILES=image-type.o myguile.o

all: myguile

myguile: $(O_FILES)
	$(CC) $(O_FILES) $(LIBS) -o myguile

clean:
	-rm -rf myguile $(O_FILES)
