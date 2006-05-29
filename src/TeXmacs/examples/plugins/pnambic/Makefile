all: EMPTY_DIRS bin/p_out bin/p_in bin/pnambic in out

EMPTY_DIRS:
	mkdir -p bin

bin/%: src/%.c
	gcc -o $@ $<

in:
	mkfifo in

out:
	mkfifo out

clean:
	rm -f in out bin/* src/*~ progs/*~ *~

.PHONY: all clean
