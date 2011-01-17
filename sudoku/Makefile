CC=gcc
LD=gcc
CFLAGS=-O0 -ggdb3 -Wall -I$(HOME)/lib
LDFLAGS=-L$(HOME)/lib -lpicosat

sudoku: sudoku.o
	$(LD) $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm -f sudoku.o sudoku