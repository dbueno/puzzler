CC=gcc
LD=gcc
CFLAGS=-m32 -O2 -Wall -I$(HOME)/lib
LDFLAGS=-L$(HOME)/lib -lpicosat

sudoku: sudoku.o
	$(LD) $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm -f sudoku.o sudoku
