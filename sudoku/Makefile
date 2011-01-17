CC=gcc
LD=gcc
CFLAGS=-O -Wall -I$(HOME)/lib
LDFLAGS=-L$(HOME)/lib -lpicosat

sudoku: sudoku.o
	$(LD) $(CFLAGS) -o $@ $^ $(LDFLAGS)