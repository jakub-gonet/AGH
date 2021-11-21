CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash
RAPORT_F := raport.txt

.PHONY: all test clean

all: main

main: main.c
	$(CC) $(CFLAGS) main.c -o main -lpthread

test: main
	$(RM) $(RAPORT_F)
	echo -e 'Dla modulo, wyniki niezbyt się różnią. Dla trybu kolumnowego widać, że najlepsze wyniki osiągamy dla 16 wątków, potem różnice są nieznaczne.\nZ teoretycznego punktu widzenia, najlepiej jest stworzyć tyle wątków ile mamy rdzeni procesora (jeżeli zadanie jest typu CPU bound) lub więcej jeżeli musimy czekać na operacje wyjścia-wejścia.' >> $(RAPORT_F)
	echo 'MOD:' >> $(RAPORT_F)
	./main test/kabelki.ascii.pgm 1 mod >> $(RAPORT_F)
	./main test/kabelki.ascii.pgm 2 mod >> $(RAPORT_F)
	./main test/kabelki.ascii.pgm 4 mod >> $(RAPORT_F)
	./main test/kabelki.ascii.pgm 16 mod >> $(RAPORT_F)
	./main test/kabelki.ascii.pgm 64 mod >> $(RAPORT_F)
	./main test/kabelki.ascii.pgm 256 mod >> $(RAPORT_F)
	echo 'COL:' >> $(RAPORT_F)
	./main test/kabelki.ascii.pgm 1 col >> $(RAPORT_F)
	./main test/kabelki.ascii.pgm 2 col >> $(RAPORT_F)
	./main test/kabelki.ascii.pgm 4 col >> $(RAPORT_F)
	./main test/kabelki.ascii.pgm 16 col >> $(RAPORT_F)
	./main test/kabelki.ascii.pgm 64 col >> $(RAPORT_F)
	./main test/kabelki.ascii.pgm 256 col >> $(RAPORT_F)

clean:
	$(RM) main $(RAPORT_F)