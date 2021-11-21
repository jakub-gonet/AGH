CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash

.PHONY: all run_test clean

all: sys lib

lib: 4_sed.c
	$(CC) $(CFLAGS) -DLIB 4_sed.c -o lib

sys: 4_sed.c
	$(CC) $(CFLAGS) 4_sed.c -o sys

run_test: sys lib
	$(RM) results.txt
	@echo -n == lib == >> results.txt
	{ time ./lib test/a.txt out.txt 'Lorem ipsum dolor sit amet' 'prosze panstwa pawel bedzie skakal, tutaj mamy metr piecdziesiat no jest wysoko'; } 2>> results.txt 1>/dev/null
	@echo -ne "\n== sys ==" >> results.txt
	{ time ./sys test/a.txt out.txt 'Lorem ipsum dolor sit amet' 'ala rzeczywiscie ala ala'; } 2>> results.txt 1>/dev/null

clean:
	$(RM) sys lib results.txt out.txt