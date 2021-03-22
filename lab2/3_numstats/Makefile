CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash

.PHONY: all run_test clean

all: sys lib

lib: 3_numstats.c
	$(CC) $(CFLAGS) -DLIB 3_numstats.c -o lib

sys: 3_numstats.c
	$(CC) $(CFLAGS) 3_numstats.c -o sys

run_test: sys lib test/a.txt
	$(RM) results.txt
	@echo -n == lib == >> results.txt
	{ time ./sys test/b.txt ; } 2>> results.txt 1>/dev/null
	@echo -ne "\n== sys ==" >> results.txt
	{ time ./lib test/b.txt ; } 2>> results.txt 1>/dev/null

clean:
	$(RM) sys lib results.txt a.txt b.txt c.txt