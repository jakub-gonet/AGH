CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash

.PHONY: all run_test clean

all: sys lib

lib: 2_grep.c
	$(CC) $(CFLAGS) -DLIB 2_grep.c -o lib

sys: 2_grep.c
	$(CC) $(CFLAGS) 2_grep.c -o sys

run_test: sys lib test/a.txt
	$(RM) results.txt
	@echo -n == lib == >> results.txt
	{ time ./lib @ test/b.txt ; } 2>> results.txt 1>/dev/null
	@echo -ne "\n== sys ==" >> results.txt
	{ time ./sys @ test/b.txt ; } 2>> results.txt 1>/dev/null

clean:
	$(RM) sys lib results.txt