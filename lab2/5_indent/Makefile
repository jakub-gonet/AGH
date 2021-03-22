CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash

.PHONY: all run_test clean

all: sys lib

lib: 5_indent.c
	$(CC) $(CFLAGS) -DLIB 5_indent.c -o lib

sys: 5_indent.c
	$(CC) $(CFLAGS) 5_indent.c -o sys

test/b.txt:
	base64 -w 256 /dev/random | head -c 400000 > test/b.txt

run_test: sys lib test/b.txt
	$(RM) results.txt
	@echo -n == lib == >> results.txt
	{ time ./lib test/b.txt out.txt; } 2>> results.txt
	@echo -ne "\n== sys ==" >> results.txt
	{ time ./sys test/b.txt out.txt; } 2>> results.txt

clean:
	$(RM) sys lib results.txt out.txt