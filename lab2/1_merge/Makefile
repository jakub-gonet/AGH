CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash

.PHONY: all test clean

all: sys lib

lib: 1_merge.c
	$(CC) $(CFLAGS) -DLIB 1_merge.c -o lib

sys: 1_merge.c
	$(CC) $(CFLAGS) 1_merge.c -o sys

test: sys lib
	@echo lib >> results.txt
	{ time ./sys test/a.txt test/b.txt ; } 1>> results.txt
	@echo sys >> results.txt
	{ time ./sys test/a.txt test/b.txt ; } 1>> results.txt

clean:
	$(RM) sys lib results.txt