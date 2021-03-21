CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash

.PHONY: all run_test clean

all: sys lib

lib: 1_merge.c
	$(CC) $(CFLAGS) -DLIB 1_merge.c -o lib

sys: 1_merge.c
	$(CC) $(CFLAGS) 1_merge.c -o sys

dirs: test
	mkdir -p test

test/b64_a.txt: dirs
	base64 -w 256 /dev/random | head -c 4000000 > test/b64_a.txt

test/b64_b.txt: dirs
	base64 -w 256 /dev/random | head -c 4000000 > test/b64_b.txt

run_test: sys lib test/b64_a.txt test/b64_b.txt
	$(RM) results.txt
	@echo -n == lib == >> results.txt
	{ time ./sys test/b64_a.txt test/b64_b.txt ; } 2>> results.txt 1>/dev/null
	@echo -ne "\n== sys ==" >> results.txt
	{ time ./sys test/b64_a.txt test/b64_b.txt ; } 2>> results.txt 1>/dev/null

clean:
	$(RM) sys lib results.txt