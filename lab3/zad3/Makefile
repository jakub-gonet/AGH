CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash

.PHONY: all test clean

all: main

main: main.c
	$(CC) $(CFLAGS) main.c -o main

test: main
	./main test kot 3

clean:
	$(RM) main