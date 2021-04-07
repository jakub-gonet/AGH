CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash
RAPORT_F := raport.txt

.PHONY: all test clean

all: main

main: main.c
	$(CC) $(CFLAGS) main.c -o main

test: main
	./main

clean:
	$(RM) main $(RAPORT_F)