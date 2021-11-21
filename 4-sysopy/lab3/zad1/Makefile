CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash

n ?= 10

.PHONY: all run_test clean

all: main

main: main.c
	$(CC) $(CFLAGS) main.c -o main

run_test: main
	./main $(n)

clean:
	$(RM) main