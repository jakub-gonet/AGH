CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash

.PHONY: all run_test clean

all: main

main: block_manager.o main.o
	$(CC) $(CFLAGS) block_manager.o main.o -o main

main.o: main.c lib/vector.h
	$(CC) $(CFLAGS) -c main.c

block_manager.o: lib/block_manager.c lib/block_manager.h lib/vector.h
	$(CC) $(CFLAGS) -c lib/block_manager.c

run_test: all
	@echo "Running tests..."

clean:
	$(RM) block_manager.o main.o main