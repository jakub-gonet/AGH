CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash

.PHONY: all test clean

all: server client

client: client.c
	$(CC) $(CFLAGS) client.c -o client

server: server.c
	$(CC) $(CFLAGS) server.c -o server

test: server client

clean:
	./remove_queues.sh
	$(RM) -r *.dSYM server client