CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash

.PHONY: all clean

all: server client

client: client.c common.h
	$(CC) $(CFLAGS) client.c -o client -lrt

server: server.c common.h
	$(CC) $(CFLAGS) server.c -o server -lrt

clean:
	$(RM) -r *.dSYM server client /dev/mqueue/msg-*