CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash
RAPORT_F := raport.txt

.PHONY: all test clean

all: server client

server: server.c common.h
	$(CC) $(CFLAGS) server.c -o server -lpthread

client: client.c common.h
	$(CC) $(CFLAGS) client.c -o client -lpthread

clean:
	$(RM) server client