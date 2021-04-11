CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash
RAPORT_F := raport.txt

.PHONY: all clean

all: sender catcher

sender: sender.c
	$(CC) $(CFLAGS) sender.c -o sender

catcher: catcher.c
	$(CC) $(CFLAGS) catcher.c -o catcher

clean:
	$(RM) sender catcher

