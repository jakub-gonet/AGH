CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash

.PHONY: all test clean

all: consumer producer

consumer: consumer.c
	$(CC) $(CFLAGS) consumer.c -o consumer

producer: producer.c
	$(CC) $(CFLAGS) producer.c -o producer

test: consumer producer
	mkfifo consumer_producer.pipe
	rm consumer_producer.pipe

clean:
	$(RM) consumer producer