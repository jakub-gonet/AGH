CC     := gcc
CFLAGS := -g -Wall -Wextra -ggdb3
SHELL  := /bin/bash
PIPE_NAME := consumer_producer.pipe
FILES_SIZE := 100
LOCK_FILE := consumer.lock

.PHONY: all test test_producers test_consumers test_producers_and_consumers clean

.SILENT: test_producers test_consumers test_producers_and_consumers test

all: consumer producer

consumer: consumer.c
	$(CC) $(CFLAGS) consumer.c -o consumer

producer: producer.c
	$(CC) $(CFLAGS) producer.c -o producer

test_files:
	mkdir -p test_files

test_files/1.txt: test_files
	yes '1' | head -c $(FILES_SIZE) | tr -d '\n' > test_files/1.txt

test_files/2.txt: test_files
	yes '2' | head -c $(FILES_SIZE) | tr -d '\n' > test_files/2.txt

test_files/3.txt: test_files
	yes '3' | head -c $(FILES_SIZE) | tr -d '\n' > test_files/3.txt

test_files/4.txt: test_files
	yes '4' | head -c $(FILES_SIZE) | tr -d '\n' > test_files/4.txt

test_files/5.txt: test_files
	yes '5' | head -c $(FILES_SIZE) | tr -d '\n' > test_files/5.txt

test: test_producers test_consumers test_producers_and_consumers
	echo "testing..."

test_producers: consumer producer test_files/1.txt test_files/2.txt test_files/3.txt test_files/4.txt test_files/5.txt
	./test_producers.sh
	diff template.txt consumer.txt && echo "test_producers - same as template"

test_consumers: consumer producer test_files/1.txt test_files/2.txt test_files/3.txt test_files/4.txt test_files/5.txt
	./test_consumers.sh
	diff template-one.txt consumer.txt && echo "test_consumers - same as template"

test_producers_and_consumers: consumer producer test_files/1.txt test_files/2.txt test_files/3.txt test_files/4.txt test_files/5.txt
	./test_producers_and_consumers.sh
	diff template.txt consumer.txt && echo "test_producers_and_consumers - same as template"

clean:
	$(RM) consumer producer test_files/*