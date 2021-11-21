#!/bin/bash
PIPE_NAME=consumer_producer.pipe
LOCK_FILE=consumer.lock


rm -f $PIPE_NAME
mkfifo $PIPE_NAME

./producer $PIPE_NAME 1 test_files/1.txt 5 &

touch $LOCK_FILE
./consumer $PIPE_NAME consumer.txt 5 &
./consumer $PIPE_NAME consumer.txt 5 &
./consumer $PIPE_NAME consumer.txt 5 &
wait < <(jobs -p)
rm -f  $LOCK_FILE
rm -f  $PIPE_NAME