#!/bin/bash
PIPE_NAME=consumer_producer.pipe
LOCK_FILE=consumer.lock


rm -f $PIPE_NAME
mkfifo $PIPE_NAME

./producer $PIPE_NAME 1 test_files/1.txt 5 &
./producer $PIPE_NAME 3 test_files/2.txt 5 &
./producer $PIPE_NAME 5 test_files/3.txt 5 &
./producer $PIPE_NAME 7 test_files/4.txt 5 &
./producer $PIPE_NAME 9 test_files/5.txt 5 &

touch $LOCK_FILE
./consumer $PIPE_NAME consumer.txt 5 &
./consumer $PIPE_NAME consumer.txt 5 &
./consumer $PIPE_NAME consumer.txt 5 &

wait < <(jobs -p)
sleep 5
rm -f $LOCK_FILE
rm -f $PIPE_NAME