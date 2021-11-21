#!/bin/bash

IDS=`ls /dev/mqueue/msg-*`

for id in $IDS; do
  unlink $id
done