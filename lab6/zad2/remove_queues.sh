#!/bin/bash

IDS=`ipcs -q | grep 0x | cut -f2 -d" "`

for id in $IDS; do
  ipcrm -q $id;
done