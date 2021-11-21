#!/usr/bin/env bash
set -euo pipefail
[[ -z "$1" ]] && >&2 echo "No file given" && exit 1

filename="$1"
ext="${filename##*.}"
cc="g++"
std="c++11"
if [ "$ext" == 'c' ]; then std="c11"; cc="gcc"; fi
echo $ext
echo $cc

docker build -t valgrind:0.1 . > /dev/null
docker run -ti -v $PWD:/to_check valgrind:0.1 sh -c "cd /to_check/; \
"$cc" -std="$std" -O0 -Wall -Wextra -Wpedantic -ftrapv  -ggdb3 -o valg_check "$filename" -lm \
&& valgrind --vgdb-error=0 --leak-check=full --show-leak-kinds=all ./valg_check && rm valg_check"
