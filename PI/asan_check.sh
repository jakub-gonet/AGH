#!/usr/bin/env bash
set -euo pipefail
[[ -z "$1" ]] && >&2 echo "No file given" && exit 1

compiler_options='-std=c11 -O2 -Wall -lm -g -fsanitize=address,undefined,leak -fno-omit-frame-pointer'
realname="$1"
docker build -t valgrind:0.1 . > /dev/null
docker run -ti -v $PWD:/to_check valgrind:0.1 sh -c "cd /to_check/; \
gcc \"$realname\" -o check ${compiler_options} \
&& echo \"Compiled, running...\n\" &&./check && rm check"
