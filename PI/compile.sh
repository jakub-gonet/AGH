#!/usr/bin/env bash
set -euo pipefail

[[ -z "$1" ]] && >&2 echo "No file given" && exit 1

filename=$(basename -- "$1")
mkdir -p bin
ext="${filename##*.}"
std="c++11"; cc="clang++"
if [ "$ext" == 'c' ]; then std="c11"; cc="clang"; fi
"$cc" -std="$std" -O2 -Wall -Wextra -Wpedantic -ftrapv -ggdb3 -o "bin/${filename%.*}" "$1" -lm && echo "bin/${filename%.*}"
