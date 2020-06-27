#!/usr/bin/env bash
set -euo pipefail
[[ -z "$1" ]] && >&2 echo "No file given" && exit 1

realname="$1"
docker build -t ubuntu . > /dev/null
docker run -ti  -v $PWD:/programs ubuntu sh -c "cd /programs/ \
&& gcc -std=c11 -O2 -Wall -o program "$realname" -lm \
&& echo '--- Program started ---' && ./program; rm program"