#!/bin/sh

DTPATH=/var/log/dt

# nothing to do if DT directory not present
test ! -d "${DTPATH}" && exit 0

# find the oldest trace files beyond MAX_MB size and delete
find "${DTPATH}" -mindepth 1 -type f -printf '%T@ %k %p\n' | sort -nr | awk -v max_mb=${MAX_MB:=100} 'BEGIN{size=0}{size+=$2; if (size>1000*max_mb) {$1=""; $2=""; print $0}}' | xargs -r rm

# delete any remaining files that are older than MAX_DAYS days
find "${DTPATH}" -mindepth 1 -mtime ${MAX_DAYS:=+30} -delete

# clean up any empty directories
find "${DTPATH}" -mindepth 1 -type d -empty -delete
