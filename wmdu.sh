#!/usr/bin/env bash

# Watches memory usage of PID $1 and size of directory $2 and writes
# to standard output with timestamp.
#
# The script ends whenever target PID is no longer running

usage() {
  echo -e "\nUsage: $0 OUT TARGET_DIR -- COMMAND\n"
  echo "Runs COMMAND and periodically writes memory usage of COMMAND and size of TARGET_DIR to file OUT"
}

if (($# < 4)); then
  usage
  exit 1
fi

OUT=$1
TARGET_DIR=$2

shift 3

CMD=$*

# SMEM=$(guix build smem)/bin/smem
SMEM=/usr/bin/smem

T0=$(date +"%s.%6N")

watch() {

  TARGET_PID=$1
  TARGET_DIR=$2

  echo "Watching memory and disk usage for TARGET_PID=${TARGET_PID} TARGET_DIR=${TARGET_DIR}."

  while true; do

    NOW=$(date +"%s.%6N")

    TD=$(bc <<<"$NOW-$T0")

    # DU=$(du -cs "${TARGET_DIR}" 2>/dev/null | cut -f 1 | tail -n 1)
    DU=0

    # get child PIDs and transform into a grep expression
    CPID_E=$(ps --ppid "${TARGET_PID}" -o pid= | sed -z "s/\n/,/g;s/\s//g;s/,$//;s/,/\\\|^/g;s/^/^/")

    if [ -z "$CPID_E" ]; then
      MU=$(${SMEM} --total --columns="pid pss" | grep "^${TARGET_PID}" | awk '{sum+=$2} END {print sum}')
    else
      MU=$(${SMEM} --total --columns="pid pss" | grep -e "^${TARGET_PID}" -e "${CPID_E}" | awk '{sum+=$2} END {print sum}')
    fi

    echo -e "${TD}\t${DU}\t${MU}" >>$OUT

    sleep 0.1
  done
}

# start the CMD
$CMD &
TARGET_PID=$!

# start the watcher
watch $TARGET_PID $TARGET_DIR &
WATCH_PID=$!

on_term() {
  kill -TERM "$TARGET_PID"
  kill "$WATCH_PID"
}

trap on_term TERM INT

# wait for CMD
wait $TARGET_PID

# kill the watcher
kill $WATCH_PID
