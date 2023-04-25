#! /bin/bash -e
#

usage() {
  echo "USGAGE: sample_store.sh OUT_PATH SAMPLE_LOWER LRU_SIZE RUN_NUMBER CUT_OFFSET"
}

if [ -z "$1" ]; then
  usage
  exit 1
fi

if [ -z "$2" ]; then
  usage
  exit 1
fi

if [ -z "$3" ]; then
  usage
  exit 1
fi

if [ -z "$4" ]; then
  usage
  exit 1
fi

if [ -z "$5" ]; then
  usage
  exit 1
fi

if ! [ -d "$1" ]; then
  mkdir -p "$1"
fi

eval "$(opam env)"

dune build bench/irmin-pack/tezos_read.exe
CMD="_build/default/bench/irmin-pack/tezos_read.exe"
OUT=$1
READ_EVERY_UPPER=$2
READ_EVERY_LOWER=$((READ_EVERY_UPPER * 50))
LRU=$3
RUN=$4
CUT_OFFSET=$5

clear_cache() {
  sync
  echo 3 >/proc/sys/vm/drop_caches
}

bench() {
  IFS=":"
  read -r root lower_root <<<"$1"
  upper="$OUT/$root.Upper.$LRU-$RUN.txt"
  lower="$OUT/$root.Lower.$LRU-$RUN.txt"

  echo "root: $root, lower: $lower_root"

  echo "Upper, $root"
  clear_cache
  if [ -n "$lower_root" ]; then
    $CMD --root="$root" --lower-root="$lower_root" --read-every="$READ_EVERY_UPPER" --lru-size="$LRU" --cut-offset="$CUT_OFFSET" --high-to-low=false --cut-side=Above >"$upper"
  else
    $CMD --root="$root" --read-every="$READ_EVERY_UPPER" --lru-size="$LRU" --cut-offset="$CUT_OFFSET" --high-to-low=false --cut-side=Above >"$upper"
  fi

  echo "Lower, $root"
  clear_cache
  if [ -n "$lower_root" ]; then
    $CMD --root="$root" --lower-root="$lower_root" --read-every="$READ_EVERY_LOWER" --lru-size="$LRU" --cut-offset="$CUT_OFFSET" --high-to-low=false --cut-side=Below >"$lower"
  else
    $CMD --root="$root" --read-every="$READ_EVERY_LOWER" --lru-size="$LRU" --cut-offset="$CUT_OFFSET" --high-to-low=false --cut-side=Below >"$lower"
  fi
}

stores="single_suffix small_volume:small_volume/lower large_volume:large_volume/lower"
IFS=" "
for store in $stores; do
  bench "$store"
done
