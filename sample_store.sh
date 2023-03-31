#! /bin/bash -e
#

usage() {
  echo "USGAGE: sample_store.sh OUT_PATH SAMPLE LRU_SIZE RUN_NUMBER"
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

if ! [ -d "$1" ]; then
  mkdir -p "$1"
fi

eval "$(opam env)"

dune build bench/irmin-pack/tezos_read.exe
CMD="_build/default/bench/irmin-pack/tezos_read.exe"
OUT=$1
READ_EVERY=$2
LRU=$3
RUN=$4

clear_cache() {
  sync
  echo 3 >/proc/sys/vm/drop_caches
}

bench() {
  IFS=":"
  read -r root lower_root <<<"$1"
  high_to_low="$OUT/$root.HL.$LRU-$RUN.txt"
  low_to_high="$OUT/$root.LH.$LRU-$RUN.txt"

  echo "root: $root, lower: $lower_root"

  echo "high to low, $root"
  clear_cache
  if [ -n "$lower_root" ]; then
    $CMD --root="$root" --lower-root="$lower_root" --read-every="$READ_EVERY" --lru-size="$LRU" --high-to-low=true >"$high_to_low"
  else
    $CMD --root="$root" --read-every="$READ_EVERY" --lru-size="$LRU" --high-to-low=true >"$high_to_low"
  fi

  echo "low to high, $root"
  clear_cache
  if [ -n "$lower_root" ]; then
    $CMD --root="$root" --lower-root="$lower_root" --read-every="$READ_EVERY" --lru-size="$LRU" --high-to-low=false >"$low_to_high"
  else
    $CMD --root="$root" --read-every="$READ_EVERY" --lru-size="$LRU" --high-to-low=false >"$low_to_high"
  fi
}

stores="single_suffix small_volume:small_volume/lower large_volume:large_volume/lower"
# stores="small_volume_no_lower large_volume_no_lower"
IFS=" "
for store in $stores; do
  bench "$store"
done
