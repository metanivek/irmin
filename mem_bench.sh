#!/usr/bin/env sh

out_dir=lru_bench_gc
trace=/home/metanivek/tmp/data4_100066commits.repr

# rm -rf ${out_dir}
if ! [ -d "$out_dir" ]; then
  mkdir -p "$out_dir"
fi

rm -rf _artefacts

# dune build bench

# clear_cache() {
#   sync
#   echo 3 >/proc/sys/vm/drop_caches
# }

bench() {
  # clear_cache
  out="${out_dir}/${1}mb"
  rm "${out}"
  # ./wmdu.sh "$out" _artefacts -- ./_build/default/bench/irmin-pack/tree.exe "$trace" --lru-max-memory="$1" --no-summary --ncommits-trace=10000
  # --gc-distance-in-the-past=VAL (absent=5000)
  #     Distance between the GC commit and the latest commit
  # --gc-every=VAL (absent=1000)
  #     Distance between calls to GC
  # --gc-wait-after=VAL (absent=0)
  #     How many commits separate the start of a GC and the moment we wait
  #     for the end of it
  ./wmdu2.sh "$out" _artefacts -- ./_build/default/bench/irmin-pack/tree.exe "$trace" --lru-max-memory="$1" --no-summary --ncommits-trace=100000 \
    --gc-every=8192 \
    --gc-distance-in-the-past="$(echo "8192 * 5" | bc -l)"
  gnuplot -c memory2.gp "$out"

  # clear_cache
  # out="${out_dir}/${1}mb.nogc"
  # rm out
  # ./wmdu.sh "$out" _artefacts -- ./_build/default/bench/irmin-pack/tree.exe "$trace" --lru-max-memory="$1" --no-summary --ncommits-trace=30000 --gc-every=0
  # gnuplot -c memory.gp "$out"

}

# levels="0 125 250 500 1000"
levels="250"
IFS=" "
for level in $levels; do
  bench "$level"
done

# xdg-open "${out_dir}/125.stats.png"
