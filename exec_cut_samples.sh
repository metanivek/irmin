#! /bin/bash -e
#

OUT=sample_cut
SAMPLE=1000   # for upper, lower will be multiplied by 50
LRU_SIZE=5000 # tezos lru size -- TINY
CUT=35426804898

./sample_store_cut.sh $OUT $SAMPLE $LRU_SIZE 0 $CUT
./sample_store_cut.sh $OUT $SAMPLE $LRU_SIZE 1 $CUT
./sample_store_cut.sh $OUT $SAMPLE $LRU_SIZE 2 $CUT
