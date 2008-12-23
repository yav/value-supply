#!/bin/bash

# Bench program written by Sebastian Fischer <sebf@informatik.uni-kiel.de>

set -e

rm -rf dist Bench
mkdir -p dist
ghc --make -O2 -hidir dist -odir dist -package ghc Bench.hs

for ((count=100; count<=10000000; count*=10)); do
  ./Bench ${count}
done

