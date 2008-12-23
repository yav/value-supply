#!/bin/bash

# Bench program written by Sebastian Fischer <sebf@informatik.uni-kiel.de>

if [ -e dist ]; then rm -rf dist; fi
mkdir -p dist
ghc --make -O2 -hidir dist -odir dist -package ghc Bench.hs

for ((count=100; count<=100000000; count*=10)); do
  echo "`./Bench ${count}` for ${count} names"
done

