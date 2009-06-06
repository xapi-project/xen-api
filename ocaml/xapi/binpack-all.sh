#!/bin/sh
# Create a set of graphs from a binpack performance test

set -e

./binpack -graph /tmp/output.dat -graph_n 64 -graph_r 64

for i in time choice success
do
  gnuplot binpack-$i.gp > binpack-$i.ps
done