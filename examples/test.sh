#!/bin/bash

echo "=== Classical ==="
for i in examples/*.rgl; do echo "Opening file " $i; ./reglisse -t $i; done
echo "=== Cooperative ==="
for i in examples/*.rgl; do echo "Opening file " $i; ./reglisse -t -m 1 $i; done
echo "=== Adversarial ==="
for i in examples/*.rgl; do echo "Opening file " $i; ./reglisse -t -m 2 $i; done
echo "=== Admissible ==="
for i in examples/*.rgl; do echo "Opening file " $i; ./reglisse -t -m 3 $i; done

