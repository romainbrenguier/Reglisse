#!/bin/bash

echo "=== Classical ==="
for i in examples/*.rgl; do
    echo "Classical " $i;
    ./reglisse -t $i; 
    echo "Cooperative " $i;
    ./reglisse -t -m 1 $i; 
    echo "Adversarial " $i;
    ./reglisse -t -m 2 $i; 
    echo "Admissible " $i;
    ./reglisse -t -m 3 $i; 
done


