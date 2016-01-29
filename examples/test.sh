#!/bin/bash

for i in examples/*.rgl; do echo "Opening file " $i; ./reglisse $i; done
