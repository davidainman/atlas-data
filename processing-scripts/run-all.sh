#!/bin/sh

echo "Running Alignment scripts"
python alignment/alignment_aggregation.py
echo "Regenerating derived features"
cd derived-features/
Rscript derived-features.R
cd ../