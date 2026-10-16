#!/usr/bin/env bash
# Copy the final/samplesheet.csv of every dnaseq dataset from yew's manualDelivery
# into <cacheDir>, keeping the manualDelivery-relative paths.
#
# usage: fetch_samplesheets.bash <datasets.tsv from dnaseq_datasets.py> <cacheDir> [host]
set -euo pipefail

datasets=$1
cacheDir=$2
host=${3:-yew}
manualDelivery=/eupath/data/EuPathDB/manualDelivery

mkdir -p "$cacheDir"
tail -n +2 "$datasets" | cut -f6 \
  | ssh -o BatchMode=yes -o ClearAllForwardings=yes "$host" \
      "cd $manualDelivery && while read -r p; do if [ -f \"\$p\" ]; then echo \"\$p\"; else echo \"MISSING \$p\" >&2; fi; done | tar cf - -T -" \
  | tar xf - -C "$cacheDir"
