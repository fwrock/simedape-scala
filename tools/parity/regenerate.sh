#!/usr/bin/env bash
set -euo pipefail
repo=$(cd -- "$(dirname -- "$0")/../.." && pwd)
reference=${SIMEDAPE_PYTHON_SOURCE:-"$repo/../simedape"}
image=${SIMEDAPE_REFERENCE_IMAGE:-htc-simedape:latest}
# Development-only golden generation; production JVM code never starts Python.
for fixture in source_metrics estimation training selection api_contract dtw multivariable observation; do
  if [[ "$fixture" == source_metrics ]]; then
    scratch=$(mktemp -d)
    mkdir -p "$scratch/main" "$scratch/test"
    docker run --rm -v "$reference:/usr/local/lib/python3.11/site-packages/pipeline:ro" -v "$repo/tools/parity/${fixture}_fixtures.py:/fixture.py:ro" -v "$scratch:/out" "$image" python /fixture.py
    cp "$scratch/main/variants.json" "$repo/src/main/resources/parity/variants.json"
    cp "$scratch/test/"*.json "$repo/src/test/resources/parity/"
    rm -rf -- "$scratch"
  else
    docker run --rm -v "$reference:/usr/local/lib/python3.11/site-packages/pipeline:ro" -v "$repo/tools/parity/${fixture}_fixtures.py:/fixture.py:ro" -v "$repo/src/test/resources/parity:/out" "$image" python /fixture.py
  fi
done
