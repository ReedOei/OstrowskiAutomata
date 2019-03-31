#!/usr/bin/env bash

set -e

source "lib.sh"

if [[ -z "$1" ]]; then
    echo "Usage: bash generate-base.sh MAX_DIG"
    echo "MAX_DIG - The upper bound on the continued fraction values."
    exit 1
fi

max_dig="$1"

stack install
Ostrowski base "$max_dig"

mv "base_add_$max_dig.txt" "$WALNUT_CUSTOM_BASES_PATH/lsd_base_${max_dig}_addition.txt"
mv "base_$max_dig.txt" "$WALNUT_CUSTOM_BASES_PATH/lsd_base_${max_dig}.txt"

