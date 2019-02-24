#!/usr/bin/env bash

set -e

source "paths.sh"

if [[ -z "$1" ]]; then
    echo "Usage: bash try-theorems.sh THEOREMS_FILE NUM_SYS_NAME NONREP REP"
    echo "THEOREMS_FILE - The file containing walnut commands to run"
    echo "NUM_SYS_NAME  - The name of the number system to generate"
    echo "NONREP        - The nonrepeating part of the continued fraction"
    echo "REP           - The repeating part of the continued fraction"
    exit 1
fi

theorems_file="$1"
num_sys="$2"
nonrep="$3"
rep="$4"

bash gen-addition.sh "$num_sys" "$nonrep" "$rep"

cat "$theorems_file" | grep -E "^eval" | while read thm; do
    base_prf_name="$(echo "$thm" | cut -f2 -d" ")"
    prf_name="${base_prf_name}_${num_sys}"
    to_check="$(echo "$thm" | sed -E "s/NUM_SYS/$num_sys/g" | sed -E "s/$base_prf_name/$prf_name/g")"

    echo "Checking theorem: $to_check"

    (
        cd "$WALNUT_PATH"
        echo "$to_check" | java -Xmx8g -cp "bin" Main.prover
    )

    bash check-true.sh "$WALNUT_RESULTS_PATH/$prf_name.txt"
done

