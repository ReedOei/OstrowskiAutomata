#!/usr/bin/env bash

set -e

source "lib.sh"

if [[ -z "$1" ]]; then
    echo "Usage: bash general-automata.sh MAX_DIG"
    echo "MAX_DIG - The upper bound on the continued fraction values."
    exit 1
fi

max_dig="$1"

stack install
Ostrowski general "$max_dig"

mv "general_recog_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "general_lt_temp_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "general_eq_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "general_zero_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "general_one_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "general_add_alg0_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
# mv "general_add_alg1_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "general_add_alg2_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "general_add_alg3_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "C_general_$max_dig.txt" "$WALNUT_WORD_PATH"

prfs="$(realpath "general_${max_dig}_prfs.txt")"

(
    cd "$WALNUT_PATH"
    cat "$prfs" | java -cp bin Main.prover

    check_true "Result/base_proof_${max_dig}.txt"
    check_true "Result/successor_proof_${max_dig}.txt"
)

