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
# stack exec -- Ostrowski general "$max_dig" +RTS -h -p
# stack exec -- Ostrowski general "$max_dig"
Ostrowski general "$max_dig" # +RTS -h -p

mv "recog_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "lt_temp_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "eq_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "zero_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "one_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "add_alg0_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "add_alg1_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "add_alg2_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "add_alg3_$max_dig.txt" "$WALNUT_AUTOMATA_PATH"
mv "C_$max_dig.txt" "$WALNUT_WORD_PATH"

prfs="$(realpath "general_${max_dig}_prfs.txt")"

(
    cd "$WALNUT_PATH"
    cat "$prfs" | java -Xmx8g -cp bin Main.prover

    check_true "Result/base_proof_${max_dig}.txt"
    check_true "Result/successor_proof_${max_dig}.txt"
)

