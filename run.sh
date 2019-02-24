#!/usr/bin/env bash

set -e

source "lib.sh"

verify_replacement() {
    word_name="$1"
    prf="$(realpath "$2")"

    (
        cd "$WALNUT_PATH"
        cat "$prf" | java -cp bin Main.prover

        for proof_name in $(cat "$prf" | cut -f2 -d" "); do
            if [[ ! -z "$proof_name" ]]; then
                bash check-true.sh "$WALNUT_RESULTS_PATH/$proof_name.txt"
            fi
        done
    )
}

word_name="$1"
num_sys="$2"
alphabet="$3"
zero_rep="$4"
one_rep="$5"
nonrep_part="$6"
rep_part="$7"
c_alpha="$8"

stack install
Ostrowski "generate" "$word_name" "$num_sys" "$alphabet" "$zero_rep" "$one_rep" "$nonrep_part" "$rep_part"
Ostrowski "generate_output" "$word_name" "$num_sys" "$alphabet" "$zero_rep" "$one_rep" "$nonrep_part" "$rep_part"
Ostrowski "proof" "$word_name" "$num_sys" "$c_alpha" "$zero_rep" "$one_rep" "$nonrep_part" "$rep_part"
Ostrowski "c_alpha" "$c_alpha" "$num_sys" "$alphabet"

for fname in $(find -maxdepth 1 -name "$word_name*.txt" -not -name "*prf*" -type f); do
    if [[ "$fname" ~= "_" ]]; then
        encode_word "$fname" 0
    else
        encode_word "$fname" 1
    fi
done
encode_word "$c_alpha.txt" 1

mv "${word_name}_prf.txt" "$RESULTS"

bash gen-addition.sh "$num_sys" "$nonrep_part" "$rep_part"

verify_replacement "$word_name" "$RESULTS/${word_name}_prf.txt"

