#!/usr/bin/env bash

set -e

source "paths.sh"

encode_word() {
    name="$1"

    iconv -f utf-8 -t utf-16be "$name" > temp
    rm "$name"
    mv temp "$RESULTS/$name"

    if [[ "$name" =~ "_" ]]; then
        echo "Encoded $name to $RESULTS/$name and $WALNUT_AUTOMATA_PATH"
        cp "$RESULTS/$name" "$WALNUT_AUTOMATA_PATH"
    else
        echo "Encoded $name to $RESULTS/$name and $WALNUT_WORD_PATH"
        cp "$RESULTS/$name" "$WALNUT_WORD_PATH"
    fi
}

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
    encode_word "$fname"
done
encode_word "$c_alpha.txt"

mv "${word_name}_prf.txt" "$RESULTS"

bash gen-addition.sh "$num_sys" "$nonrep_part" "$rep_part"

verify_replacement "$word_name" "$RESULTS/${word_name}_prf.txt"

