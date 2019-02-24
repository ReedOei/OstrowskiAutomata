#!/usr/bin/env bash

set -e

source "lib.sh"

if [[ -z "$1" ]] || [[ -z "$2" ]] || [[ -z "$3" ]]; then
    echo "Usage: bash gen-addition.sh NUM_SYS_NAME NONREP REP"
    echo "NUM_SYS_NAME - The name of the number system to generate"
    echo "NONREP       - The nonrepeating part of the continued fraction"
    echo "REP          - The repeating part of the continued fraction"
    exit 1
fi

sys="$(echo "$1" | sed -E "s/lsd_//g" | sed -E "s/msd_//g")"
nonrep="$2"
rep="$3"

(
    cd "$GENERAL_ADDITION_AUTOMATON_PATH"

    input_file="$sys-input.txt"

    (
        echo "$sys"
        to_space_list "$nonrep" | sed -E "s/^[0-9]+ ?//"
        to_space_list "$rep"
    ) | tee "$input_file"

    java -jar "Automaton_Producer.jar" "$input_file"

    find "output/$sys" -type f -not -name "*Command.txt" | while read fname; do
        cp "$fname" "$WALNUT_AUTOMATA_PATH"
    done
)

run_walnut "$GENERAL_ADDITION_AUTOMATON_PATH/output/$sys/${sys}AdditionCommand.txt"

(
    cd "$WALNUT_PATH"
    cp "$WALNUT_RESULTS_PATH/lsd_${sys}_addition.txt" "$WALNUT_CUSTOM_BASES_PATH"
    # Ostrowski minimize "$WALNUT_CUSTOM_BASES_PATH/lsd_${sys}_addition.txt"
    cp "$WALNUT_AUTOMATA_PATH/lsd_${sys}.txt" "$WALNUT_CUSTOM_BASES_PATH"
    # Ostrowski minimize "$WALNUT_CUSTOM_BASES_PATH/lsd_${sys}.txt"
)

rm "$GENERAL_ADDITION_AUTOMATON_PATH/$sys-input.txt"

