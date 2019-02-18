#!/usr/bin/env bash

WALNUT_PATH="$HOME/Walnut"
WALNUT_WORD_PATH="$WALNUT_PATH/Word Automata Library"
WALNUT_AUTOMATA_PATH="$WALNUT_PATH/Automata Library"
WALNUT_COMMAND_FILES_PATH="$WALNUT_PATH/Command Files"
WALNUT_CUSTOM_BASES_PATH="$WALNUT_PATH/Custom Bases"
WALNUT_RESULTS_PATH="$WALNUT_PATH/Result"
GENERAL_ADDITION_AUTOMATON_PATH="./general-ostrowski-addition-automaton-java"
RESULTS="results"

run_walnut_command() {
    fname="$(realpath "$1")"

    (
        cd "$WALNUT_PATH"
        cp "$fname" "$WALNUT_COMMAND_FILES_PATH"
        # Need this to avoid manually pressing Ctrl+D to end the session
        echo "" | java -cp bin Main.prover "$(basename "$fname")"
    )
}

encode_word() {
    name="$1"

    iconv -f utf-8 -t utf-16be "$name.txt" > temp
    rm "$name.txt"
    mv temp "$RESULTS/$name.txt"
    cp "$RESULTS/$name.txt" "$WALNUT_WORD_PATH"
}

to_space_list() {
    echo "$1" | sed -E "s/\]//g" | sed -E "s/,/ /g" | sed -E "s/\[//g"
}

generate_addition_automaton() {
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

        run_walnut_command "output/$sys/${sys}AdditionCommand.txt"

        (
            cd "$WALNUT_PATH"
            cp "$WALNUT_RESULTS_PATH/lsd_${sys}_addition.txt" "$WALNUT_CUSTOM_BASES_PATH"
            cp "$WALNUT_AUTOMATA_PATH/lsd_${sys}.txt" "$WALNUT_CUSTOM_BASES_PATH"
        )

        rm "$input_file"
    )
}

print_color_num() {
    cnum="$1"
    msg="$2"
    flags="$3"

    tput setaf "$cnum"
    echo $flags "$msg"
    tput sgr0
}

print_color() {
    color="$1"
    msg="$2"
    flags="$3"
    cnum=0

    if [[ "$color" == "red" ]]; then
        cnum=1
    elif [[ "$color" == "green" ]]; then
        cnum=2
    elif [[ "$color" == "yellow" ]]; then
        cnum=3
    elif [[ "$color" == "blue" ]]; then
        cnum=4
    elif [[ "$color" == "magenta" ]]; then
        cnum=5
    elif [[ "$color" == "cyan" ]]; then
        cnum=6
    elif [[ "$color" == "white" ]]; then
        cnum=7
    fi

    print_color_num "$cnum" "$msg" "$flag"
}

check_true() {
    fname="$1"

    iconv -f utf-16 -t utf-8 "$fname" -o temp
    content="$(cat "temp")"

    if [[ "$content" == "true" ]]; then
        print_color "green" "$fname: proved true"
    else
        print_color "red" "$fname: failed to prove true (got: '$content')"
    fi
}

verify_replacement() {
    prf="$(realpath "$1")"

    (
        cd "$WALNUT_PATH"
        cat "$prf" | java -cp bin Main.prover

        check_true "$WALNUT_RESULTS_PATH/replace_1_between_0.txt"
        check_true "$WALNUT_RESULTS_PATH/replace_0_between_1.txt"
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

stack build Ostrowski
stack exec Ostrowski "generate" "$word_name" "$num_sys" "$alphabet" "$zero_rep" "$one_rep" "$nonrep_part" "$rep_part"
stack exec Ostrowski "proof" "$word_name" "$num_sys" "$c_alpha" "$zero_rep" "$one_rep"
stack exec Ostrowski "c_alpha" "$c_alpha" "$num_sys" "$alphabet"

encode_word "$word_name"
encode_word "$c_alpha"

mv "${word_name}_prf.txt" "$RESULTS"

generate_addition_automaton "$num_sys" "$nonrep_part" "$rep_part"

verify_replacement "$RESULTS/${word_name}_prf.txt"

