#!/usr/bin/env bash

WALNUT_PATH="$HOME/Walnut"
WALNUT_WORD_PATH="$WALNUT_PATH/Word Automata Library"
WALNUT_AUTOMATA_PATH="$WALNUT_PATH/Automata Library"
WALNUT_COMMAND_FILES_PATH="$WALNUT_PATH/Command Files"
WALNUT_CUSTOM_BASES_PATH="$WALNUT_PATH/Custom Bases"
WALNUT_RESULTS_PATH="$WALNUT_PATH/Result"
GENERAL_ADDITION_AUTOMATON_PATH="./general-ostrowski-addition-automaton-java"
RESULTS="results"

run_walnut() {
    fname="$(realpath "$1")"

    (
        cd "$WALNUT_PATH"
        cp "$fname" "$WALNUT_COMMAND_FILES_PATH"
        # Need this echo to avoid manually pressing Ctrl+D to end the session
        echo "" | java -Xmx8g -cp bin Main.prover "$(basename "$fname")"
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

to_space_list() {
    echo "$1" | sed -E "s/\]//g" | sed -E "s/,/ /g" | sed -E "s/\[//g"
}

encode_word() {
    name="$1"
    is_word="$2"

    iconv -f utf-8 -t utf-16be "$name" > temp
    rm "$name"
    mv temp "$RESULTS/$name"

    if [[ "$is_word" == "0" ]]; then
        echo "Encoded $name to $RESULTS/$name and $WALNUT_AUTOMATA_PATH"
        cp "$RESULTS/$name" "$WALNUT_AUTOMATA_PATH"
    else
        echo "Encoded $name to $RESULTS/$name and $WALNUT_WORD_PATH"
        cp "$RESULTS/$name" "$WALNUT_WORD_PATH"
    fi
}

check_true() {
    fname="$1"

    (
        iconv -f utf-16 -t utf-8 "$fname" -o temp
        content="$(cat "temp")"

        if [[ "$content" == "true" ]]; then
            print_color "green" "$fname: proved true"
        else
            print_color "red" "$fname: failed to prove true (got: '$content')"
        fi

        rm temp
    )
}

