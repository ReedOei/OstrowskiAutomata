#!/usr/bin/env bash

set -e

if [[ -z "$1" ]]; then
    echo "Usage: bash check-true.sh WALNUT_COMMAND"
    exit 1
fi

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

fname="$1"

iconv -f utf-16 -t utf-8 "$fname" -o temp
content="$(cat "temp")"

if [[ "$content" == "true" ]]; then
    print_color "green" "$fname: proved true"
else
    print_color "red" "$fname: failed to prove true (got: '$content')"
fi

rm temp

