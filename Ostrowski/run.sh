#!/usr/bin/env bash

WALNUT_WORD_PATH="$HOME/Walnut/Word Automata Library"

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

iconv -f utf-8 -t utf-16be "$word_name.txt" > temp
mv temp "$word_name.txt"

cp "$word_name.txt" "$WALNUT_WORD_PATH"

