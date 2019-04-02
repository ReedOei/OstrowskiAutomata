#!/usr/bin/env bash

source "lib.sh"

if [[ -z "$1" ]]; then
    echo "Usage: bash make-utility.sh K MAX_POW"
    echo "This script generates various utility predicates, as well as a second version of everything accepting only binary."
    echo "K - The upper bound on the continued fraction."
    echo "MAX_POW - The highest power (e.g., square, cube) to generate a predicate for. Also generates a predicate for a * b = c for a in 1..MAX_POW"
    echo "Assumes you have already generated the addition automata."
    exit
fi

binary() {
    path="$1"

    dir="$(dirname "$path")"
    name="$(basename "$path")"
    new_path="$dir/bin_$name"

    tmpfile="$(mktemp)"
    echo "Processing $path"
    Ostrowski toBase 2 "$path" |& (grep "Minimiz" |& tee /dev/stderr) | grep -v "Minimiz" > "$tmpfile"
    iconv -f utf-8 -t utf-16be "$tmpfile" -o "$new_path"
    rm "$tmpfile"
}

times() {
    val="$1"

    echo "def times_${val}_${max_dig} \"?lsd_base_${max_dig} \$recog_${max_dig}(a,b) & Ez \$times_$((val - 1))_${max_dig}(a,b,z) & \$add_${max_dig}(a,b,z,c)\":"
}

pow() {
    val="$1"

    echo "def pow_${val}_${max_dig} \"?lsd_base_${max_dig} n > 0 & \$recog_${max_dig}(a,n) & i > 0 & \$recog_${max_dig}(a,i) & (At (\$recog_${max_dig}(a,t) & (Emax \$times_$((val - 1))_${max_dig}(a,n,max) & t < max)) => (\$C_${max_dig}_p(a,i,t) <=> \$C_${max_dig}_p_p(a,i,t,n)))\":"
}

max_dig="$1"
max_pow="$2"

cur_dir="$(pwd)"
util_path="$cur_dir/utility_${max_dig}.txt"

stack install

binary "$WALNUT_AUTOMATA_PATH/add_${max_dig}.txt"
binary "$WALNUT_AUTOMATA_PATH/lt_${max_dig}.txt"
binary "$WALNUT_AUTOMATA_PATH/lte_${max_dig}.txt"
binary "$WALNUT_AUTOMATA_PATH/sub_${max_dig}.txt"
binary "$WALNUT_AUTOMATA_PATH/double_${max_dig}.txt"
binary "$WALNUT_AUTOMATA_PATH/successor_${max_dig}.txt"

(
    echo "def C_${max_dig}_p \"?lsd_base_${max_dig} Ez \$add_${max_dig}(a,x,y,z) & C_${max_dig}[z] = @1\":"
    echo "def C_${max_dig}_s \"?lsd_base_${max_dig} Ez \$sub_${max_dig}(a,x,y,z) & C_${max_dig}[z] = @1\":"
    echo "def C_${max_dig}_s_s \"?lsd_base_${max_dig} Ez \$sub_${max_dig}(a,b,c,z) & \$C_${max_dig}_s(a,z,d)\":"
    echo "def C_${max_dig}_p_s \"?lsd_base_${max_dig} Ez \$add_${max_dig}(a,b,c,z) & \$C_${max_dig}_s(a,z,d)\":"
    echo "def C_${max_dig}_p_p \"?lsd_base_${max_dig} Ez \$add_${max_dig}(a,b,c,z) & \$C_${max_dig}_p(a,z,d)\":"
    echo "def C_${max_dig}_p_p_s \"?lsd_base_${max_dig} Ez \$add_${max_dig}(a,b,c,z) & \$C_${max_dig}_p_s(a,z,d,e)\":"
    echo "def C_${max_dig}_p_s_s \"?lsd_base_${max_dig} Ez \$add_${max_dig}(a,b,c,z) & \$C_${max_dig}_s_s(a,z,d,e)\":"

    echo "def times_1_${max_dig} \"?lsd_base_${max_dig} \$recog_${max_dig}(a,b) & b = c\":"

    for i in $(seq 2 "$max_pow"); do
        times "$i"

        pow "$i"
    done
) > "$util_path"

(
    cd "$WALNUT_PATH"

    cat "$util_path" | java -Xmx45g -cp bin Main.prover |& tee "util-log-${max_dig}.txt"
)

cat "utility_${max_dig}.txt" | while read line; do
    pred_name="$(echo "$line" | cut -f2 -d" ")"
    binary "$WALNUT_AUTOMATA_PATH/$pred_name.txt"
done

