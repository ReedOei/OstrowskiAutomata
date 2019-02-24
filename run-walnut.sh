#!/usr/bin/env bash

set -e

source "paths.sh"

if [[ -z "$1" ]]; then
    echo "Usage: bash run-walnut.sh FILENAME"
    echo "FILENAME - The path to the file containing the commands to run"
    exit 1
fi

fname="$(realpath "$1")"

cd "$WALNUT_PATH"
cp "$fname" "$WALNUT_COMMAND_FILES_PATH"
# Need this echo to avoid manually pressing Ctrl+D to end the session
echo "" | java -Xmx8g -cp bin Main.prover "$(basename "$fname")"

