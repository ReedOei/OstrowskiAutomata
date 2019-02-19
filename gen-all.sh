#!/usr/bin/env bash

set -e

bash "run.sh" "X3" "lsd_x3" "[0,1,2]" "[0,1]" "[2]" "[0]" "[2]" "C_x3"
bash "run.sh" "X4" "lsd_x4" "[0,1]" "[0,1]" "[2,3]" "[0,2]" "[1]" "C_x4"
bash "run.sh" "X5" "lsd_x5" "[0,1,2]" "[0,1,0,2]" "[3,4]" "[0]" "[2]" "C_x5"
bash "run.sh" "X6" "lsd_x6" "[0,1,2]" "[0]" "[1,2,3,4,1,5,3,2,1,4,3,5]" "[0,1,2,1,1]" "[1,1,1,2]" "C_x6"
bash "run.sh" "X7" "lsd_x7" "[0,1,2,3]" "[0,1]" "[2,3,4,5,2,6,4,3,2,5,4,6]" "[0,1,1,3]" "[1,2,1]" "C_x7"
bash "run.sh" "X8" "lsd_x8" "[0,1,2,3]" "[0,1]" "[2,3,4,5,2,6,7,3,2,5,4,6,2,3,7,5,2,6,4,3,2,5,7,6]" "[0,1,3,1]" "[2]" "C_x8"
bash "run.sh" "X9" "lsd_x9" "[0,1,2,3]" "[0,1]" "[2,3,4,5,6,7,2,8,4,3,6,5,2,7,4,8,6,3,2,5,4,7,6,8]" "[0,1,2,3]" "[2]" "C_x9"
bash "run.sh" "X10" "lsd_x10" "[0,1,2,3,4]" "[0,1]" "[2,3,4,5,6,7,2,8,4,9,6,3,2,5,4,7,6,8,2,9,4,3,6,5,2,7,4,8,6,9]" "[0,1,4,2]" "[3]" "C_x10"

