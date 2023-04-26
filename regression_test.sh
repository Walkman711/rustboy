#!/bin/bash

# set -eu -o pipefail

mkdir -p logs
rm logs/log_*

for i in {01..11}
do
    timeout 15s cargo run --release -- --rom ~/gb-test-roms/cpu_instrs/individual/"${i}"-* > logs/log_${i}  
    cd ~/gameboy-doctor
    ./gameboy-doctor ~/rustboy/logs/log_${i} cpu_instrs ${i}
    cd -
done
