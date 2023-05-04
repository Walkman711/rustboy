#!/bin/bash

# set -eu -o pipefail

mkdir -p logs
rm logs/log_*

# for i in {02..02}
for i in {03..11}
do
    cargo run --release -- --rom ../gb-test-roms/cpu_instrs/individual/"${i}"-* > logs/log_${i} &
    sleep 20 && pkill -9 rustboy
    cd ../gameboy-doctor
    ./gameboy-doctor ~/rustboy/logs/log_${i} cpu_instrs ${i}
    cd -
done
