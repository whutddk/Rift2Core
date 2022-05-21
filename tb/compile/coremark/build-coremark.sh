#!/bin/bash

set -e

BASEDIR=$PWD
CM_FOLDER=coremark

cd $BASEDIR/$CM_FOLDER

# run the compile
echo "Start compilation"
make PORT_DIR=../riscv64 compile
mv coremark.riscv ../

make PORT_DIR=../riscv64-baremetal compile
mv coremark.bare.riscv ../coremark1_bare

riscv64-unknown-elf-objdump --disassemble-all --disassemble-zeroes --section=.text --section=.text.startup --section=.text.init --section=.data ../coremark1_bare > ../coremark1_bare.dump
riscv64-unknown-elf-objcopy -O verilog ../coremark1_bare ../coremark1_bare.verilog
sed -i 's/@800/@000/g' ../coremark1_bare.verilog
