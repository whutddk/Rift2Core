
# How to Setup

-----------------

## Setup Repo
There are a few dependencies of this repo. Either for generating the circuits or for simulation. As a result, clone all of them, if you don't know how these repo works.

```
git clone https://github.com/whutddk/Rift2Core.git
cd Rift2Core
git submodule update --init --recursive
```


## Setup SBT
As a lib of scala, [Chisel3](https://github.com/chipsalliance/chisel3) can compile under *the Scala Build Tool* (SBT).
1. Java is needed
``` sudo apt-get install default-jdk
```
2. Install the SBT
``` sudo apt-get install sbt
```
You can also install SBT in Windows


## Setup Verilator and GTKWave
The [Icarus Verilog](http://iverilog.icarus.com/) has been removed, bucause it's too slow. The Simulation is based on Verilator now. Also GTKWave is used to log the waveform. Although *Verilator* can be intalled by APT-GET, I strongly suggest you to compile the newest version by the [steps](https://verilator.org/guide/latest/install.html).

```
git clone https://github.com/verilator/verilator.git
cd verilator
autoconf
./configure
make -j4096
sudo make install
```

## Compile chisel3 to verilog


```
cd .
make compile
```

The sbt will download the dependency (来自某些地区的赛博残障人士，请自行寻找稳定连接网络，并从**Maven**下载依赖项的方案) and than emits the verilog to `./generated`.

## Compile Model of Rif2Chip

The Verilator will compile the Verilog-files emited by chisel3 and a top wrapper `SimTop.v` into a library "VSimTop__ALL". Then a main function in `sim_main.cpp` will be built up to simulate the behavior of the Rift2Chip SoC. 

```
export R2=/PATH/TO/Rift2Core
cd ./tb
make sim 
```

We will get an executable file `./tb/build/VSimTop`.

## Test a single ISA with waveform

Make sure your executable riscv files are placed in `./tb/ci`.

```
cd ./tb
make single TESTFILE=./ci/FILENAME
```

the FILENAME will be loaded for diff-test with Dromajo. The FILENAME.verilog will be loaded into the memory of Rift2Chip

To check the waveform?
```
make wave
```


## Test all ISA without waveform

Make sure your executable riscv files are placed in `./tb/ci`.

```
cd ./tb
make unit
```
