
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



## Test a single ISA with waveform



## Test all ISA without waveform

