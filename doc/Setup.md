
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
``` 
sudo apt-get install default-jdk
```
2. Install the [SBT](https://www.scala-sbt.org/release/docs/Installing-sbt-on-Linux.html)
``` 
sudo apt-get update
sudo apt-get install apt-transport-https curl gnupg -yqq
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo -H gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import
sudo chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg
sudo apt-get update
sudo apt-get install sbt
```
You can also install SBT in Windows


## Setup Verilator and GTKWave
The [Icarus Verilog](http://iverilog.icarus.com/) has been removed, bucause it's too slow. The Simulation is based on Verilator now. Also GTKWave is used to log the waveform. Although *Verilator* can be intalled by APT-GET, I strongly suggest you to compile the newest version by the [steps](https://verilator.org/guide/latest/install.html).

```
#sudo apt-get install git perl python3 make autoconf g++ flex bison ccache
#sudo apt-get install libgoogle-perftools-dev numactl perl-doc
#sudo apt-get install libfl2  # Ubuntu only (ignore if gives error)
#sudo apt-get install libfl-dev  # Ubuntu only (ignore if gives error)
#sudo apt-get install zlibc zlib1g zlib1g-dev  # Ubuntu only (ignore if gives error)

git clone https://github.com/verilator/verilator.git
cd verilator
autoconf
./configure --prefix=/PATH/TO/INSTALL
make -j4096
sudo make install
```

## Compile chisel3 to verilog


```
cd .
make compile
```

The sbt will download the dependency (来自某些地区的赛博残障人士，请自行寻找稳定连接网络的方案，并从**Maven**下载依赖项) and than emits the verilog to `./generated`.

## Compile Model of Rif2Chip

The Verilator will compile the Verilog-files emited by chisel3 and a top wrapper `SimTop.v` into a library `VSimTop__ALL`. Then a main function in `sim_main.cpp` will be built up to simulate the behavior of the Rift2Chip SoC. 

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
