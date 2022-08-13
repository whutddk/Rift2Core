FROM ubuntu:latest

RUN    apt-get update \
	&& apt-get install -y curl git gnupg2 autoconf automake autotools-dev libmpc-dev libmpfr-dev libgmp-dev libusb-1.0-0-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev device-tree-compiler pkg-config libexpat-dev python3 \
    && mkdir -p /work \
    && mkdir -p /RISCV \
    && export RISCV=/RISCV/ \
    && cd /work \
    && git clone https://github.com/riscv-collab/riscv-gnu-toolchain.git \
    && cd riscv-gnu-toolchain \
    && git submodule update --init --recursive \
    && ./configure --prefix=$RISCV --enable-multilib \
    && make -j128 \
    && make linux -j128 \
    && make clean \
    && ./configure --prefix=$RISCV --with-arch=rv32gc --with-abi=ilp32d \
    && make -j128 \
    && make linux -j128 \
    && cd / \
    && rm -rf ./work \
    && apt-get purge -y --auto-remove curl git gnupg2 autoconf automake autotools-dev libmpc-dev libmpfr-dev libgmp-dev libusb-1.0-0-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev device-tree-compiler pkg-config libexpat-dev python3 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*



