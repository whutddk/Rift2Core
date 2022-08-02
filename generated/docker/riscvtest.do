FROM whutddk/riscv-gnu-toolchain

ENV RISCV=/RISCV/ PATH=$PATH:/RISCV/bin:/RISCV/lib

RUN   apt-get update \
	&& apt-get install -y curl git gnupg2 autoconf automake autotools-dev libmpc-dev libmpfr-dev libgmp-dev libusb-1.0-0-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev device-tree-compiler pkg-config libexpat-dev python3 \
	&& mkdir -p /work \

	&& cd /work \
    && git clone https://github.com/riscv-software-src/riscv-tools.git \
    && cd riscv-tools \
    && git submodule update --init --recursive \

    && cd riscv-isa-sim \
    && git checkout master \
    && git pull \
    && mkdir -p build \
    && cd build \
    && ../configure --prefix=$RISCV \
    && make -j128 \
    && make install \

    && cd ../../riscv-openocd \
    && git checkout riscv \
    && git pull \
    && ./bootstrap \
    && ./configure --prefix=$RISCV \
    && make -j128 \
    && make install \


    && cd ../riscv-pk  \
    && git checkout master \
    && git pull \
    && mkdir -p build \
	&& cd build \
	&& ../configure --prefix=$RISCV --host=riscv64-unknown-elf \
	&& make -j128 \
	&& make install \

	&& mkdir -p /test
	&& cd /work \
	&& git clone https://github.com/whutddk/Rift2Core.git \
	&& git clone https://github.com/riscv-boom/riscv-coremark.git \
	&& cd /work/riscv-coremark \
	&& git submodule update --init --recursive \

	&& cd /work/riscv-tools/riscv-tests \
	&& git checkout master \
	&& git pull \
	&& cd env \
	&& git checkout master \
	&& git pull \


	&& cp -r /work/Rift2Core/tb/compile/isa /work/riscv-tools/riscv-tests \
	&& cp -r /work/Rift2Core/tb/compile/env /work/riscv-tools/riscv-tests \
	&& cp -r /work/Rift2Core/tb/compile/benchmark /work/riscv-tools/riscv-tests \
	&& cp -r /work/Rift2Core/tb/compile/coremark /work/riscv-coremark \
	&& cd /work/Rift2Core/tb/compile/isa \
	&& make \
	&& cp *-p-* /test  \
	&& cp *-v-* /test  \

	&& cd /work/riscv-tools/riscv-tests/benchmark \
	&& make \
	&& cp dhrystone500* /test   \

	&& cd /work/riscv-coremark  \
	&& ./build-coremark.sh      \
	&& cp coremark1* /test      \







	&& rm -rf /work \
    && apt-get purge -y --auto-remove curl git gnupg2 autoconf automake autotools-dev libmpc-dev libmpfr-dev libgmp-dev libusb-1.0-0-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev device-tree-compiler pkg-config libexpat-dev python3 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*