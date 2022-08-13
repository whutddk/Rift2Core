FROM whutddk/rift2env:sbt

RUN    apt-get update \
    && apt-get install -y git cmake build-essential \
    && mkdir -p /work \
    && cd /work \
    && git clone https://github.com/chipsalliance/dromajo.git   \
    && cd dromajo   \
    && mkdir -p build  \
    && cd build     \
    && cmake ..     \
    && make         \
    && cp ./libdromajo_cosim.a /RISCV/lib  \
    && cp ./dromajo /RISCV/bin  \
    && cp ./dromajo_cosim_test /RISCV/bin  \
    && mkdir -p /RISCV/include/dromajo     \
    && cp ../include/* /RISCV/include/dromajo/   \
    && cd /   \
    && rm -rf ./work \
    && apt-get purge -y --auto-remove git cmake build-essential \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

