FROM whutddk/rift2env:dromajo

RUN    apt-get update
    && apt-get install -y git perl python3 make autoconf g++ flex bison ccache libgoogle-perftools-dev numactl perl-doc libfl2 libfl-dev     
    && mkdir -p /work     
    && cd /work     
    && git clone https://github.com/verilator/verilator.git       
    && cd verilator       
    && git checkout v4.224      
    && unset VERILATOR_ROOT     
    && autoconf      
    && ./configure --prefix /RISCV/     
    && make     
    && make install     
    && cd /       
    && rm -rf ./work     
    && apt-get purge -y --auto-remove git perl python3 make autoconf g++ flex bison ccache libgoogle-perftools-dev numactl perl-doc libfl2 libfl-dev     
    && apt-get clean     
    && rm -rf /var/lib/apt/lists/* # buildkit

