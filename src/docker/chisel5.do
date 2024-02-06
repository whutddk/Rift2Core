FROM whutddk/rift2env:riscvtest

ENV RISCV=/RISCV/ PATH=$PATH:/RISCV/bin:/RISCV/lib:$YOSYS/bin:/firtool-1.59.0/bin

RUN    apt-get update \
	&& apt-get install -y curl wget zip\
	&& cd /usr/local/bin \
    && curl -L https://raw.githubusercontent.com/lefou/millw/0.4.11/millw > mill && chmod +x mill \
    && cd / \
    && wget https://github.com/llvm/circt/releases/download/firtool-1.59.0/circt-full-shared-linux-x64.tar.gz \
    && tar -zxvf circt-full-shared-linux-x64.tar.gz \
    && rm circt-full-shared-linux-x64.tar.gz \
    && apt-get purge -y --auto-remove curl wget \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
