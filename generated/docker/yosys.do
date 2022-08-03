FROM whutddk/rift2env:verilator
ENV YOSYS=/YOSYS/

RUN    apt-get update \
    && apt-get install -y wget \
	&& cd /  \
	&& wget https://github.com/YosysHQ/oss-cad-suite-build/releases/download/2022-08-02/oss-cad-suite-linux-x64-20220802.tgz \
	&& tar -zxvf oss-cad-suite-linux-x64-20220802.tgz  \
	&& mv oss-cad-suite YOSYS  \
	&& rm oss-cad-suite-linux-x64-20220802.tgz  \
	&& cd /YOSYS  \
	&& wget http://www.vlsitechnology.org/synopsys/vsclib013.lib  \
	&& apt-get purge -y --auto-remove wget \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \