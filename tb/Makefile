# verilator version
verilator      ?= verilator
R2        ?= ${R2}



sim:
	verilator -Wno-fatal  -y ${R2}/generated/ -y ${R2}/tb/ -y ${R2}/tb/vtb/ --cc ${R2}/tb/verilator/SimTop.v  --exe --build ./verilator/sim_main.cpp -j 8


