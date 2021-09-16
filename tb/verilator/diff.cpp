/*
* @Author: Ruige Lee
* @Date:   2021-09-16 14:25:51
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-09-16 16:07:46
*/


#include "diff.h"
#include <iostream>



RISCVMachine *machine;
RISCVCPUState *cpu;
struct diff diff;

extern char* img;

void dromajo_init() {
	char * temp[2];
	temp[0] = "dromajo_init";
	
	temp[1] = img;

	char **argv_temp = temp;

	machine = virt_machine_main(2,  argv_temp );

    if ( machine == NULL ) {
    	std::cout << "DROMAJO Init Failed!!!" << std::endl;;
    } else {
    	std::cout << "DROMAJO Init Success!!!" << std::endl;
		virt_machine_run(machine, 0);
		virt_machine_run(machine, 0);
		virt_machine_run(machine, 0);
		virt_machine_run(machine, 0);
		virt_machine_run(machine, 0);
		virt_machine_run(machine, 0);
		virt_machine_run(machine, 0);
		virt_machine_run(machine, 0);
		virt_machine_run(machine, 0);
		virt_machine_run(machine, 0);    	
    }

}

void dromajo_step() {
	cpu = machine->cpu_state[0];

	diff.pc  = virt_machine_get_pc(machine, 0);

	// printf("pc=0x%lx \n", last_pc);
	for ( uint8_t i = 0; i < 32; i++) {
		diff.ireg[i] = virt_machine_get_reg(machine, 0, i);
		// printf("reg %d = 0x%lx   ", i, virt_machine_get_reg(machine, 0, i));
	}
	// printf("\n");
	// std::cout << "pc=" << last_pc << std::endl;

	diff.priv = riscv_get_priv_level(cpu);


	diff.mstatus = riscv_cpu_get_mstatus(cpu);
	diff.mcause = cpu->mcause;
	diff.mtval = cpu -> mtval;
	diff.mtvec = cpu -> mtvec;
	diff.mepc = cpu -> mepc;


	printf("mstatus=%lx ",diff.mstatus);
	printf("mcause=%lx ",diff.mcause);
	printf("mtval=%lx ",diff.mtval);
	printf("mtvec=%lx ",diff.mtvec);
	printf("mepc=%lx ",diff.mepc);
	printf("\n");

	virt_machine_run(machine, 0);


	if (cpu->pending_exception != -1)
	std::cout << "Exception" << std::endl;
}

void dromajo_deinit() {
	virt_machine_end(machine);
}

