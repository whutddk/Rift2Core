/*
* @Author: Ruige Lee
* @Date:   2021-09-15 15:37:08
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-09-16 12:01:05
*/


#include "riscv_machine.h"



#include <verilated.h>
#include "Vtest.h"
#include <memory>
#include <iostream>
#include <getopt.h>


#if VM_TRACE
#include "verilated_vcd_c.h"
#endif

RISCVMachine *machine;
RISCVCPUState *cpu;
char* img;



// int iterate_core(RISCVMachine *m, int hartid) {
//     if (m->common.maxinsns-- <= 0)
//         /* Succeed after N instructions without failure. */
//         return 0;

//     RISCVCPUState *cpu = m->cpu_state[hartid];

//     /* Instruction that raises exceptions should be marked as such in
//      * the trace of retired instructions.
//      */
//     uint64_t last_pc  = virt_machine_get_pc(m, hartid);
//     // std::cout << "pc=" << last_pc << std::endl;	



//     int      priv     = riscv_get_priv_level(cpu);
//     uint32_t insn_raw = -1;
//     (void)riscv_read_insn(cpu, &insn_raw, last_pc);
//     int keep_going = virt_machine_run(m, hartid);
//     if (last_pc == virt_machine_get_pc(m, hartid))
//         return 0;

//     if (m->common.trace) {
//         --m->common.trace;
//         return keep_going;
//     }

//     ;

//     int iregno = riscv_get_most_recently_written_reg(cpu);
//     int fregno = riscv_get_most_recently_written_fp_reg(cpu);

//     if (cpu->pending_exception != -1)
//         ;
//     else if (iregno > 0)
//         ;
//     else if (fregno >= 0)
//         ;

//     ;

//     return keep_going;
// }












vluint64_t main_time = 0;

double sc_time_stamp () {
	return main_time;
}


uint8_t flag_waveEnable = 0;

int prase_arg(int argc, char **argv) {
	int opt;
	while( -1 != ( opt = getopt( argc, argv, "wf:" ) ) ) {
		switch(opt) {
			case 'w':
				flag_waveEnable = 1;
				std::cout << "Waveform is Enable" << std::endl;
				break;
			case 'f':
				img = strdup(optarg);
				std::cout << "load in image is " << img << std::endl;
				break;
			case '?':
				std::cout << "-w to enable waveform" << std::endl;
				std::cout << "-f FILENAME to testfile" << std::endl;
				return -1;
				break;
			default:
			    std::cout << opt << std::endl;
			    assert(0);
		}
	}
	return 0;
}









int main(int argc, char **argv, char **env) {


	if ( -1 == prase_arg(argc, argv) ) {
		std::cout << "Prase Error." << std::endl;
		return -1;
	}

// 	Verilated::commandArgs(argc, argv);

// 	Vtest *top = new Vtest();

// #if VM_TRACE
// 	VerilatedVcdC* tfp = new VerilatedVcdC;;
// 	if (flag_waveEnable) {
// 		Verilated::traceEverOn(true);
// 		top->trace(tfp, 99); // Trace 99 levels of hierarchy
// 		tfp->open("./build/wave.vcd");		
// 	}

// #endif

	
// 	top->RSTn = 0;
// 	top->CLK = 0;


// 	while(!Verilated::gotFinish()) {
// 		Verilated::timeInc(1);

// 		if ( main_time != 50 ){
// 		} else {
// 			top->RSTn = 1;
// 		}

// 		if ( main_time % 10 == 1 ){
// 			top->CLK = 1;
// 		} else if ( main_time % 10 == 6 ){
// 			top->CLK = 0;
// 		} 

// 		top->eval();

// #if VM_TRACE
// 		if ( flag_waveEnable ) {
// 			tfp->dump(Verilated::time());			
// 		}
// #endif

// 		if ( main_time > 500000 ){
// 			std::cout << "Timeout!" << std::endl;	
// 			break;
// 		} 

// 		// if ( top -> fail == 1 && main_time % 100 == 0 ) {
// 		// 	std::cout << "Fail!!!" << std::endl;	
// 		// 	break;			
// 		// }
// 		// else if ( top -> success == 1 && main_time % 100 == 0 ) {
// 		// 	std::cout << "Pass!" << std::endl;	
// 		// 	break;			
// 		// } 


// 		main_time ++;
// 	}
// #if VM_TRACE
// 	if ( flag_waveEnable ) {
// 		tfp->close();		
// 	}

// #endif
// 	top->final();


std::cout << "dromajo_cosim_init" << std::endl;	


	char * temp[2] = { "dromajo_init", "./ci/rv64ui-p-add" };
	char **argv_temp = temp;

	machine = virt_machine_main(2,  argv_temp );

    if ( machine == NULL ) {return 1;}



    // iterate_core(machine, 0);
    int cnt = 500;
    while(cnt --) {
	    cpu = machine->cpu_state[0];

	    /* Instruction that raises exceptions should be marked as such in
	     * the trace of retired instructions.
	     */
	    uint64_t last_pc  = virt_machine_get_pc(machine, 0);
	    printf("pc=0x%lx \n", last_pc);
	    for ( uint8_t i = 0; i < 32; i++) {
	    	printf("reg %d = 0x%lx   ", i, virt_machine_get_reg(machine, 0, i));
	    }
	    printf("\n");
	    // std::cout << "pc=" << last_pc << std::endl;	

	    int      priv     = riscv_get_priv_level(cpu);

	    virt_machine_run(machine, 0);


	    if (cpu->pending_exception != -1)
	        std::cout << "Exception" << std::endl;	    	
    }





    virt_machine_end(machine);

std::cout << "dromajo_cosim_done" << std::endl;	
	// delete top;

	return 0;

}


