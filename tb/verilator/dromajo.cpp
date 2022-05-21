/*
  Copyright (c) 2020 - 2022 Wuhan University of Technology <295054118@whut.edu.cn>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/




#include "diff.h"

#include <verilated.h>
#include "Vtest.h"
#include <memory>
#include <iostream>
#include <getopt.h>


#if VM_TRACE
#include "verilated_vcd_c.h"
#endif


char* img;





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


// std::cout << "dromajo_cosim_init" << std::endl;	



//     // iterate_core(machine, 0);
//     int cnt = 100;
//     while(cnt --) {
// 	    cpu = machine->cpu_state[0];

// 	    /* Instruction that raises exceptions should be marked as such in
// 	     * the trace of retired instructions.
// 	     */
// 	    uint64_t last_pc  = virt_machine_get_pc(machine, 0);
// 	    printf("pc=0x%lx \n", last_pc);
// 	    for ( uint8_t i = 0; i < 32; i++) {
// 	    	// printf("reg %d = 0x%lx   ", i, virt_machine_get_reg(machine, 0, i));
// 	    }
// 	    // printf("\n");
// 	    // std::cout << "pc=" << last_pc << std::endl;	

// 	    int      priv     = riscv_get_priv_level(cpu);

// 		virt_machine_run(machine, 0);


// 	    if (cpu->pending_exception != -1)
// 	        std::cout << "Exception" << std::endl;	    	
//     }





//     virt_machine_end(machine);

// std::cout << "dromajo_cosim_done" << std::endl;	
// 	// delete top;


	dromajo_init();

	int step = 100;
	while(step--) {
		dromajo_step();
	}

dromajo_deinit();
	return 0;

}


