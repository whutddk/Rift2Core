/*
* @Author: Ruige Lee
* @Date:   2021-09-15 15:37:08
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-09-15 20:01:08
*/

#include "dromajo_cosim.h"

#include <verilated.h>
#include "Vtest.h"
#include <memory>
#include <iostream>
#include <getopt.h>


#if VM_TRACE
#include "verilated_vcd_c.h"
#endif


vluint64_t main_time = 0;

double sc_time_stamp () {
	return main_time;
}


uint8_t flag_waveEnable = 0;

int prase_arg(int argc, char **argv) {
	int opt;
	while( -1 != ( opt = getopt( argc, argv, "w" ) ) ) {
		switch(opt) {
			case 'w':
				flag_waveEnable = 1;
				std::cout << "Waveform is Enable" << std::endl;
				break;
			case '?':
				std::cout << "-w to enable waveform" << std::endl;
				std::cout << "+FILENAME to testfile" << std::endl;
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

	Verilated::commandArgs(argc, argv);

	Vtest *top = new Vtest();

#if VM_TRACE
	VerilatedVcdC* tfp = new VerilatedVcdC;;
	if (flag_waveEnable) {
		Verilated::traceEverOn(true);
		top->trace(tfp, 99); // Trace 99 levels of hierarchy
		tfp->open("./build/wave.vcd");		
	}

#endif

	
	top->RSTn = 0;
	top->CLK = 0;


	while(!Verilated::gotFinish()) {
		Verilated::timeInc(1);

		if ( main_time != 50 ){
		} else {
			top->RSTn = 1;
		}

		if ( main_time % 10 == 1 ){
			top->CLK = 1;
		} else if ( main_time % 10 == 6 ){
			top->CLK = 0;
		} 

		top->eval();

#if VM_TRACE
		if ( flag_waveEnable ) {
			tfp->dump(Verilated::time());			
		}
#endif

		if ( main_time > 500000 ){
			std::cout << "Timeout!" << std::endl;	
			break;
		} 

		// if ( top -> fail == 1 && main_time % 100 == 0 ) {
		// 	std::cout << "Fail!!!" << std::endl;	
		// 	break;			
		// }
		// else if ( top -> success == 1 && main_time % 100 == 0 ) {
		// 	std::cout << "Pass!" << std::endl;	
		// 	break;			
		// } 


		main_time ++;
	}
#if VM_TRACE
	if ( flag_waveEnable ) {
		tfp->close();		
	}

#endif
	top->final();


std::cout << "dromajo_cosim_init" << std::endl;	

    dromajo_cosim_state_t *costate = 0;
    costate                        = dromajo_cosim_init(argc, argv);

    if (!costate)
        return 1;

std::cout << "dromajo_cosim_step" << std::endl;	

    while (!dromajo_cosim_step(costate, 0, 0, 0, 0, 0, false)) {
    	RISCVMachine m;
    	m = *(RISCVMachine *)costate;
    	std::cout << m->RISCVCPUState[0]->pc << std::endl;	
    	std::cout << m->RISCVCPUState[0]->reg[0] << std::endl;	
    	std::cout << m->RISCVCPUState[0]->reg[1] << std::endl;	
    }


std::cout << "dromajo_cosim_fini" << std::endl;	
    dromajo_cosim_fini(costate);



	delete top;

	return 0;

}


