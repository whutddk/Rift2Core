/*
* @Author: Ruige Lee
* @Date:   2021-08-06 10:14:14
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-09-22 20:00:24
*/


#include <verilated.h>
#include "VSimTop.h"
#include <memory>
#include <iostream>
#include <getopt.h>
#include "diff.h"

#include <sstream>



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
				// std::cout << "load in image is " << img << std::endl;
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

	if ( -1 == dromajo_init() ) {
		return -1;
	}

	
	char * temp[2];
	char cmd[64] = "+";
	strcat(cmd, img);
	strcat(cmd, ".verilog");
	temp[0] = "Verilated";
	temp[1] = cmd;
	char **argv_temp = temp;
	Verilated::commandArgs(2, argv_temp);

	VSimTop *top = new VSimTop();

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

	// printf("start diff\n");
	while(!Verilated::gotFinish()) {
		static uint8_t flag_pendingChk = 0;
		static uint8_t flag_align = 0;

		Verilated::timeInc(1);

		if ( main_time != 50 ){
		} else {
			top->RSTn = 1;
		}

		if ( main_time % 10 == 1 ){
			top->CLK = 1;
		} else if ( main_time % 10 == 2 ){
			if ( flag_pendingChk ) {

				flag_pendingChk = 0;
				if ( -1 == diff_chk_reg(top) ) {
					printf("failed at dromajo pc = 0x%lx\n", diff.pc);
					break;
				}
			}

		} else if ( main_time % 10 == 6 ){
			top->CLK = 0;




			if ( flag_align ) {
				if ( top->trace_comfirm_0 && top->trace_comfirm_1) {


					dromajo_step();
					dromajo_step();
					flag_pendingChk = 1;

					if ( -1 == diff_chk_pc(top) ) {
						printf("failed at dromajo pc = 0x%lx\n", diff.pc);
						break;
					}


				} else if ( top->trace_comfirm_0 || top->trace_abort_0 ) {

					dromajo_step();
					flag_pendingChk = 1;
					if ( -1 == diff_chk_pc(top) ) {
						printf("failed at dromajo pc = 0x%lx\n", diff.pc);
						break;
					}

				} else {
				    ;
				}
			} else {
				if( top->trace_comfirm_0 && top->trace_pc_0 == 0x80000000 ) {
					if ( -1 == diff_chk_pc(top) ) {
						printf("failed at dromajo pc = 0x%lx\n", diff.pc);
						break;
					}
					// printf("is_align");
					flag_align = 1;
				}
			}






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

		if ( top -> fail == 1 && main_time % 100 == 0 ) {
			std::cout << "Fail!!!" << std::endl;	
			break;			
		}
		else if ( top -> success == 1 && main_time % 100 == 0 ) {
			std::cout << "Pass!" << std::endl;	
			break;			
		} 


		main_time ++;
	}
#if VM_TRACE
	if ( flag_waveEnable ) {
		tfp->close();		
	}

#endif
	top->final();
	dromajo_deinit();

	delete top;


	
	return 0;




}








