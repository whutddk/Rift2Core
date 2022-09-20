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


#include <verilated.h>
#include "VSimTop.h"
#include <memory>
#include <iostream>
#include <getopt.h>
#include "diff.h"

#include <sstream>



#if VM_TRACE
#include "verilated_fst_c.h"
#endif


char* img;
VSimTop *top;
#if VM_TRACE
VerilatedFstC* tfp;
#endif
vluint64_t main_time = 0;

double sc_time_stamp () {
	return main_time;
}


uint8_t flag_waveEnable = 0;
uint8_t flag_diffEnable = 0;
uint8_t flag_limitEnable = 0;
uint8_t flag_perform = 0;

int prase_arg(int argc, char **argv) {
	int opt;
	while( -1 != ( opt = getopt( argc, argv, "pjldwf:" ) ) ) {
		switch(opt) {
			case 'p':
			flag_perform = 1;
			break;
			case 'l':
			flag_limitEnable = 1;
			break;
			case 'd':
			flag_diffEnable = 1;
			break;
			case 'w':
				flag_waveEnable = 1;
				std::cout << "Waveform is Enable" << std::endl;
				break;
			case 'f':
				img = strdup(optarg);
				// std::cout << "load in image is " << img << std::endl;
				break;
			case '?':
				std::cout << "-p to enable performmace logger" << std::endl;
				std::cout << "-w to enable waveform" << std::endl;
				std::cout << "-f FILENAME to testfile" << std::endl;
				std::cout << "-j to enable jtag mode" << std::endl;
				return -1;
				break;
			default:
				std::cout << opt << std::endl;
				assert(0);
		}
	}
	return 0;
}

static void sim_exit(){
#if VM_TRACE
	if ( flag_waveEnable ) { tfp->close(); }
#endif

	top->final();

	if ( flag_diffEnable ) { dromajo_deinit(); }

	delete top;
}






int main(int argc, char **argv, char **env) {


	if ( -1 == prase_arg(argc, argv) ) {
		std::cout << "Prase Error." << std::endl;
		return -1;
	}

	if ( flag_diffEnable ) {
		if ( -1 == dromajo_init() ) {
			return -1;
		}		
	}

	char * temp[2];
	char cmd[64] = "+";
	strcat(cmd, img);
	strcat(cmd, ".verilog");
	temp[0] = "Verilated";
	temp[1] = cmd;
	char **argv_temp = temp;
	Verilated::commandArgs(2, argv_temp);		



	top = new VSimTop();

#if VM_TRACE
	tfp = new VerilatedFstC;
	if (flag_waveEnable) {
		Verilated::traceEverOn(true);
		top->trace(tfp, 99); // Trace 99 levels of hierarchy
		tfp->open("./generated/build/wave.fst");		
	}

#endif

	
	top->RSTn = 0;
	top->CLK = 0;

	// printf("start diff\n");
	while(!Verilated::gotFinish()) {
		static uint8_t flag_chk = 0;

		Verilated::timeInc(1);

		if ( main_time != 100 ){
		} else {
			top->RSTn = 1;
		}

		if ( main_time % 10 == 1 ) {
			top->CLK = 1;
		} else if ( main_time % 10 == 6 ) {
			top->CLK = 0;

			if ( flag_diffEnable ) {
				if ( flag_chk ) {
					if ( -1 == diff_chk_reg(top) ) {
						printf("failed at dromajo pc = 0x%lx\n", diff.pc);
						sim_exit();
						return -1;
					}
					flag_chk = 0;
				}
			}

			if ( top->trace_comfirm_0 && (top->trace_comfirm_1 || top->trace_abort_1) ) {
				// printf("real pc = %lx, real t0 = %lx\n", top->trace_pc_1, top->trace_abi_t0);
				// printf("    Commit2: \n");
				if ( flag_diffEnable ) {
					if ( -1 == diff_chk_pc(top) ) {
						printf("failed at dromajo pc = 0x%lx\n", diff.pc);
						sim_exit();
						return -1;
					}

					dromajo_step();
					dromajo_step();
					flag_chk = 1;					
				}


			} else if ( top->trace_comfirm_0 || top->trace_abort_0 ) {
				// printf("real pc = %lx, real t0 = %lx\n", top->trace_pc_0, top->trace_abi_t0);
				// printf("    Commit1: \n");
				if ( flag_diffEnable ) {
					if ( -1 == diff_chk_pc(top) ) {
						printf("failed at dromajo pc = 0x%lx\n", diff.pc);
						sim_exit();
						return -1;
					}

					dromajo_step();
					flag_chk = 1;					
				}

			} else { ; }

		} 

		if ( main_time % 500 == 250 ) {top->rtc_clock = 1;}
		else if ( main_time % 500 == 0 ) { top->rtc_clock = 0;}

		// if ( main_time % 24 == 12 ) {top->io_hspi_clk = 1;}
		// else if ( main_time % 24 == 1 ) { top->io_hspi_clk = 0;}
		// if ( main_time == 0xffff ) {
		// 	top->io_interrupt_0 = 1;
		// 	top->io_interrupt_1 = 0;
		// 	top->io_interrupt_2 = 1;
		// }
		// if ( main_time == 0x1000f ) {
		// 	top->io_interrupt_0 = 0;
		// 	top->io_interrupt_1 = 0;
		// 	top->io_interrupt_2 = 0;
		// }

		top->eval();

#if VM_TRACE
		if ( flag_waveEnable ) {
			tfp->dump(Verilated::time());			
		}

#endif

		if ( flag_limitEnable ) {
			if ( main_time > 15000000 ){
				std::cout << "Timeout!!!!!" << std::endl;	
				sim_exit();
				return -1;
			} 			
		}

		if ( top -> fail == 1 && main_time % 100 == 0 ) {
			std::cout << "Fail!!!!!!" << std::endl;	
			sim_exit();
			return -1;
		}
		else if ( top -> success == 1 && main_time % 100 == 0 ) {
			std::cout << "Pass!" << std::endl;
			if ( flag_perform ) {
				std::cout << "Simulation-Cycle(not equal to cpu-cycle) is:" << top ->trace_mcycle << std::endl;
				std::cout << "Retired-instruction is: " << top -> trace_minstret << std::endl;

				std::cout << "Branch:" << std::endl;				
				std::cout << "Branch-instruction is: " << top -> trace_scsBPredict + top -> trace_misBPredict << std::endl;
				std::cout << top -> trace_scsBPredict << " (" << (float)(top -> trace_scsBPredict) / (top -> trace_scsBPredict + top -> trace_misBPredict) * 100. << "%) success" << std::endl;
				std::cout << top -> trace_misBPredict << " (" << (float)(top -> trace_misBPredict) / (top -> trace_scsBPredict + top -> trace_misBPredict) * 100. << "%) failed"  << std::endl;

				std::cout << "Jalr:" << std::endl;
				std::cout << "Jalr-instruction is: "   << top -> trace_scsJPredict + top -> trace_misJPredict << std::endl;
				std::cout << top -> trace_scsJPredict << " (" << (float)(top -> trace_scsJPredict) / (top -> trace_scsJPredict + top -> trace_misJPredict) * 100. << "%) success" << std::endl;
				std::cout << top -> trace_misJPredict << " (" << (float)(top -> trace_misJPredict) / (top -> trace_scsJPredict + top -> trace_misJPredict) * 100. << "%) failed"  << std::endl;
			}
			sim_exit();
			return 0;			
		} 


		main_time ++;
	}

	
	sim_exit();
	return -1;


}








