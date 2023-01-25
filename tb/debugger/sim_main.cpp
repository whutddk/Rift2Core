/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

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

#include <sstream>



#if VM_TRACE
#include "verilated_fst_c.h"
#endif

#include "remote_bitbang.h"

char* img;
vluint64_t main_time = 0;

double sc_time_stamp () {
	return main_time;
}


uint8_t flag_waveEnable = 0;
uint8_t flag_limitEnable = 0;
uint8_t flag_jtagMode = 0;

int prase_arg(int argc, char **argv) {
	int opt;
	while( -1 != ( opt = getopt( argc, argv, "jldwf:" ) ) ) {
		switch(opt) {
			case 'l':
			flag_limitEnable = 1;
			break;
			break;
			case 'w':
				flag_waveEnable = 1;
				std::cout << "Waveform is Enable" << std::endl;
				break;
			case 'f':
				img = strdup(optarg);
				// std::cout << "load in image is " << img << std::endl;
				break;
			case 'j':
				flag_jtagMode = 1;
				std::cout << "jtag Mode is Enable, bypassing memory load" << std::endl;
				break;
			case '?':
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







extern remote_bitbang_t * jtag;

int main(int argc, char **argv, char **env) {


	if ( -1 == prase_arg(argc, argv) ) {
		std::cout << "Prase Error." << std::endl;
		return -1;
	}

	jtag = new remote_bitbang_t(16666);

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
	VerilatedFstC* tfp = new VerilatedFstC;
	if (flag_waveEnable) {
		Verilated::traceEverOn(true);
		top->trace(tfp, 5); // Trace 99 levels of hierarchy
		tfp->open("./generated/build/wave.fst");		
	}

#endif

	
	top->RSTn = 0;
	top->CLK = 0;

	// printf("start diff\n");
	while(!Verilated::gotFinish()) {
		static uint8_t flag_chk = 0;

		Verilated::timeInc(1);

		if ( main_time != 50 ){
		} else {
			top->RSTn = 1;
		}

		if ( main_time % 10 == 1 ) {
			top->CLK = 1;
		} else if ( main_time % 10 == 6 ) {
			top->CLK = 0;

		} 








		top->eval();

#if VM_TRACE
		if ( flag_waveEnable ) {
			tfp->dump(Verilated::time());			
		}

#endif

		if ( flag_limitEnable ) {
			if ( main_time > 5000000 ){
				std::cout << "Timeout!!!!!" << std::endl;	
				break;
			} 			
		}

		if ( top -> fail == 1 && main_time % 100 == 0 ) {
			std::cout << "Fail!!!!!!" << std::endl;	
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


	delete top;
	
	return 0;




}








