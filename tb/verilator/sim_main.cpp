/*
* @Author: Ruige Lee
* @Date:   2021-08-06 10:14:14
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-09-07 15:13:57
*/


#include <verilated.h>
#include "Vtest.h"
#include <memory>
#include <iostream>

#include "verilated_vcd_c.h"



vluint64_t main_time = 0;
Vtest *top;
int main(int argc, char **argv, char **env) {
	Verilated::commandArgs(argc, argv);









	std::cout << argc << std::endl;
	std::cout << argv[0] << std::endl;
	std::cout << argv[1] << std::endl;

	std::cout << "Hello World" << std::endl;
	// const std::unique_ptr<VerilatedContext> contextp{new VerilatedContext};
    // contextp->commandArgs(argc, argv);

	// char* arg[] = { "abcd\n", "efag\n", "ab23\n", "efag\n" };
 //    char **argv1  = &arg[0];
	top = new Vtest();



    



	Verilated::traceEverOn(true);


	VerilatedVcdC* tfp = new VerilatedVcdC;


	top->trace(tfp, 99); // Trace 99 levels of hierarchy
	tfp->open("./build/wave.vcd");

	


	top->RSTn = 0;
	top->CLK = 0;


	while(1) {
		// contextp->timeInc(1);

		if ( main_time > 50 ){
			top->RSTn = 1;
		}
		if ( main_time % 10 == 1 ){
			top->CLK = 1;
		}
		if ( main_time % 10 == 6 ){
			top->CLK = 0;
		} 



		top->eval();

		// tfp->dump(contextp->time());


		if ( main_time > 50000 ){
			break;
		} 
		main_time ++;
	}
	tfp->close();
	top->final();

	delete top;


	std::cout << "End!" << std::endl;	
	return 0;




}








