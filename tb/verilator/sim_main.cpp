/*
* @Author: Ruige Lee
* @Date:   2021-08-06 10:14:14
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-09-07 16:37:14
*/


#include <verilated.h>
#include "VSimTop.h"
#include <memory>
#include <iostream>

#include "verilated_vcd_c.h"



vluint64_t main_time = 0;

int main(int argc, char **argv, char **env) {


	// const std::unique_ptr<VerilatedContext> contextp{new VerilatedContext};
	Verilated::commandArgs(argc, argv);


	VSimTop *top = new VSimTop();


	Verilated::traceEverOn(true);


	VerilatedVcdC* tfp = new VerilatedVcdC;


	top->trace(tfp, 99); // Trace 99 levels of hierarchy
	tfp->open("./build/wave.vcd");

	
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

		tfp->dump(Verilated::time());


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
	tfp->close();
	top->final();

	delete top;


	
	return 0;




}








