

#include <verilated.h>
#include "VReservation_top.h"
#include <memory>
#include <iostream>
#include <getopt.h>

#include <sstream>

#include <stdlib.h>
#include <time.h>



VReservation_top *top;



#if VM_TRACE
#include "verilated_vcd_c.h"
#endif

char* img;
vluint64_t main_time = 0;

double sc_time_stamp () {
	return main_time;
}




int main(int argc, char **argv, char **env) {

	top = new VReservation_top();
	// srand(time(NULL));

#if VM_TRACE
	VerilatedVcdC* tfp = new VerilatedVcdC;;

	Verilated::traceEverOn(true);
	top->trace(tfp, 99); // Trace 99 levels of hierarchy
	tfp->open("./build/mdl_reservation_fpu.vcd");       


#endif

	
	top->reset = 1;
	top->clock = 0;


	// printf("start diff\n");
	while(!Verilated::gotFinish()) {


		Verilated::timeInc(1);

		if ( main_time != 50 ){
		} else {
			top->reset = 0;
		}

		if ( main_time % 10 == 1 ) {
			top->clock = 1;		
		} else if ( main_time % 10 == 6 ) {
			top->clock = 0;


		} 



		top->eval();

#if VM_TRACE

		tfp->dump(Verilated::time());           


#endif



		main_time ++;
	}
#if VM_TRACE

	tfp->close();       


#endif
	top->final();


	delete top;
	
	return 0;




}





