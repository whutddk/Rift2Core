/*
* @Author: Ruige Lee
* @Date:   2021-09-16 14:25:51
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-09-23 10:59:41
*/


#include "diff.h"
#include <iostream>
#include "VSimTop.h"


RISCVMachine *machine;
RISCVCPUState *cpu;
struct diff diff;

extern char* img;



void dromajo_step() {
	

  virt_machine_run(machine, 0);
  diff.pc  = virt_machine_get_pc(machine, 0);


	if (cpu->pending_exception != -1) {
      // std::cout << "Exception" << std::endl;
  }

  for ( uint8_t i = 0; i < 32; i++) {
    diff.ireg[i] = virt_machine_get_reg(machine, 0, i);
    // printf("reg %d = 0x%lx   ", i, virt_machine_get_reg(machine, 0, i));
  }


  diff.priv = riscv_get_priv_level(cpu);
  diff.mstatus = riscv_cpu_get_mstatus(cpu);
  diff.mcause = cpu->mcause;
  diff.mtval = cpu -> mtval;
  diff.mtvec = cpu -> mtvec;
  diff.mepc = cpu -> mepc;

  printf("pc is %lx, t0 is %lx\n", diff.pc, diff.ireg[5]);

}


int dromajo_init() {
	char * temp[4];
	temp[0] = "dromajo_init";
	
  temp[1] = "--reset_vector";
  temp[2] = "0x80000000";
  temp[3] = img;


	char **argv_temp = temp;

	machine = virt_machine_main(4,  argv_temp );
  cpu = machine->cpu_state[0];
    if ( machine == NULL ) {
    	std::cout << "DROMAJO Init Failed!!!" << std::endl;;
      return -1;
    } else {
    	// std::cout << "DROMAJO Init Success!!!" << std::endl;  	
    }
    return 0;
}

void dromajo_deinit() {
	virt_machine_end(machine);


}



int diff_chk_pc(VSimTop *top) {
  // printf("check\n");


  static uint64_t last_pc;

  if ( top->trace_comfirm_0 && top->trace_comfirm_1) {
    last_pc = top->trace_pc_1;
  } else if ( top->trace_comfirm_0 || top->trace_abort_0 ) {
    last_pc = top->trace_pc_0;
  } else {
    ;
  }

  if (diff.pc != last_pc  ) { printf( "Failed at pc, real is 0x%lx, should be 0x%lx\n", last_pc , diff.pc ); return -1; }

  return 0;
}

int diff_chk_reg(VSimTop *top) {
  // printf("check\n");

 


  // if (diff.ireg[0]  != top->trace_abi_zero) { printf( "Failed at zero, real is 0x%lx, should be 0x%lx\n", top->trace_abi_zero , diff.ireg[0] ); return -1; }
  if (diff.ireg[1]  != top->trace_abi_ra  ) { printf( "Failed at ra, real is 0x%lx, should be 0x%lx\n", top->trace_abi_ra , diff.ireg[1] ); return -1; }
  if (diff.ireg[2]  != top->trace_abi_sp  ) { printf( "Failed at sp, real is 0x%lx, should be 0x%lx\n", top->trace_abi_sp , diff.ireg[2] ); return -1; }
  if (diff.ireg[3]  != top->trace_abi_gp  ) { printf( "Failed at gp, real is 0x%lx, should be 0x%lx\n", top->trace_abi_gp , diff.ireg[3] ); return -1; }
  if (diff.ireg[4]  != top->trace_abi_tp  ) { printf( "Failed at tp, real is 0x%lx, should be 0x%lx\n", top->trace_abi_tp , diff.ireg[4] ); return -1; }
  if (diff.ireg[5]  != top->trace_abi_t0  ) { printf( "Failed at t0, real is 0x%lx, should be 0x%lx\n", top->trace_abi_t0 , diff.ireg[5] ); return -1; }
  if (diff.ireg[6]  != top->trace_abi_t1  ) { printf( "Failed at t1, real is 0x%lx, should be 0x%lx\n", top->trace_abi_t1 , diff.ireg[6] ); return -1; }
  if (diff.ireg[7]  != top->trace_abi_t2  ) { printf( "Failed at t2, real is 0x%lx, should be 0x%lx\n", top->trace_abi_t2 , diff.ireg[7] ); return -1; }
  if (diff.ireg[8]  != top->trace_abi_s0  ) { printf( "Failed at s0, real is 0x%lx, should be 0x%lx\n", top->trace_abi_s0 , diff.ireg[8] ); return -1; }
  if (diff.ireg[9]  != top->trace_abi_s1  ) { printf( "Failed at s1, real is 0x%lx, should be 0x%lx\n", top->trace_abi_s1 , diff.ireg[9] ); return -1; }
  if (diff.ireg[10] != top->trace_abi_a0  ) { printf( "Failed at a0, real is 0x%lx, should be 0x%lx\n", top->trace_abi_a0 , diff.ireg[10] ); return -1; }
  if (diff.ireg[11] != top->trace_abi_a1  ) { printf( "Failed at a1, real is 0x%lx, should be 0x%lx\n", top->trace_abi_a1 , diff.ireg[11] ); return -1; }
  if (diff.ireg[12] != top->trace_abi_a2  ) { printf( "Failed at a2, real is 0x%lx, should be 0x%lx\n", top->trace_abi_a2 , diff.ireg[12] ); return -1; }
  if (diff.ireg[13] != top->trace_abi_a3  ) { printf( "Failed at a3, real is 0x%lx, should be 0x%lx\n", top->trace_abi_a3 , diff.ireg[13] ); return -1; }
  if (diff.ireg[14] != top->trace_abi_a4  ) { printf( "Failed at a4, real is 0x%lx, should be 0x%lx\n", top->trace_abi_a4 , diff.ireg[14] ); return -1; }
  if (diff.ireg[15] != top->trace_abi_a5  ) { printf( "Failed at a5, real is 0x%lx, should be 0x%lx\n", top->trace_abi_a5 , diff.ireg[15] ); return -1; }
  if (diff.ireg[16] != top->trace_abi_a6  ) { printf( "Failed at a6, real is 0x%lx, should be 0x%lx\n", top->trace_abi_a6 , diff.ireg[16] ); return -1; }
  if (diff.ireg[17] != top->trace_abi_a7  ) { printf( "Failed at a7, real is 0x%lx, should be 0x%lx\n", top->trace_abi_a7 , diff.ireg[17] ); return -1; }
  if (diff.ireg[18] != top->trace_abi_s2  ) { printf( "Failed at s2, real is 0x%lx, should be 0x%lx\n", top->trace_abi_s2 , diff.ireg[18] ); return -1; }
  if (diff.ireg[19] != top->trace_abi_s3  ) { printf( "Failed at s3, real is 0x%lx, should be 0x%lx\n", top->trace_abi_s3 , diff.ireg[19] ); return -1; }
  if (diff.ireg[20] != top->trace_abi_s4  ) { printf( "Failed at s4, real is 0x%lx, should be 0x%lx\n", top->trace_abi_s4 , diff.ireg[20] ); return -1; }
  if (diff.ireg[21] != top->trace_abi_s5  ) { printf( "Failed at s5, real is 0x%lx, should be 0x%lx\n", top->trace_abi_s5 , diff.ireg[21] ); return -1; }
  if (diff.ireg[22] != top->trace_abi_s6  ) { printf( "Failed at s6, real is 0x%lx, should be 0x%lx\n", top->trace_abi_s6 , diff.ireg[22] ); return -1; }
  if (diff.ireg[23] != top->trace_abi_s7  ) { printf( "Failed at s7, real is 0x%lx, should be 0x%lx\n", top->trace_abi_s7 , diff.ireg[23] ); return -1; }
  if (diff.ireg[24] != top->trace_abi_s8  ) { printf( "Failed at s8, real is 0x%lx, should be 0x%lx\n", top->trace_abi_s8 , diff.ireg[24] ); return -1; }
  if (diff.ireg[25] != top->trace_abi_s9  ) { printf( "Failed at s9, real is 0x%lx, should be 0x%lx\n", top->trace_abi_s9 , diff.ireg[25] ); return -1; }
  if (diff.ireg[26] != top->trace_abi_s10 ) { printf( "Failed at s10, real is 0x%lx, should be 0x%lx\n", top->trace_abi_s10 , diff.ireg[26] ); return -1; }
  if (diff.ireg[27] != top->trace_abi_s11 ) { printf( "Failed at s11, real is 0x%lx, should be 0x%lx\n", top->trace_abi_s11 , diff.ireg[27] ); return -1; }
  if (diff.ireg[28] != top->trace_abi_t3  ) { printf( "Failed at t3, real is 0x%lx, should be 0x%lx\n", top->trace_abi_t3 , diff.ireg[28] ); return -1; }
  if (diff.ireg[29] != top->trace_abi_t4  ) { printf( "Failed at t4, real is 0x%lx, should be 0x%lx\n", top->trace_abi_t4 , diff.ireg[29] ); return -1; }
  if (diff.ireg[30] != top->trace_abi_t5  ) { printf( "Failed at t5, real is 0x%lx, should be 0x%lx\n", top->trace_abi_t5 , diff.ireg[30] ); return -1; }
  if (diff.ireg[31] != top->trace_abi_t6  ) { printf( "Failed at t6, real is 0x%lx, should be 0x%lx\n", top->trace_abi_t6 , diff.ireg[31] ); return -1; }





	return 0;
}

