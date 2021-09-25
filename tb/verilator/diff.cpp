/*
* @Author: Ruige Lee
* @Date:   2021-09-16 14:25:51
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-09-25 10:22:49
*/


#include "diff.h"
#include <iostream>
#include "VSimTop.h"


RISCVMachine *machine;
RISCVCPUState *cpu;
struct diff diff;

extern char* img;

#define CHK_REG( NAME, DIFF, TRACE ) \
	if ( DIFF != TRACE ) { printf( "Failed at %s, real is b\'%lx, should be 0x%lx\n", NAME, TRACE, DIFF ); return -1; }


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
	diff.mtvec = cpu -> mtvec;
	diff.mscratch = cpu -> mscratch;
	diff.mepc = cpu -> mepc;
	diff.mcause = cpu->mcause;
	diff.mtval = cpu -> mtval;

    diff.mvendorid            = cpu -> mvendorid;
    diff.marchid              = cpu -> marchid;
    diff.mimpid               = cpu -> mimpid;
    diff.mhartid              = cpu -> mhartid;
    diff.misa                 = cpu -> misa;
    diff.mie                  = cpu -> mie;
    diff.mip                  = cpu -> mip;
    diff.medeleg              = cpu -> medeleg;
    diff.mideleg              = cpu -> mideleg;
    // diff.mcounteren           = cpu -> mcounteren;
    // diff.mcountinhibit        = cpu -> mcountinhibit;
    // diff.tselect              = cpu -> tselect;
    // diff.tdata1[MAX_TRIGGERS] = cpu -> tdata1[MAX_TRIGGERS];
    // diff.tdata2[MAX_TRIGGERS] = cpu -> tdata2[MAX_TRIGGERS];
    // diff.tdata3[MAX_TRIGGERS] = cpu -> tdata3[MAX_TRIGGERS];
    // diff.mhpmevent[32] = cpu -> mhpmevent[32];

    for ( uint8_t i = 0; i < 4; i += 2 ) {
        diff.pmpcfg[i] = cpu -> csr_pmpcfg[i];  // But only 0 and 2 are valid    	
    }

    for ( uint8_t i = 0; i < 16; i++ ) {
	    diff.pmpaddr[i] = cpu -> csr_pmpaddr[i];
	}

    diff.stvec = cpu -> stvec;
    diff.sscratch = cpu -> sscratch;
    diff.sepc = cpu -> sepc;
    diff.scause = cpu -> scause;
    diff.stval = cpu -> stval;
    diff.satp = cpu -> satp;
    // diff.scounteren = cpu -> scounteren;

    // diff.dcsr = cpu -> dcsr;
    // diff.dpc = cpu -> dpc;
    // diff.dscratch = cpu -> dscratch;
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
		diff.pc  = virt_machine_get_pc(machine, 0);
	return 0;
}

void dromajo_deinit() {
	virt_machine_end(machine);
}



int diff_chk_pc(VSimTop *top) {
	// printf("check\n");

	static uint64_t last_pc;

	if ( top->trace_comfirm_0 || top->trace_abort_0 ) {
		last_pc = top->trace_pc_0;
	}

	if (diff.pc != last_pc  ) { printf( "Failed at pc, real is 0x%lx, should be 0x%lx\n", last_pc , diff.pc ); return -1; }

	return 0;
}

int diff_chk_reg(VSimTop *top) {
	// printf( "mstatus = %lx\n", diff.mstatus );

	// if (diff.ireg[0]  != top->trace_abi_zero) { printf( "Failed at zero, real is 0x%lx, should be 0x%lx\n", top->trace_abi_zero , diff.ireg[0] ); return -1; }
	CHK_REG( "ra", diff.ireg[1] , top->trace_abi_ra  )
	CHK_REG( "sp", diff.ireg[2] , top->trace_abi_sp  )
	CHK_REG( "gp", diff.ireg[3] , top->trace_abi_gp  )
	CHK_REG( "tp", diff.ireg[4] , top->trace_abi_tp  )
	CHK_REG( "t0", diff.ireg[5] , top->trace_abi_t0  )
	CHK_REG( "t1", diff.ireg[6] , top->trace_abi_t1  )
	CHK_REG( "t2", diff.ireg[7] , top->trace_abi_t2  )
	CHK_REG( "s0", diff.ireg[8] , top->trace_abi_s0  )
	CHK_REG( "s1", diff.ireg[9] , top->trace_abi_s1  )
	CHK_REG( "a0", diff.ireg[10], top->trace_abi_a0  )
	CHK_REG( "a1", diff.ireg[11], top->trace_abi_a1  )
	CHK_REG( "a2", diff.ireg[12], top->trace_abi_a2  )
	CHK_REG( "a3", diff.ireg[13], top->trace_abi_a3  )
	CHK_REG( "a4", diff.ireg[14], top->trace_abi_a4  )
	CHK_REG( "a5", diff.ireg[15], top->trace_abi_a5  )
	CHK_REG( "a6", diff.ireg[16], top->trace_abi_a6  )
	CHK_REG( "a7", diff.ireg[17], top->trace_abi_a7  )
	CHK_REG( "s2", diff.ireg[18], top->trace_abi_s2  )
	CHK_REG( "s3", diff.ireg[19], top->trace_abi_s3  )
	CHK_REG( "s4", diff.ireg[20], top->trace_abi_s4  )
	CHK_REG( "s5", diff.ireg[21], top->trace_abi_s5  )
	CHK_REG( "s6", diff.ireg[22], top->trace_abi_s6  )
	CHK_REG( "s7", diff.ireg[23], top->trace_abi_s7  )
	CHK_REG( "s8", diff.ireg[24], top->trace_abi_s8  )
	CHK_REG( "s9", diff.ireg[25], top->trace_abi_s9  )
	CHK_REG( "s10", diff.ireg[26], top->trace_abi_s10 )
	CHK_REG( "s11", diff.ireg[27], top->trace_abi_s11 )
	CHK_REG( "t3", diff.ireg[28], top->trace_abi_t3  )
	CHK_REG( "t4", diff.ireg[29], top->trace_abi_t4  )
	CHK_REG( "t5", diff.ireg[30], top->trace_abi_t5  )
	CHK_REG( "t6", diff.ireg[31], top->trace_abi_t6  )


	CHK_REG( "mstatus", diff.mstatus,     top->trace_mstatus  )
	CHK_REG( "mtvec", diff.mtvec,         top->trace_mtvec    )
	CHK_REG( "mscratch", diff.mscratch,   top->trace_mscratch )
	// CHK_REG( "mepc", diff.mepc,           top->trace_mepc     )
	CHK_REG( "mcause", diff.mcause,       top->trace_mcause   )
	// CHK_REG( "mtval", diff.mtval,         top->trace_mtval    )
    // CHK_REG( "mvendorid", diff.mvendorid, top->trace_mvendorid)
    // CHK_REG( "marchid", diff.marchid,     top->trace_marchid  )
    // CHK_REG( "mimpid", diff.mimpid,       top->trace_mimpid   )
    CHK_REG( "mhartid", diff.mhartid,     top->trace_mhartid  )
    // CHK_REG( "misa", diff.misa,           top->trace_misa     )
    CHK_REG( "mie", diff.mie,             top->trace_mie      )
    // CHK_REG( "mip", diff.mip,             top->trace_mip      )
    CHK_REG( "medeleg", diff.medeleg,     top->trace_medeleg  )
    CHK_REG( "mideleg", diff.mideleg,     top->trace_mideleg  )
    // CHK_REG( "mcounteren", diff.mcounteren,       top->trace_mcounteren )
    // CHK_REG( "mcountinhibit", diff.mcountinhibit, top->trace_mcountinhibit )
    // CHK_REG( "tselect", diff.tselect,             top->trace_tselect )
    // CHK_REG( "tdata1", diff.tdata1[MAX_TRIGGERS], top->trace_tdata1[MAX_TRIGGERS] )
    // CHK_REG( "tdata2", diff.tdata2[MAX_TRIGGERS], top->trace_tdata2[MAX_TRIGGERS] )
    // CHK_REG( "tdata3", diff.tdata3[MAX_TRIGGERS], top->trace_tdata3[MAX_TRIGGERS] )
    // CHK_REG( "mhpmevent", diff.mhpmevent[32],     top->trace_mhpmevent[32] )


    CHK_REG( "pmpcfg[0]", diff.pmpcfg[0], top->trace_pmpcfg_0 )
    CHK_REG( "pmpcfg[2]", diff.pmpcfg[2], top->trace_pmpcfg_2 )

    CHK_REG( "pmpaddr[0]", diff.pmpaddr[0], top->trace_pmpaddr_0 )
    CHK_REG( "pmpaddr[2]", diff.pmpaddr[2], top->trace_pmpaddr_2 )
    CHK_REG( "pmpaddr[4]", diff.pmpaddr[4], top->trace_pmpaddr_4 )
    CHK_REG( "pmpaddr[6]", diff.pmpaddr[6], top->trace_pmpaddr_6 )
    CHK_REG( "pmpaddr[8]", diff.pmpaddr[8], top->trace_pmpaddr_8 )
    CHK_REG( "pmpaddr[10]", diff.pmpaddr[10], top->trace_pmpaddr_10 )
    CHK_REG( "pmpaddr[12]", diff.pmpaddr[12], top->trace_pmpaddr_12 )
    CHK_REG( "pmpaddr[14]", diff.pmpaddr[14], top->trace_pmpaddr_14 )


    CHK_REG( "stvec",      diff.stvec,      top->trace_stvec )
    CHK_REG( "sscratch",   diff.sscratch,   top->trace_sscratch )
    CHK_REG( "sepc",       diff.sepc,       top->trace_sepc )
    CHK_REG( "scause",     diff.scause,     top->trace_scause )
    CHK_REG( "stval",      diff.stval,      top->trace_stval )
    CHK_REG( "satp",       diff.satp,       top->trace_satp )
    // CHK_REG( "scounteren", diff.scounteren, top->trace_scounteren )

    // CHK_REG( "dcsr",     diff.dcsr,     top->trace_dcsr )
    // CHK_REG( "dpc",      diff.dpc,      top->trace_dpc )
    // CHK_REG( "dscratch", diff.dscratch, top->trace_dscratch )



	return 0;
}

