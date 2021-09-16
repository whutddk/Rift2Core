
#ifndef _DIFF_H_
#define _DIFF_H_

#include "VSimTop.h"
#include "riscv_machine.h"

struct diff {
	uint64_t ireg[64];

	uint64_t pc;

	uint8_t priv;

	uint64_t mstatus;
	uint64_t mcause;
	uint64_t mtval;
	uint64_t mtvec;
	uint64_t mepc;
};



extern void dromajo_init();
extern void dromajo_step();
extern void dromajo_deinit();
extern int diff_chk(VSimTop *top);
extern struct diff diff;
#endif


