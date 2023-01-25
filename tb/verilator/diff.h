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


#ifndef _DIFF_H_
#define _DIFF_H_

#include "VSimTop.h"
#include "riscv_machine.h"

struct diff {
  uint64_t ireg[32];
	uint64_t freg[32];

	uint64_t pc;

	uint8_t priv;

	uint64_t mstatus;
	uint64_t mtvec;
	uint64_t mscratch;
	uint64_t mepc;
	uint64_t mcause;
	uint64_t mtval;

    uint64_t mvendorid;
    uint64_t marchid;
    uint64_t mimpid;
    uint64_t mhartid;
    uint64_t misa;
    uint64_t mie;
    uint64_t mip;
    uint64_t medeleg;
    uint64_t mideleg;
    // uint64_t mcounteren;
    // uint64_t mcountinhibit;
    // uint64_t tselect;
    // uint64_t tdata1[MAX_TRIGGERS];
    // uint64_t tdata2[MAX_TRIGGERS];
    // uint64_t tdata3[MAX_TRIGGERS];
    // uint64_t mhpmevent[32];

    uint64_t pmpcfg[1];
  	uint64_t pmpaddr[8];


    uint64_t stvec;
    uint64_t sscratch;
    uint64_t sepc;
    uint64_t scause;
    uint64_t stval;
    uint64_t satp;
    // uint64_t scounteren;

    // uint64_t dcsr;
    // uint64_t dpc;
    // uint64_t dscratch;

    uint32_t fflags;
    uint8_t  frm;
};



extern int dromajo_init();
extern void dromajo_step();
extern void dromajo_deinit();
extern int diff_chk_reg(VSimTop *top);
extern int diff_chk_pc(VSimTop *top);
extern struct diff diff;
#endif


