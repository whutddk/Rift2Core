package rift2Core

/*
* @Author: Ruige Lee
* @Date:   2021-03-19 16:24:13
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-19 16:58:36
*/




/*
  Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

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


import chisel3._
import chisel3.util._
import rift2Core.basicElement._


class decode16 extends Module {
	val io = IO ( new Bundle {
		val instr16 = Input(UInt(32.W))
		val info16 = Output(new Instruction_info)
	})


		val c_addi4spn = Wire(Bool())
		val c_fld = Wire(Bool())
		val c_lw  = Wire(Bool())
		val c_ld  = Wire(Bool())
		val c_fsd  = Wire(Bool())
		val c_sw  = Wire(Bool())
		val c_sd  = Wire(Bool())
		val c_nop  = Wire(Bool())
		val c_addi  = Wire(Bool())
		val c_addiw  = Wire(Bool())
		val c_li  = Wire(Bool())
		val c_addi16sp  = Wire(Bool())
		val c_lui  = Wire(Bool())
		val c_andi  = Wire(Bool())
		val c_sub  = Wire(Bool())
		val c_xor  = Wire(Bool())
		val c_or  = Wire(Bool())
		val c_and  = Wire(Bool())
		val c_subw  = Wire(Bool())
		val c_addw  = Wire(Bool())
		val c_j  = Wire(Bool())
		val c_beqz  = Wire(Bool())
		val c_bnez  = Wire(Bool())

		val c_slli  = Wire(Bool())
		val c_fldsp  = Wire(Bool())
		val c_lwsp  = Wire(Bool())
		val c_ldsp  = Wire(Bool())
		val c_jr  = Wire(Bool())
		val c_mv  = Wire(Bool())
		val c_ebreak  = Wire(Bool())
		val c_jalr  = Wire(Bool())
		val c_add  = Wire(Bool())
		val c_fsdsp  = Wire(Bool())
		val c_swsp  = Wire(Bool())
		val c_sdsp  = Wire(Bool())


		c_addi4spn := ( io.instr16 === BitPat("b????????????????000???????????00") )
		c_fld      := ( io.instr16 === BitPat("b????????????????001???????????00") )
		c_lw       := ( io.instr16 === BitPat("b????????????????010???????????00") )
		c_ld       := ( io.instr16 === BitPat("b????????????????011???????????00") )
		c_fsd      := ( io.instr16 === BitPat("b????????????????101???????????00") )
		c_sw       := ( io.instr16 === BitPat("b????????????????110???????????00") )
		c_sd       := ( io.instr16 === BitPat("b????????????????111???????????00") )
		c_nop      := ( io.instr16 === BitPat("b????????????????0000000000000001") )
		c_addi     := ( io.instr16 === BitPat("b????????????????000???????????01") )
		c_addiw    := ( io.instr16 === BitPat("b????????????????001???????????01") )
		c_li       := ( io.instr16 === BitPat("b????????????????010???????????01") )
		c_addi16sp := ( io.instr16 === BitPat("b????????????????011?00010?????01") )
		c_lui      := ( io.instr16 === BitPat("b????????????????011???????????01") )
		c_andi     := ( io.instr16 === BitPat("b????????????????100?10????????01") )
		c_sub      := ( io.instr16 === BitPat("b????????????????100011???00???01") )
		c_xor      := ( io.instr16 === BitPat("b????????????????100011???01???01") )
		c_or       := ( io.instr16 === BitPat("b????????????????100011???10???01") )
		c_and      := ( io.instr16 === BitPat("b????????????????100011???11???01") )
		c_subw     := ( io.instr16 === BitPat("b????????????????100111???00???01") )
		c_addw     := ( io.instr16 === BitPat("b????????????????100111???01???01") )
		c_j        := ( io.instr16 === BitPat("b????????????????101???????????01") )
		c_beqz     := ( io.instr16 === BitPat("b????????????????110???????????01") )
		c_bnez     := ( io.instr16 === BitPat("b????????????????111???????????01") )
		c_slli     := ( io.instr16 === BitPat("b????????????????000???????????10") )
		c_fldsp    := ( io.instr16 === BitPat("b????????????????001???????????10") )
		c_lwsp     := ( io.instr16 === BitPat("b????????????????010???????????10") )
		c_ldsp     := ( io.instr16 === BitPat("b????????????????011???????????10") )
		c_jr       := ( io.instr16 === BitPat("b????????????????1000?????0000010") )
		c_mv       := ( io.instr16 === BitPat("b????????????????1000??????????10") )
		c_ebreak   := ( io.instr16 === BitPat("b????????????????1001000000000010") )
		c_jalr     := ( io.instr16 === BitPat("b????????????????1001?????0000010") )
		c_add      := ( io.instr16 === BitPat("b????????????????1001??????????10") )
		c_fsdsp    := ( io.instr16 === BitPat("b????????????????101???????????10") )
		c_swsp     := ( io.instr16 === BitPat("b????????????????110???????????10") )
		c_sdsp     := ( io.instr16 === BitPat("b????????????????111???????????10") )


	io.info16.rd0        := 
	io.info16.rs1        := 
	io.info16.rs2        := 
	io.info16.shamt      := 

	io.info16.lui         := 
	io.info16.auipc       := 
	io.info16.addi        := 
	io.info16.addiw       := 
	io.info16.slti        := 
	io.info16.sltiu       := 
	io.info16.xori        := 
	io.info16.ori         := 
	io.info16.andi        := 
	io.info16.slli        := 
	io.info16.slliw       := 
	io.info16.srli        := 
	io.info16.srliw       := 
	io.info16.srai        := 
	io.info16.sraiw       := 
	io.info16.add         := 
	io.info16.addw        := 
	io.info16.sub         := 
	io.info16.subw        := 
	io.info16.sll         := 
	io.info16.sllw        := 
	io.info16.slt         := 
	io.info16.sltu        := 
	io.info16.xor         := 
	io.info16.srl         := 
	io.info16.srlw        := 
	io.info16.sra         := 
	io.info16.sraw        := 
	io.info16.or          := 
	io.info16.and         := 

	io.info16.jal         := 
	io.info16.jalr        := 
	io.info16.beq         := 
	io.info16.bne         := 
	io.info16.blt         := 
	io.info16.bge         := 
	io.info16.bltu        := 
	io.info16.bgeu        := 

	io.info16.lb          := 
	io.info16.lh          := 
	io.info16.lw          := 
	io.info16.ld          := 
	io.info16.lbu         := 
	io.info16.lhu         := 
	io.info16.lwu         := 
	io.info16.sb          := 
	io.info16.sh          := 
	io.info16.sw          := 
	io.info16.sd          := 
	io.info16.fence       := 
	io.info16.fence_i     := 

	io.info16.rw          := 
	io.info16.rs          := 
	io.info16.rc          := 
	io.info16.rwi         := 
	io.info16.rsi         := 
	io.info16.rci         := 

	io.info16.mul         := 
	io.info16.mulh        := 
	io.info16.mulhsu      := 
	io.info16.mulhu       := 
	io.info16.div         := 
	io.info16.divu        := 
	io.info16.rem         := 
	io.info16.remu        := 
	io.info16.mulw        := 
	io.info16.divw        := 
	io.info16.divuw       := 
	io.info16.remw        := 
	io.info16.remuw       := 

	io.info16.ecall       := 
	io.info16.ebreak      := 
	io.info16.mret        := 
	io.info16.uret        := 
	io.info16.sret        := 
	io.info16.dret        := 

	io.info16.wfi         := 

	io.info16.sfence_vma  := 

	io.info16.hfence_vvma := 
	io.info16.hfence_gvma := 

	io.info16.hlv_b       := 
	io.info16.hlv_bu      := 
	io.info16.hlv_h       := 
	io.info16.hlv_hu      := 
	io.info16.hlvx_hu     := 
	io.info16.hlv_w       := 
	io.info16.hlvx_wu     := 
	io.info16.hsv_b       := 
	io.info16.hsv_h       := 
	io.info16.hsv_w       := 
	io.info16.hlv_wu      := 
	io.info16.hlv_d       := 
	io.info16.hsv_d       := 

	io.info16.lr_w        := 
	io.info16.sc_w        := 
	io.info16.amoswap_w   := 
	io.info16.amoadd_w    := 
	io.info16.amoxor_w    := 
	io.info16.amoand_w    := 
	io.info16.amoor_w     := 
	io.info16.amomin_w    := 
	io.info16.amomax_w    := 
	io.info16.amominu_w   := 
	io.info16.amomaxu_w   := 
	io.info16.lr_d        := 
	io.info16.sc_d        := 
	io.info16.amoswap_d   := 
	io.info16.amoadd_d    := 
	io.info16.amoxor_d    := 
	io.info16.amoand_d    := 
	io.info16.amoor_d     := 
	io.info16.amomin_d    := 
	io.info16.amomax_d    := 
	io.info16.amominu_d   := 
	io.info16.amomaxu_d   := 


	io.info16.flw         := 
	io.info16.fsw         := 
	io.info16.fmadd_s     := 
	io.info16.fmsub_s     := 
	io.info16.fnmsub_s    := 
	io.info16.fnmadd_s    := 
	io.info16.fadd_s      := 
	io.info16.fsub_s      := 
	io.info16.fmul_s      := 
	io.info16.fdiv_s      := 
	io.info16.fsqrt_s     := 
	io.info16.fsgnj_s     := 
	io.info16.fsgnjn_s    := 
	io.info16.fsgnjx_s    := 
	io.info16.fmin_s      := 
	io.info16.fmax_s      := 
	io.info16.fcvt_w_s    := 
	io.info16.fcvt_wu_s   := 
	io.info16.fmv_x_w     := 
	io.info16.feq_s       := 
	io.info16.flt_s       := 
	io.info16.fle_s       := 
	io.info16.fclass_s    := 
	io.info16.fcvt_s_w    := 
	io.info16.fcvt_s_wu   := 
	io.info16.fmv_w_x     := 
	io.info16.fcvt_l_s    := 
	io.info16.fcvt_lu_s   := 
	io.info16.fcvt_s_l    := 
	io.info16.fcvt_s_lu   := 


	io.info16.fld         := 
	io.info16.fsd         := 
	io.info16.fmadd_d     := 
	io.info16.fmsub_d     := 
	io.info16.fnmsub_d    := 
	io.info16.fnmadd_d    := 
	io.info16.fadd_d      := 
	io.info16.fsub_d      := 
	io.info16.fmul_d      := 
	io.info16.fdiv_d      := 
	io.info16.fsqrt_d     := 
	io.info16.fsgnj_d     := 
	io.info16.fsgnjn_d    := 
	io.info16.fsgnjx_d    := 
	io.info16.fmin_d      := 
	io.info16.fmax_d      := 
	io.info16.fcvt_s_d    := 
	io.info16.fcvt_d_s    := 
	io.info16.feq_d       := 
	io.info16.flt_d       := 
	io.info16.fle_d       := 
	io.info16.fclass_d    := 
	io.info16.fcvt_w_d    := 
	io.info16.fcvt_wu_d   := 
	io.info16.fcvt_d_w    := 
	io.info16.fcvt_d_wu   := 
	io.info16.fcvt_l_d    := 
	io.info16.fcvt_lu_d   := 
	io.info16.fmv_x_d     := 
	io.info16.fcvt_d_l    := 
	io.info16.fcvt_d_lu   := 
	io.info16.fmv_d_x     := 
}




}
