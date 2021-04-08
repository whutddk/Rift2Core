package rift2Core.frontend



/*
* @Author: Ruige Lee
* @Date:   2021-03-19 10:40:09
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-24 11:33:27
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
import rift2Core.basic._

class Decode32 (x:UInt) {

	val info = Wire(new Info_instruction)




	def iType_imm = Cat( Fill(52, x(31)), x(31,20))
	def sType_imm = Cat( Fill(52, x(31)), x(31,25), x(11,7) )
	def bType_imm = Cat( Fill(52, x(31)), x(7), x(30,25), x(11,8), 0.U(1.W))
	def uType_imm = Cat( Fill(32, x(31)), x(31,12), 0.U(12.W))
	def jType_imm = Cat( Fill(44, x(31)), x(19,12), x(20), x(30,21), 0.U(1.W))
	def aType_imm = Cat( Fill(62, 0.U), x(26,25))
	def mType_imm = Cat( Fill(61, 0.U), x(14,12))
	def shamt_imm = Cat( Fill(58, 0.U), x(25,20) )


	def is_iType = info.bru_isa.jalr | info.lsu_isa.lb | info.lsu_isa.lh |info.lsu_isa.lw | info.lsu_isa.lbu | info.lsu_isa.lhu | info.lsu_isa.lwu | info.lsu_isa.ld | info.alu_isa.addi | info.alu_isa.addiw | info.alu_isa.slti | info.alu_isa.sltiu | info.alu_isa.xori | info.alu_isa.ori | info.alu_isa.andi | info.lsu_isa.fence | info.lsu_isa.fence_i | info.csr_isa.rw | info.csr_isa.rs | info.csr_isa.rc | info.csr_isa.rwi | info.csr_isa.rsi | info.csr_isa.rci | info.lsu_isa.flw | info.lsu_isa.fld
	def is_sType = info.lsu_isa.sb | info.lsu_isa.sh | info.lsu_isa.sw | info.lsu_isa.sd | info.lsu_isa.fsw | info.lsu_isa.fsd 
	def is_bType = info.bru_isa.beq | info.bru_isa.bne | info.bru_isa.blt | info.bru_isa.bge | info.bru_isa.bltu | info.bru_isa.bgeu;
	def is_uType = info.alu_isa.lui | info.alu_isa.auipc;
	def is_jType = info.bru_isa.jal;
	def is_aType = info.lsu_isa.is_lrsc | info.lsu_isa.is_amo
	def is_mType = info.fpu_isa.is_fpu
	def is_shamt = info.alu_isa.slli | info.alu_isa.srli | info.alu_isa.srai | info.alu_isa.slliw | info.alu_isa.srliw | info.alu_isa.sraiw



	info.param.imm := MuxCase( 0.S, Array(
			is_iType -> iType_imm,
			is_sType -> sType_imm,
			is_bType -> bType_imm,
			is_uType -> uType_imm,
			is_jType -> jType_imm,
			is_aType -> aType_imm,
			is_mType -> mType_imm,
			is_shamt -> shamt_imm
		)
	)





	info.param.rd0_raw        := x(11,7)
	info.param.rs1_raw        := x(19,15)
	info.param.rs2_raw        := x(24,20)
	info.param.rs3_raw        := x(31,27)


	info.alu_isa.lui         := ( x === BitPat("b?????????????????????????0110111") )
	info.alu_isa.auipc       := ( x === BitPat("b?????????????????????????0010111") )
	info.alu_isa.addi        := ( x === BitPat("b?????????????????000?????0010011") )
	info.alu_isa.addiw       := ( x === BitPat("b?????????????????000?????0011011") )
	info.alu_isa.slti        := ( x === BitPat("b?????????????????010?????0010011") )
	info.alu_isa.sltiu       := ( x === BitPat("b?????????????????011?????0010011") )
	info.alu_isa.xori        := ( x === BitPat("b?????????????????100?????0010011") )
	info.alu_isa.ori         := ( x === BitPat("b?????????????????110?????0010011") )
	info.alu_isa.andi        := ( x === BitPat("b?????????????????111?????0010011") )
	info.alu_isa.slli        := ( x === BitPat("b000000???????????001?????0010011") )
	info.alu_isa.slliw       := ( x === BitPat("b0000000??????????001?????0011011") )
	info.alu_isa.srli        := ( x === BitPat("b000000???????????101?????0010011") )
	info.alu_isa.srliw       := ( x === BitPat("b0000000??????????101?????0011011") )
	info.alu_isa.srai        := ( x === BitPat("b010000???????????101?????0010011") )
	info.alu_isa.sraiw       := ( x === BitPat("b0100000??????????101?????0011011") )
	info.alu_isa.add         := ( x === BitPat("b0000000??????????000?????0110011") )
	info.alu_isa.addw        := ( x === BitPat("b0000000??????????000?????0111011") )
	info.alu_isa.sub         := ( x === BitPat("b0100000??????????000?????0110011") )
	info.alu_isa.subw        := ( x === BitPat("b0100000??????????000?????0111011") )
	info.alu_isa.sll         := ( x === BitPat("b0000000??????????001?????0110011") )
	info.alu_isa.sllw        := ( x === BitPat("b0000000??????????001?????0111011") )
	info.alu_isa.slt         := ( x === BitPat("b0000000??????????010?????0110011") )
	info.alu_isa.sltu        := ( x === BitPat("b0000000??????????011?????0110011") )
	info.alu_isa.xor         := ( x === BitPat("b0000000??????????100?????0110011") )
	info.alu_isa.srl         := ( x === BitPat("b0000000??????????101?????0110011") )
	info.alu_isa.srlw        := ( x === BitPat("b0000000??????????101?????0111011") )
	info.alu_isa.sra         := ( x === BitPat("b0100000??????????101?????0110011") )
	info.alu_isa.sraw        := ( x === BitPat("b0100000??????????101?????0111011") )
	info.alu_isa.or          := ( x === BitPat("b0000000??????????110?????0110011") )
	info.alu_isa.and         := ( x === BitPat("b0000000??????????111?????0110011") )

	info.bru_isa.jal         := ( x === BitPat("b?????????????????????????1101111") )
	info.bru_isa.jalr        := ( x === BitPat("b?????????????????000?????1100111") )
	info.bru_isa.beq         := ( x === BitPat("b?????????????????000?????1100011") )
	info.bru_isa.bne         := ( x === BitPat("b?????????????????001?????1100011") )
	info.bru_isa.blt         := ( x === BitPat("b?????????????????100?????1100011") )
	info.bru_isa.bge         := ( x === BitPat("b?????????????????101?????1100011") )
	info.bru_isa.bltu        := ( x === BitPat("b?????????????????110?????1100011") )
	info.bru_isa.bgeu        := ( x === BitPat("b?????????????????111?????1100011") )

	info.lsu_isa.lb          := ( x === BitPat("b?????????????????000?????0000011") )
	info.lsu_isa.lh          := ( x === BitPat("b?????????????????001?????0000011") )
	info.lsu_isa.lw          := ( x === BitPat("b?????????????????010?????0000011") )
	info.lsu_isa.ld          := ( x === BitPat("b?????????????????011?????0000011") )
	info.lsu_isa.lbu         := ( x === BitPat("b?????????????????100?????0000011") )
	info.lsu_isa.lhu         := ( x === BitPat("b?????????????????101?????0000011") )
	info.lsu_isa.lwu         := ( x === BitPat("b?????????????????110?????0000011") )
	info.lsu_isa.sb          := ( x === BitPat("b?????????????????000?????0100011") )
	info.lsu_isa.sh          := ( x === BitPat("b?????????????????001?????0100011") )
	info.lsu_isa.sw          := ( x === BitPat("b?????????????????010?????0100011") )
	info.lsu_isa.sd          := ( x === BitPat("b?????????????????011?????0100011") )
	info.lsu_isa.fence       := ( x === BitPat("b?????????????????000?????0001111") )
	info.lsu_isa.fence_i     := ( x === BitPat("b?????????????????001?????0001111") )

	info.csr_isa.rw          := ( x === BitPat("b?????????????????001?????1110011") )
	info.csr_isa.rs          := ( x === BitPat("b?????????????????010?????1110011") )
	info.csr_isa.rc          := ( x === BitPat("b?????????????????011?????1110011") )
	info.csr_isa.rwi         := ( x === BitPat("b?????????????????101?????1110011") )
	info.csr_isa.rsi         := ( x === BitPat("b?????????????????110?????1110011") )
	info.csr_isa.rci         := ( x === BitPat("b?????????????????111?????1110011") )

	info.mul_isa.mul         := ( x === BitPat("b0000001??????????000?????0110011") )
	info.mul_isa.mulh        := ( x === BitPat("b0000001??????????001?????0110011") )
	info.mul_isa.mulhsu      := ( x === BitPat("b0000001??????????010?????0110011") )
	info.mul_isa.mulhu       := ( x === BitPat("b0000001??????????011?????0110011") )
	info.mul_isa.div         := ( x === BitPat("b0000001??????????100?????0110011") )
	info.mul_isa.divu        := ( x === BitPat("b0000001??????????101?????0110011") )
	info.mul_isa.rem         := ( x === BitPat("b0000001??????????110?????0110011") )
	info.mul_isa.remu        := ( x === BitPat("b0000001??????????111?????0110011") )
	info.mul_isa.mulw        := ( x === BitPat("b0000001??????????000?????0111011") )
	info.mul_isa.divw        := ( x === BitPat("b0000001??????????100?????0111011") )
	info.mul_isa.divuw       := ( x === BitPat("b0000001??????????101?????0111011") )
	info.mul_isa.remw        := ( x === BitPat("b0000001??????????110?????0111011") )
	info.mul_isa.remuw       := ( x === BitPat("b0000001??????????111?????0111011") )

	info.privil_isa.ecall       := ( x === BitPat("b00000000000000000000000001110011") )
	info.privil_isa.ebreak      := ( x === BitPat("b00000000000100000000000001110011") )
	info.privil_isa.mret        := ( x === BitPat("b00110000001000000000000001110011") )
	info.privil_isa.uret        := ( x === BitPat("b00000000001000000000000001110011") )
	info.privil_isa.sret        := ( x === BitPat("b00010000001000000000000001110011") )
	info.privil_isa.dret        := ( x === BitPat("b01111011001000000000000001110011") )

	info.privil_isa.wfi         := ( x === BitPat("b00010000010100000000000001110011") )

	info.privil_isa.sfence_vma  := ( x === BitPat("b0001001??????????000000001110011") )

	info.privil_isa.hfence_vvma := ( x === BitPat("b0010001??????????000000001110011") )
	info.privil_isa.hfence_gvma := ( x === BitPat("b0110001??????????000000001110011") )

	info.privil_isa.hlv_b       := ( x === BitPat("b0110000 00000 ????? 100 ????? 1110011") )
	info.privil_isa.hlv_bu      := ( x === BitPat("b0110000 00001 ????? 100 ????? 1110011") )
	info.privil_isa.hlv_h       := ( x === BitPat("b0110010 00000 ????? 100 ????? 1110011") )
	info.privil_isa.hlv_hu      := ( x === BitPat("b0110010 00001 ????? 100 ????? 1110011") )
	info.privil_isa.hlvx_hu     := ( x === BitPat("b0110010 00011 ????? 100 ????? 1110011") )
	info.privil_isa.hlv_w       := ( x === BitPat("b0110100 00000 ????? 100 ????? 1110011") )
	info.privil_isa.hlvx_wu     := ( x === BitPat("b0110100 00011 ????? 100 ????? 1110011") )
	info.privil_isa.hsv_b       := ( x === BitPat("b0110001 ????? ????? 100 00000 1110011") )
	info.privil_isa.hsv_h       := ( x === BitPat("b0110011 ????? ????? 100 00000 1110011") )
	info.privil_isa.hsv_w       := ( x === BitPat("b0110101 ????? ????? 100 00000 1110011") )
	info.privil_isa.hlv_wu      := ( x === BitPat("b0110100 00001 ????? 100 ????? 1110011") )
	info.privil_isa.hlv_d       := ( x === BitPat("b0110110 00000 ????? 100 ????? 1110011") )
	info.privil_isa.hsv_d       := ( x === BitPat("b0110111 ????? ????? 100 00000 1110011") )

	info.lsu_isa.lr_w        := ( x === BitPat("b00010??00000?????010?????0101111") )
	info.lsu_isa.sc_w        := ( x === BitPat("b00011????????????010?????0101111") )
	info.lsu_isa.amoswap_w   := ( x === BitPat("b00001????????????010?????0101111") )
	info.lsu_isa.amoadd_w    := ( x === BitPat("b00000????????????010?????0101111") )
	info.lsu_isa.amoxor_w    := ( x === BitPat("b00100????????????010?????0101111") )
	info.lsu_isa.amoand_w    := ( x === BitPat("b01100????????????010?????0101111") )
	info.lsu_isa.amoor_w     := ( x === BitPat("b01000????????????010?????0101111") )
	info.lsu_isa.amomin_w    := ( x === BitPat("b10000????????????010?????0101111") )
	info.lsu_isa.amomax_w    := ( x === BitPat("b10100????????????010?????0101111") )
	info.lsu_isa.amominu_w   := ( x === BitPat("b11000????????????010?????0101111") )
	info.lsu_isa.amomaxu_w   := ( x === BitPat("b11100????????????010?????0101111") )
	info.lsu_isa.lr_d        := ( x === BitPat("b00010??00000?????011?????0101111") )
	info.lsu_isa.sc_d        := ( x === BitPat("b00011????????????011?????0101111") )
	info.lsu_isa.amoswap_d   := ( x === BitPat("b00001????????????011?????0101111") )
	info.lsu_isa.amoadd_d    := ( x === BitPat("b00000????????????011?????0101111") )
	info.lsu_isa.amoxor_d    := ( x === BitPat("b00100????????????011?????0101111") )
	info.lsu_isa.amoand_d    := ( x === BitPat("b01100????????????011?????0101111") )
	info.lsu_isa.amoor_d     := ( x === BitPat("b01000????????????011?????0101111") )
	info.lsu_isa.amomin_d    := ( x === BitPat("b10000????????????011?????0101111") )
	info.lsu_isa.amomax_d    := ( x === BitPat("b10100????????????011?????0101111") )
	info.lsu_isa.amominu_d   := ( x === BitPat("b11000????????????011?????0101111") )
	info.lsu_isa.amomaxu_d   := ( x === BitPat("b11100????????????011?????0101111") )


	info.lsu_isa.flw         := ( x === BitPat("b?????????????????010?????0000111") )
	info.lsu_isa.fsw         := ( x === BitPat("b?????????????????010?????0100111") )
	info.fpu_isa.fmadd_s     := ( x === BitPat("b?????00??????????????????1000011") )
	info.fpu_isa.fmsub_s     := ( x === BitPat("b?????00??????????????????1000111") )
	info.fpu_isa.fnmsub_s    := ( x === BitPat("b?????00??????????????????1001011") )
	info.fpu_isa.fnmadd_s    := ( x === BitPat("b?????00??????????????????1001111") )
	info.fpu_isa.fadd_s      := ( x === BitPat("b0000000??????????????????1010011") )
	info.fpu_isa.fsub_s      := ( x === BitPat("b0000100??????????????????1010011") )
	info.fpu_isa.fmul_s      := ( x === BitPat("b0001000??????????????????1010011") )
	info.fpu_isa.fdiv_s      := ( x === BitPat("b0001100??????????????????1010011") )
	info.fpu_isa.fsqrt_s     := ( x === BitPat("b010110000000?????????????1010011") )
	info.fpu_isa.fsgnj_s     := ( x === BitPat("b0010000??????????000?????1010011") )
	info.fpu_isa.fsgnjn_s    := ( x === BitPat("b0010000??????????001?????1010011") )
	info.fpu_isa.fsgnjx_s    := ( x === BitPat("b0010000??????????010?????1010011") )
	info.fpu_isa.fmin_s      := ( x === BitPat("b0010100??????????000?????1010011") )
	info.fpu_isa.fmax_s      := ( x === BitPat("b0010100??????????001?????1010011") )
	info.fpu_isa.fcvt_w_s    := ( x === BitPat("b110000000000?????????????1010011") )
	info.fpu_isa.fcvt_wu_s   := ( x === BitPat("b110000100001?????????????1010011") )
	info.fpu_isa.fmv_x_w     := ( x === BitPat("b111000000000?????000?????1010011") )
	info.fpu_isa.feq_s       := ( x === BitPat("b1010000??????????010?????1010011") )
	info.fpu_isa.flt_s       := ( x === BitPat("b1010000??????????001?????1010011") )
	info.fpu_isa.fle_s       := ( x === BitPat("b1010000??????????000?????1010011") )
	info.fpu_isa.fclass_s    := ( x === BitPat("b111000000000?????001?????1010011") )
	info.fpu_isa.fcvt_s_w    := ( x === BitPat("b110100000000?????????????1010011") )
	info.fpu_isa.fcvt_s_wu   := ( x === BitPat("b110100000001?????????????1010011") )
	info.fpu_isa.fmv_w_x     := ( x === BitPat("b111100000000?????000?????1010011") )
	info.fpu_isa.fcvt_l_s    := ( x === BitPat("b110000000010?????????????1010011") )
	info.fpu_isa.fcvt_lu_s   := ( x === BitPat("b110000000011?????????????1010011") )
	info.fpu_isa.fcvt_s_l    := ( x === BitPat("b110100000010?????????????1010011") )
	info.fpu_isa.fcvt_s_lu   := ( x === BitPat("b110100000011?????????????1010011") )


	info.lsu_isa.fld         := ( x === BitPat("b?????????????????011?????0000111") )
	info.lsu_isa.fsd         := ( x === BitPat("b?????????????????011?????0100111") )
	info.fpu_isa.fmadd_d     := ( x === BitPat("b?????01??????????????????1000011") )
	info.fpu_isa.fmsub_d     := ( x === BitPat("b?????01??????????????????1000111") )
	info.fpu_isa.fnmsub_d    := ( x === BitPat("b?????01??????????????????1001011") )
	info.fpu_isa.fnmadd_d    := ( x === BitPat("b?????01??????????????????1001111") )
	info.fpu_isa.fadd_d      := ( x === BitPat("b0000001??????????????????1010011") )
	info.fpu_isa.fsub_d      := ( x === BitPat("b0000101??????????????????1010011") )
	info.fpu_isa.fmul_d      := ( x === BitPat("b0001001??????????????????1010011") )
	info.fpu_isa.fdiv_d      := ( x === BitPat("b0001101??????????????????1010011") )
	info.fpu_isa.fsqrt_d     := ( x === BitPat("b010110100000?????????????1010011") )
	info.fpu_isa.fsgnj_d     := ( x === BitPat("b0010001??????????000?????1010011") )
	info.fpu_isa.fsgnjn_d    := ( x === BitPat("b0010001??????????001?????1010011") )
	info.fpu_isa.fsgnjx_d    := ( x === BitPat("b0010001??????????010?????1010011") )
	info.fpu_isa.fmin_d      := ( x === BitPat("b0010101??????????000?????1010011") )
	info.fpu_isa.fmax_d      := ( x === BitPat("b0010101??????????001?????1010011") )
	info.fpu_isa.fcvt_s_d    := ( x === BitPat("b010000000001?????????????1010011") )
	info.fpu_isa.fcvt_d_s    := ( x === BitPat("b010000100000?????????????1010011") )
	info.fpu_isa.feq_d       := ( x === BitPat("b1010001??????????010?????1010011") )
	info.fpu_isa.flt_d       := ( x === BitPat("b1010001??????????001?????1010011") )
	info.fpu_isa.fle_d       := ( x === BitPat("b1010001??????????000?????1010011") )
	info.fpu_isa.fclass_d    := ( x === BitPat("b111000100000?????001?????1010011") )
	info.fpu_isa.fcvt_w_d    := ( x === BitPat("b110000100000?????????????1010011") )
	info.fpu_isa.fcvt_wu_d   := ( x === BitPat("b110000100001?????????????1010011") )
	info.fpu_isa.fcvt_d_w    := ( x === BitPat("b110100100000?????????????1010011") )
	info.fpu_isa.fcvt_d_wu   := ( x === BitPat("b110100100001?????????????1010011") )
	info.fpu_isa.fcvt_l_d    := ( x === BitPat("b110000100010?????????????1010011") )
	info.fpu_isa.fcvt_lu_d   := ( x === BitPat("b110000100011?????????????1010011") )
	info.fpu_isa.fmv_x_d     := ( x === BitPat("b111000100000?????000?????1010011") )
	info.fpu_isa.fcvt_d_l    := ( x === BitPat("b110100100010?????????????1010011") )
	info.fpu_isa.fcvt_d_lu   := ( x === BitPat("b110100100011?????????????1010011") )
	info.fpu_isa.fmv_d_x     := ( x === BitPat("b111100100000?????000?????1010011") )
}




