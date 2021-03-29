package rift2Core.basicElement

/*
* @Author: Ruige Lee
* @Date:   2021-03-18 19:41:58
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-29 15:10:06
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


class Alu_isa extends Bundle{
	val lui   = Bool()
	val auipc = Bool()
	val addi  = Bool()
	val addiw = Bool()
	val slti  = Bool()
	val sltiu = Bool()
	val xori  = Bool()
	val ori   = Bool()
	val andi  = Bool()
	val slli  = Bool()
	val slliw = Bool()
	val srli  = Bool()
	val srliw = Bool()
	val srai  = Bool()
	val sraiw = Bool()
	val add   = Bool()
	val addw  = Bool()
	val sub   = Bool()
	val subw  = Bool()
	val sll   = Bool()
	val sllw  = Bool()
	val slt   = Bool()
	val sltu  = Bool()
	val xor   = Bool()
	val srl   = Bool()
	val srlw  = Bool()
	val sra   = Bool()
	val sraw  = Bool()
	val or    = Bool()
	val and   = Bool()

	def is_32w = addiw | addw | subw | slliw | sllw | srliw | srlw | sraiw | sraw;
	def is_usi = sltiu | sltu
	def is_imm =  lui | auipc | addi | addiw | slti | sltiu | xori | ori | andi | slli | slliw | srli | srliw | srai | sraiw

	def is_fun_add = addi | addiw | add | addw | lui | auipc | sub | subw
	def is_fun_slt = slti | sltiu | slt | sltu
	def is_fun_xor = xori | xor
	def is_fun_or  = ori | or
	def is_fun_and = andi | and
	def is_fun_sll = slli | slliw | sll | sllw
	def is_fun_srl = srli | srliw | srl | srlw
	def is_fun_sra = srai | sraiw | sra | sraw

	def is_alu = lui | auipc | addi | addiw | slti | sltiu | xori | ori | andi | slli | slliw | srli | srliw | srai | sraiw | add | addw | sub | subw | sll | sllw | slt | sltu | xor | srl | srlw | sra | sraw | or |and

} 

class Bru_isa extends Bundle {
	val jal  = Bool()
	val jalr = Bool()
	val beq  = Bool()
	val bne  = Bool()
	val blt  = Bool()
	val bge  = Bool()
	val bltu = Bool()
	val bgeu = Bool()

	def is_bru = jal | jalr | beq | bne | blt | bge | bltu | bgeu
	def is_branch = beq | bne | blt | bge | bltu | bgeu
}

class Lsu_isa extends Bundle {
	val lb  = Bool()
	val lh  = Bool()
	val lw  = Bool()
	val ld  = Bool()
	val lbu = Bool()
	val lhu = Bool()
	val lwu = Bool()
	val sb  = Bool()
	val sh  = Bool()
	val sw  = Bool()
	val sd  = Bool()
	val fence = Bool()
	val fence_i = Bool()

	val lr_w      = Bool()
	val sc_w      = Bool()
	val amoswap_w = Bool()
	val amoadd_w  = Bool()
	val amoxor_w  = Bool()
	val amoand_w  = Bool()
	val amoor_w   = Bool()
	val amomin_w  = Bool()
	val amomax_w  = Bool()
	val amominu_w = Bool()
	val amomaxu_w = Bool()
	val lr_d      = Bool()
	val sc_d      = Bool()
	val amoswap_d = Bool()
	val amoadd_d  = Bool()
	val amoxor_d  = Bool()
	val amoand_d  = Bool()
	val amoor_d   = Bool()
	val amomin_d  = Bool()
	val amomax_d  = Bool()
	val amominu_d = Bool()
	val amomaxu_d = Bool()

	val flw = Bool()
	val fsw = Bool()
	val fld = Bool()
	val fsd = Bool()

	def is_lu  = lb | lh | lw | ld | lbu | lhu | lwu | lr_w | lr_d | flw | fld 
	def is_su  = sb | sh | sw | sd | sc_w | sc_d | fsw | fsd
	def is_nls = lb | lh | lw | ld | lbu | lhu | lwu | sb | sh | sw | sd | fence | fence_i
	def is_lrsc = lr_w | sc_w | lr_d | sc_d
	def is_amo = amoswap_w | amoadd_w | amoxor_w | amoand_w | amoor_w | amomin_w | amomax_w | amominu_w | amomaxu_w | amoswap_d | amoadd_d | amoxor_d | amoand_d | amoor_d | amomin_d | amomax_d | amominu_d | amomaxu_d
	def is_fls = flw | fsw | fld | fsd
	def is_lsu = is_nls | is_lrsc | is_amo | is_fls
}

class Csr_isa extends Bundle {
	val rw  = Bool()
	val rs  = Bool()
	val rc  = Bool()
	val rwi = Bool()
	val rsi = Bool()
	val rci = Bool()

	def is_csr = rw | rs | rc | rwi | rsi | rci

	
}

class Mul_isa extends Bundle {
	val mul     = Bool()
	val mulh    = Bool()
	val mulhsu = Bool()
	val mulhu   = Bool()
	val div     = Bool()
	val divu    = Bool()
	val rem     = Bool()
	val remu    = Bool()
	val mulw    = Bool()
	val divw    = Bool()
	val divuw   = Bool()
	val remw    = Bool()
	val remuw   = Bool()

	def is_mul = mul | mulh | mulhsu | mulhu | div | divu | rem | remu | mulw | divw | divuw | remw | remuw



}


class Privil_isa extends Bundle {
	val ecall = Bool()
	val ebreak = Bool()
	val mret = Bool()

	val uret = Bool()
	val sret = Bool()
	val dret = Bool()

	val wfi = Bool()

	val sfence_vma = Bool()

	val hfence_vvma = Bool()
	val hfence_gvma = Bool()

	val hlv_b = Bool()
	val hlv_bu = Bool()
	val hlv_h = Bool()
	val hlv_hu = Bool()
	val hlvx_hu = Bool()
	val hlv_w = Bool()
	val hlvx_wu = Bool()
	val hsv_b = Bool()
	val hsv_h = Bool()
	val hsv_w = Bool()

	val hlv_wu = Bool()
	val hlv_d = Bool()
	val hsv_d = Bool()


	def is_privil = ecall | ebreak | mret | uret | sret | dret | wfi | sfence_vma | hfence_vvma | hfence_gvma | hlv_b | hlv_bu | hlv_h | hlv_hu | hlvx_hu | hlv_w | hlvx_wu | hsv_b | hsv_h | hsv_w | hlv_wu | hlv_d | hsv_d


}


class Fpu_isa extends Bundle {

	val fmadd_s = Bool()
	val fmsub_s = Bool()
	val fnmsub_s = Bool()
	val fnmadd_s = Bool()
	val fadd_s = Bool()
	val fsub_s = Bool()
	val fmul_s = Bool()
	val fdiv_s = Bool()
	val fsqrt_s = Bool()
	val fsgnj_s = Bool()
	val fsgnjn_s = Bool()
	val fsgnjx_s = Bool()
	val fmin_s = Bool()
	val fmax_s = Bool()
	val fcvt_w_s = Bool()
	val fcvt_wu_s = Bool()
	val fmv_x_w = Bool()
	val feq_s = Bool()
	val flt_s = Bool()
	val fle_s = Bool()
	val fclass_s = Bool()
	val fcvt_s_w = Bool()
	val fcvt_s_wu = Bool()
	val fmv_w_x = Bool()

	val fcvt_l_s = Bool()
	val fcvt_lu_s = Bool()
	val fcvt_s_l = Bool()
	val fcvt_s_lu = Bool()


	val fmadd_d = Bool()
	val fmsub_d = Bool()
	val fnmsub_d = Bool()
	val fnmadd_d = Bool()
	val fadd_d = Bool()
	val fsub_d = Bool()
	val fmul_d = Bool()
	val fdiv_d = Bool()
	val fsqrt_d = Bool()
	val fsgnj_d = Bool()
	val fsgnjn_d = Bool()
	val fsgnjx_d = Bool()
	val fmin_d = Bool()
	val fmax_d = Bool()
	val fcvt_s_d = Bool()
	val fcvt_d_s = Bool()
	val feq_d = Bool()
	val flt_d = Bool()
	val fle_d = Bool()
	val fclass_d = Bool()
	val fcvt_w_d = Bool()
	val fcvt_wu_d = Bool()
	val fcvt_d_w = Bool()
	val fcvt_d_wu = Bool()

	val fcvt_l_d = Bool()
	val fcvt_lu_d = Bool()
	val fmv_x_d = Bool()
	val fcvt_d_l = Bool()
	val fcvt_d_lu = Bool()
	val fmv_d_x = Bool()


	def is_fpu = fmadd_s | fmsub_s | fnmsub_s | fnmadd_s | fadd_s | fsub_s | fmul_s | fdiv_s | fsqrt_s | fsgnj_s | fsgnjn_s | fsgnjx_s | fmin_s | fmax_s | fcvt_w_s | fcvt_wu_s | fmv_x_w | feq_s | flt_s | fle_s | fclass_s | fcvt_s_w | fcvt_s_wu | fmv_w_x | fcvt_l_s | fcvt_lu_s | fcvt_s_l | fcvt_s_lu | fmadd_d | fmsub_d | fnmsub_d | fnmadd_d | fadd_d | fsub_d | fmul_d | fdiv_d | fsqrt_d | fsgnj_d | fsgnjn_d | fsgnjx_d | fmin_d | fmax_d | fcvt_s_d | fcvt_d_s | feq_d | flt_d | fle_d | fclass_d | fcvt_w_d | fcvt_wu_d | fcvt_d_w | fcvt_d_wu | fcvt_l_d | fcvt_lu_d | fmv_x_d | fcvt_d_l | fcvt_d_lu | fmv_d_x



}





trait Instruction_set {
	val alu_isa = new Alu_isa
	val bru_isa = new Bru_isa
	val lsu_isa = new Lsu_isa
	val csr_isa = new Csr_isa
	val mul_isa = new Mul_isa
	val privil_isa = new Privil_isa
	val fpu_isa = new Fpu_isa

	def is_fwb = lsu_isa.flw | lsu_isa.fld | lsu_isa.lr_w | lsu_isa.lr_d |
			fpu_isa.fmadd_s | fpu_isa.fmsub_s | fpu_isa.fnmsub_s | 
			fpu_isa.fnmadd_s | fpu_isa.fadd_s | fpu_isa.fsub_s | 
			fpu_isa.fmul_s | fpu_isa.fdiv_s | fpu_isa.fsqrt_s | fpu_isa.fsgnj_s | 
			fpu_isa.fsgnjn_s | fpu_isa.fsgnjx_s | fpu_isa.fmin_s | fpu_isa.fmax_s | 
			fpu_isa.feq_s | fpu_isa.flt_s | fpu_isa.fle_s |
			fpu_isa.fmadd_d | fpu_isa.fmsub_d | fpu_isa.fnmsub_d |
			fpu_isa.fnmadd_d | fpu_isa.fadd_d | fpu_isa.fsub_d | fpu_isa.fmul_d |
			fpu_isa.fdiv_d | fpu_isa.fsqrt_d | fpu_isa.fsgnj_d | fpu_isa.fsgnjn_d |
			fpu_isa.fsgnjx_d | fpu_isa.fmin_d | fpu_isa. fmax_d

	def is_iwb = ~is_fwb
	def is_illeage = alu_isa.is_alu | bru_isa.is_bru | lsu_isa.is_lsu | csr_isa.is_csr | mul_isa.is_mul | privil_isa.is_privil | fpu_isa.is_fpu 

}



class Instruction_param extends Bundle {
	val is_rvc = Bool()
	val pc = UInt(64.W)
	val imm = UInt(64.W)
	val rd0_raw = UInt(5.W)
	val rs1_raw = UInt(5.W)
	val rs2_raw = UInt(5.W)
	val rs3_raw = UInt(5.W)
}

class Info_instruction extends Bundle with Instruction_set {
	val param = new Instruction_param
}


class Reg_raw extends Bundle {
	val rd0_raw = UInt(5.W)
	val rs1_raw = UInt(5.W)
	val rs2_raw = UInt(5.W)
	val rs3_raw = UInt(5.W)
}


class Reg_idx extends Bundle {
	val rd0_idx = UInt(2.W)
	val rs1_idx = UInt(2.W)
	val rs2_idx = UInt(2.W)
	val rs3_idx = UInt(2.W)
}




class Alu_dpt_info extends Bundle {
	val isa = new Alu_isa()
	val param = new Instruction_param
	val rn = new Reg_idx

}





class Bru_dpt_info extends Bundle {
	val isa = new Bru_isa
	val param = new Instruction_param
	val rn = new Reg_idx
}





class Lsu_dpt_info extends Bundle {
	val isa = new Lsu_isa
	val param = new Instruction_param
	val rn = new Reg_idx
}



class Csr_dpt_info extends Bundle {
	val isa = new Csr_isa
	val param = new Instruction_param
	val rn = new Reg_idx

}


class Mul_dpt_info extends Bundle {
	val isa = new Mul_isa
	val param = new Instruction_param
	val rn = new Reg_idx
}


class Fpu_dpt_info extends Bundle {
	val isa = new Fpu_isa
	val param = new Instruction_param
	val rn = new Reg_idx
}





class Info_reorder_i extends Bundle {
	val pc = UInt(64.W)
	val rd0_raw = UInt(5.W)
	val rd0_idx = UInt(2.W)

	val is_branch = Bool()
	val is_lu = Bool()
	val is_su = Bool()
	val is_fence = Bool()
	val is_csr = Bool()

	val privil = new Privil_isa
	val is_accessFault = Bool()
	val is_illeage = Bool()

}

class Info_reorder_f extends Bundle {
	val pc = UInt(64.W)
	val rd0_raw = UInt(5.W)
	val rd0_idx = UInt(2.W)

	val is_lu = Bool()
	val is_su = Bool()
}






class Privil_dpt_info extends Bundle {

}



class Alu_function extends Bundle {

	val add = Bool()
	val slt = Bool()
	val xor = Bool()
	val or  = Bool()
	val and = Bool()
	val sll = Bool()
	val srl = Bool()
	val sra = Bool()
}

class Alu_param extends Bundle {
	val is_32w = Bool()
	val is_usi = Bool()


	val op1 = UInt(64.W)
	val op2 = UInt(64.W)

	val rd0_raw = UInt(5.W)
	val rd0_idx = UInt(2.W)
}

class Alu_iss_info extends Bundle {
	val fun = new Alu_function
	val param = new Alu_param

}

class Bru_param extends Bundle {
	val is_rvc = Bool()
	val pc = UInt(64.W)
	val imm = UInt(64.W)


	val op1 = UInt(64.W)
	val op2 = UInt(64.W)

	val rd0_raw = UInt(5.W)
	val rd0_idx = UInt(2.W)
}

class Bru_iss_info extends Bundle {
	val fun = new Bru_isa
	val param = new Bru_param


}

class Lsu_param extends Bundle {
	val op1 = UInt(64.W)
	val op2 = UInt(64.W)

	val rd0_raw = UInt(5.W)
	val rd0_idx = UInt(2.W)
}

class Lsu_iss_info extends Bundle {
	val fun = new Lsu_isa
	val param = new Lsu_param
}

class Csr_param extends Bundle {
	val op1 = UInt(5.W)
	val op2 = UInt(12.W)

	val rd0_raw = UInt(5.W)
	val rd0_idx = UInt(2.W)
}

class Csr_iss_info extends Bundle {
	val fun = new Csr_isa
	val param = new Csr_param
}

class Mul_param extends Bundle {
	val op1 = UInt(64.W)
	val op2 = UInt(64.W)

	val rd0_raw = UInt(5.W)
	val rd0_idx = UInt(2.W)
}

class Mul_iss_info extends Bundle {
	val fun = new Mul_isa
	val param = new Mul_param
}

class Fpu_isu_info extends Bundle {
	
}




class Exe_iwb_info extends Bundle {
	val res = UInt(64.W)
	val rd0_raw = UInt(5.W)
	val rd0_idx = UInt(2.W)
}

class Exe_fwb_info extends Bundle {
	
}
