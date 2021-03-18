package rift2Core.basicElement

/*
* @Author: Ruige Lee
* @Date:   2021-03-18 19:41:58
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-18 19:59:57
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


trait Alu_isa extends Bundle{
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


} 

trait Bru_isa extends Bundle {
	val jal  = Bool()
	val jalr = Bool()
	val beq  = Bool()
	val bne  = Bool()
	val blt  = Bool()
	val bge  = Bool()
	val bltu = Bool()
	val bgeu = Bool()
}

trait Lsu_isa extends Bundle {
	val Lb  = Bool()
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
}

trait Csr_isa extends Bundle {
	val rw  = Bool()
	val rs  = Bool()
	val rc  = Bool()
	val rwi = Bool()
	val rsi = Bool()
	val rci = Bool()
}

trait Mul_isa extends Bundle {
	val mul     = Bool()
	val mulh    = Bool()
	val mullhsu = Bool()
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
}


trait Privil_isa extends Bundle {
	val ecall = Bool()
	val ebreak = Bool()
	val mret = Bool()

	val uret = Bool()
	val sret = Bool()

	val wfi = Bool()

	val sfence_vma = Bool()

	val hfence_vvma = Bool()
	val hfence_gvma = Bool()

	val hlv_b = Bool()
	val hlv_bu = Bool()
	val hlv_h = Bool()
	val hlv_hu = Bool()
	val hlv_w = Bool()
	val hlvx_wu = Bool()
	val hsv_b = Bool()
	val hsv_h = Bool()
	val hsv_w = Bool()

	val hlv_wu = Bool()
	val hlv_d = Bool()
	val hsv_d = Bool()
}

trait Aextend_isa extends Bundle {
	val LR_w      = Bool()
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
}

trait Fextend_isa extends Bundle {
	val flw = Bool()
	val fsw = Bool()
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
} 

trait Qextend_isa extends Bundle {
	val fld = Bool()
	val fsd = Bool()
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
	val fmv_d_w = Bool()
	val fmv_d_wu = Bool()

	val fcvt_l_d = Bool()
	val fcvt_lu_d = Bool()
	val fmv_x_d = Bool()
	val fcvt_d_l = Bool()
	val fcvt_d_lu = Bool()
	val fmv_d_x = Bool()
} 





