package rift2Core.frontend

/*
* @Author: Ruige Lee
* @Date:   2021-03-19 16:24:13
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-24 11:23:40
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


class Decode16 (x:UInt) {

	val info = Wire(new Instruction_info)


	// val c_addi4spn = Wire(Bool())
	// val c_fld = Wire(Bool())
	// val c_lw  = Wire(Bool())
	// val c_ld  = Wire(Bool())
	// val c_fsd  = Wire(Bool())
	// val c_sw  = Wire(Bool())
	// val c_sd  = Wire(Bool())
	// val c_nop  = Wire(Bool())
	// val c_addi  = Wire(Bool())
	// val c_addiw  = Wire(Bool())
	// val c_li  = Wire(Bool())
	// val c_addi16sp  = Wire(Bool())
	// val c_lui  = Wire(Bool())
	// val c_srli  = Wire(Bool())
	// val c_srai  = Wire(Bool())
	// val c_andi  = Wire(Bool())
	// val c_sub  = Wire(Bool())
	// val c_xor  = Wire(Bool())
	// val c_or  = Wire(Bool())
	// val c_and  = Wire(Bool())
	// val c_subw  = Wire(Bool())
	// val c_addw  = Wire(Bool())
	// val c_j  = Wire(Bool())
	// val c_beqz  = Wire(Bool())
	// val c_bnez  = Wire(Bool())

	// val c_slli  = Wire(Bool())
	// val c_fldsp  = Wire(Bool())
	// val c_lwsp  = Wire(Bool())
	// val c_ldsp  = Wire(Bool())
	// val c_jr  = Wire(Bool())
	// val c_mv  = Wire(Bool())
	// val c_ebreak  = Wire(Bool())
	// val c_jalr  = Wire(Bool())
	// val c_add  = Wire(Bool())
	// val c_fsdsp  = Wire(Bool())
	// val c_swsp  = Wire(Bool())
	// val c_sdsp  = Wire(Bool())


	def addi4spnImm = Cat(0.U(54.W), x(10,7), x(12,11), x(5), x(6), 0.U(2.W)).asSInt()
	def lwImm = Cat(0.U(57.W), x(5), x(12,10), x(6), 0.U(2.W)).asSInt()
	def ldImm = Cat(0.U(56.W), x(6,5), x(12,10), 0.U(3.W)).asSInt()
	def lwspImm = Cat(0.U(56.W), x(3,2), x(12), x(6,4), 0.U(2.W)).asSInt()
	def ldspImm = Cat(0.U(55.W), x(4,2), x(12), x(6,5), 0.U(3.W)).asSInt()
	def swspImm = Cat(0.U(56.W), x(8,7), x(12,9), 0.U(2.W)).asSInt()
	def sdspImm = Cat(0.U(55.W), x(9,7), x(12,10), 0.U(3.W)).asSInt()
	def luiImm = Cat(Fill(47, x(12)), x(6,2), 0.U(12.W)).asSInt()
	def addi16spImm = Cat(Fill(55, x(12)), x(4,3), x(5), x(2), x(6), 0.U(4.W)).asSInt()
	def addiImm = Cat(Fill(59, x(12)), x(6,2)).asSInt()
	def jImm = Cat(Fill(53, x(12)), x(8), x(10,9), x(6), x(7), x(2), x(11), x(5,3), 0.U(1.W)).asSInt()
	def bImm = Cat(Fill(56, x(12)), x(6,5), x(2), x(11,10), x(4,3), 0.U(1.W)).asSInt()


	def c_addi4spn = ( x === BitPat("b????????????????000???????????00") ) & (x(12,5) =/= 0.U)
	def c_fld      = ( x === BitPat("b????????????????001???????????00") )
	def c_lw       = ( x === BitPat("b????????????????010???????????00") )
	def c_ld       = ( x === BitPat("b????????????????011???????????00") )
	def c_fsd      = ( x === BitPat("b????????????????101???????????00") )
	def c_sw       = ( x === BitPat("b????????????????110???????????00") )
	def c_sd       = ( x === BitPat("b????????????????111???????????00") )
	def c_nop      = ( x === BitPat("b????????????????0000000000000001") )
	def c_addi     = ( x === BitPat("b????????????????000???????????01") ) & (x(11,7) =/= 0.U)
	def c_addiw    = ( x === BitPat("b????????????????001???????????01") ) & (x(11,7) =/= 0.U)
	def c_li       = ( x === BitPat("b????????????????010???????????01") ) & (x(11,7) =/= 0.U)
	def c_addi16sp = ( x === BitPat("b????????????????011?00010?????01") ) & (x(11,7) === 2.U) & (Cat(x(12), x(6,2)) =/= 0.U)
	def c_lui      = ( x === BitPat("b????????????????011???????????01") ) & (x(11,7) =/= 2.U) & (x(12,2) =/= 0.U)
	def c_srli     = ( x === BitPat("b????????????????100?00????????01") ) & (Cat(x(12), x(6,2)) =/= 0.U)
	def c_srai     = ( x === BitPat("b????????????????100?01????????01") ) & (Cat(x(12), x(6,2)) =/= 0.U)

	def c_andi     = ( x === BitPat("b????????????????100?10????????01") )
	def c_sub      = ( x === BitPat("b????????????????100011???00???01") )
	def c_xor      = ( x === BitPat("b????????????????100011???01???01") )
	def c_or       = ( x === BitPat("b????????????????100011???10???01") )
	def c_and      = ( x === BitPat("b????????????????100011???11???01") )
	def c_subw     = ( x === BitPat("b????????????????100111???00???01") )
	def c_addw     = ( x === BitPat("b????????????????100111???01???01") )
	def c_j        = ( x === BitPat("b????????????????101???????????01") )
	def c_beqz     = ( x === BitPat("b????????????????110???????????01") )
	def c_bnez     = ( x === BitPat("b????????????????111???????????01") )
	def c_slli     = ( x === BitPat("b????????????????000???????????10") ) & (x(11,7) =/= 0.U)
	def c_fldsp    = ( x === BitPat("b????????????????001???????????10") )
	def c_lwsp     = ( x === BitPat("b????????????????010???????????10") ) & (x(11,7) =/= 0.U)
	def c_ldsp     = ( x === BitPat("b????????????????011???????????10") ) & (x(11,7) =/= 0.U)
	def c_jr       = ( x === BitPat("b????????????????1000?????0000010") ) & (x(11,7) =/= 0.U)
	def c_mv       = ( x === BitPat("b????????????????1000??????????10") ) & (x(11,2) =/= 0.U)
	def c_ebreak   = ( x === BitPat("b????????????????1001000000000010") )
	def c_jalr     = ( x === BitPat("b????????????????1001?????0000010") ) & (x(11,7) =/= 0.U)
	def c_add      = ( x === BitPat("b????????????????1001??????????10") ) & (x(11,2) =/= 0.U)
	def c_fsdsp    = ( x === BitPat("b????????????????101???????????10") )
	def c_swsp     = ( x === BitPat("b????????????????110???????????10") )
	def c_sdsp     = ( x === BitPat("b????????????????111???????????10") )


	info.rd0        :=
		MuxCase( 0.U, Array(
				( x === BitPat("b????????????????0?????????????00") ) -> Cat(1.U(2.W), x(4,2)),
				( x === BitPat("b????????????????0?????????????01") ) -> x(11,7),
				( x === BitPat("b????????????????1?????????????01") ) -> Cat(1.U(2.W), x(9,7)),
				( x === BitPat("b??????????????????????????????10") ) -> x(11,7)
			)
		)


	info.rs1        := 
		MuxCase( 0.U, Array(
				( x === BitPat("b??????????????????????????????00") ) -> Cat(1.U(2.W), x(9,7)),
				( x === BitPat("b????????????????0?????????????01") ) -> x(11,7),
				( x === BitPat("b????????????????1?????????????01") ) -> Cat(1.U(2.W), x(9,7)),
				( x === BitPat("b??????????????????????????????10") ) -> x(11,7)
			)
		)


	info.rs2        := 
		MuxCase( 0.U, Array(
				( x === BitPat("b????????????????1?????????????00") ) -> Cat(1.U(2.W), x(4,2)),
				( x === BitPat("b????????????????0?????????????01") ) -> x(11,7),
				( x === BitPat("b????????????????100???????????01") ) -> Cat(1.U(2.W), x(4,2)),
				( x === BitPat("b????????????????100???????????10") ) -> x(6,2)
			)
		)
	
	info.shamt      := Cat(x(12), x(6,2))

	info.imm        :=
		MuxCase( 0.S, Array(
				c_addi4spn -> addi4spnImm,
				c_fld -> ldImm,
				c_lw -> lwImm,
				c_ld -> ldImm,
				c_fsd -> ldImm,
				c_sw -> lwImm,
				c_sd -> ldImm,
				c_addi -> addiImm,
				c_addiw -> addiImm,
				c_li ->  addiImm,
				c_addi16sp -> addi16spImm,
				c_lui -> luiImm,
				c_andi ->addiImm,
				c_j -> jImm,
				c_beqz -> bImm,
				c_bnez -> bImm,
				c_fldsp -> ldspImm,
				c_lwsp -> lwspImm,
				c_ldsp -> ldspImm,
				c_fsdsp -> sdspImm,
				c_swsp -> swspImm,
				c_sdsp -> sdspImm
			)
		)





	info.lui         := c_lui
	info.auipc       := false.B
	info.addi        := c_addi4spn | c_nop | c_addi | c_addi16sp
	info.addiw       := c_addiw
	info.slti        := false.B
	info.sltiu       := false.B
	info.xori        := false.B
	info.ori         := false.B
	info.andi        := c_andi
	info.slli        := c_slli
	info.slliw       := false.B
	info.srli        := c_srli
	info.srliw       := false.B
	info.srai        := c_srai
	info.sraiw       := false.B
	info.add         := c_mv | c_add
	info.addw        := c_addw
	info.sub         := c_sub
	info.subw        := c_subw
	info.sll         := false.B
	info.sllw        := false.B
	info.slt         := false.B
	info.sltu        := false.B
	info.xor         := c_xor
	info.srl         := false.B
	info.srlw        := false.B
	info.sra         := false.B
	info.sraw        := false.B
	info.or          := c_or
	info.and         := c_and

	info.jal         := c_j
	info.jalr        := c_jr | c_jalr
	info.beq         := c_beqz
	info.bne         := c_bnez
	info.blt         := false.B
	info.bge         := false.B
	info.bltu        := false.B
	info.bgeu        := false.B

	info.lb          := false.B
	info.lh          := false.B
	info.lw          := c_lw | c_lwsp
	info.ld          := c_ld | c_ldsp
	info.lbu         := false.B
	info.lhu         := false.B
	info.lwu         := false.B
	info.sb          := false.B
	info.sh          := false.B
	info.sw          := c_sw | c_swsp
	info.sd          := c_sd | c_sdsp
	info.fence       := false.B
	info.fence_i     := false.B

	info.rw          := false.B
	info.rs          := false.B
	info.rc          := false.B
	info.rwi         := false.B
	info.rsi         := false.B
	info.rci         := false.B

	info.mul         := false.B
	info.mulh        := false.B
	info.mulhsu      := false.B
	info.mulhu       := false.B
	info.div         := false.B
	info.divu        := false.B
	info.rem         := false.B
	info.remu        := false.B
	info.mulw        := false.B
	info.divw        := false.B
	info.divuw       := false.B
	info.remw        := false.B
	info.remuw       := false.B

	info.ecall       := false.B
	info.ebreak      := c_ebreak
	info.mret        := false.B
	info.uret        := false.B
	info.sret        := false.B
	info.dret        := false.B

	info.wfi         := false.B

	info.sfence_vma  := false.B

	info.hfence_vvma := false.B
	info.hfence_gvma := false.B

	info.hlv_b       := false.B
	info.hlv_bu      := false.B
	info.hlv_h       := false.B
	info.hlv_hu      := false.B
	info.hlvx_hu     := false.B
	info.hlv_w       := false.B
	info.hlvx_wu     := false.B
	info.hsv_b       := false.B
	info.hsv_h       := false.B
	info.hsv_w       := false.B
	info.hlv_wu      := false.B
	info.hlv_d       := false.B
	info.hsv_d       := false.B

	info.lr_w        := false.B
	info.sc_w        := false.B
	info.amoswap_w   := false.B
	info.amoadd_w    := false.B
	info.amoxor_w    := false.B
	info.amoand_w    := false.B
	info.amoor_w     := false.B
	info.amomin_w    := false.B
	info.amomax_w    := false.B
	info.amominu_w   := false.B
	info.amomaxu_w   := false.B
	info.lr_d        := false.B
	info.sc_d        := false.B
	info.amoswap_d   := false.B
	info.amoadd_d    := false.B
	info.amoxor_d    := false.B
	info.amoand_d    := false.B
	info.amoor_d     := false.B
	info.amomin_d    := false.B
	info.amomax_d    := false.B
	info.amominu_d   := false.B
	info.amomaxu_d   := false.B

	info.flw         := false.B
	info.fsw         := false.B
	info.fmadd_s     := false.B
	info.fmsub_s     := false.B
	info.fnmsub_s    := false.B
	info.fnmadd_s    := false.B
	info.fadd_s      := false.B
	info.fsub_s      := false.B
	info.fmul_s      := false.B
	info.fdiv_s      := false.B
	info.fsqrt_s     := false.B
	info.fsgnj_s     := false.B
	info.fsgnjn_s    := false.B
	info.fsgnjx_s    := false.B
	info.fmin_s      := false.B
	info.fmax_s      := false.B
	info.fcvt_w_s    := false.B
	info.fcvt_wu_s   := false.B
	info.fmv_x_w     := false.B
	info.feq_s       := false.B
	info.flt_s       := false.B
	info.fle_s       := false.B
	info.fclass_s    := false.B
	info.fcvt_s_w    := false.B
	info.fcvt_s_wu   := false.B
	info.fmv_w_x     := false.B
	info.fcvt_l_s    := false.B
	info.fcvt_lu_s   := false.B
	info.fcvt_s_l    := false.B
	info.fcvt_s_lu   := false.B

	info.fld         := c_fld | c_fldsp
	info.fsd         := c_fsd | c_fsdsp
	info.fmadd_d     := false.B
	info.fmsub_d     := false.B
	info.fnmsub_d    := false.B
	info.fnmadd_d    := false.B
	info.fadd_d      := false.B
	info.fsub_d      := false.B
	info.fmul_d      := false.B
	info.fdiv_d      := false.B
	info.fsqrt_d     := false.B
	info.fsgnj_d     := false.B
	info.fsgnjn_d    := false.B
	info.fsgnjx_d    := false.B
	info.fmin_d      := false.B
	info.fmax_d      := false.B
	info.fcvt_s_d    := false.B
	info.fcvt_d_s    := false.B
	info.feq_d       := false.B
	info.flt_d       := false.B
	info.fle_d       := false.B
	info.fclass_d    := false.B
	info.fcvt_w_d    := false.B
	info.fcvt_wu_d   := false.B
	info.fcvt_d_w    := false.B
	info.fcvt_d_wu   := false.B
	info.fcvt_l_d    := false.B
	info.fcvt_lu_d   := false.B
	info.fmv_x_d     := false.B
	info.fcvt_d_l    := false.B
	info.fcvt_d_lu   := false.B
	info.fmv_d_x     := false.B

}

