
/*
* @Author: Ruige Lee
* @Date:   2021-04-09 10:34:13
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-09 10:34:13
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


package rift2Core.frontend

import chisel3._
import chisel3.util._
import rift2Core.basic._



class PreDecode16() {
	val io = new Bundle {
		val instr16 = Wire( UInt(16.W) )

		val info = Wire(new Info_preDecode)
	}

	io.info.is_rvc := true.B

	io.info.is_jal    := (io.instr16 === BitPat("b101???????????01"))
	io.info.is_jalr   := (io.instr16 === BitPat("b100??????0000010") & io.instr16(11,7) =/= 0.U)
	io.info.is_branch := (io.instr16 === BitPat("b11????????????01"))
	io.info.is_call   := (io.instr16 === BitPat("b1001?????0000010") & io.instr16(11,7) =/= 0.U)
	io.info.is_return := (io.instr16 === BitPat("b100000?010000010"))
	io.info.is_fencei := false.B
	io.info.imm       := MuxCase( 0.U, Array(
							io.info.is_jal    -> Cat( Fill(52, io.instr16(12)), io.instr16(12), io.instr16(8), io.instr16(10,9), io.instr16(6), io.instr16(7), io.instr16(2), io.instr16(11), io.instr16(5,3), 0.U(1.W)),
							io.info.is_jalr   -> 0.U,
							io.info.is_branch -> Cat( Fill(55, io.instr16(12)), io.instr16(12), io.instr16(6,5), io.instr16(2), io.instr16(11,10), io.instr16(4,3), 0.U(1.W))
						))

}

class PreDecode32() {
	val io = new Bundle {
		val instr32 = Wire( UInt(32.W) )

		val info = Wire(new Info_preDecode)
	}

	io.info.is_rvc := false.B

	io.info.is_jal    := (io.instr32(6,0) === "b1101111".U)
	io.info.is_jalr   := (io.instr32(6,0) === "b1100111".U)
	io.info.is_branch := (io.instr32(6,0) === "b1100011".U)
	io.info.is_call   := ( io.info.is_jal | io.info.is_jalr ) & ( io.instr32(11,7) === BitPat("b00?01") ) //is 1 or 5
	io.info.is_return := io.info.is_jalr & ( io.instr32(19,15) === BitPat("b00?01") ) & (io.instr32(19,15) =/= io.instr32(11,7))
	io.info.is_fencei := ( io.instr32 === BitPat("b?????????????????001?????0001111") )
	io.info.imm       := MuxCase( 0.U, Array(
							io.info.is_jal    -> Cat( Fill(44, io.instr32(31)), io.instr32(19,12), io.instr32(20), io.instr32(30,21), 0.U(1.W) ),
							io.info.is_jalr   -> Cat( Fill(52, io.instr32(31)), io.instr32(31,20) ),
							io.info.is_branch -> Cat( Fill(52, io.instr32(31)), io.instr32(7), io.instr32(30,25), io.instr32(11,8), 0.U(1.W) )
						))

}

trait PreDecode {

	val ibuf_pop = Vec(4, UInt(16.W))
	val preDecode_info = Vec(2, new Info_preDecode)


	val pd16 = for ( i <- 0 until 2 ) yield { val mdl = new PreDecode16(); mdl }
	val pd32 = for ( i <- 0 until 2 ) yield { val mdl = new PreDecode32(); mdl }


	def is_1st00 = ~is_1st16 & ~is_1st32

	def is_1st16: Bool // = (ibuf_pop(0).valid === true.B) & ibuf_pop(0).bits(1,0) =/= "b11".U

	def is_1st32: Bool
	
	// {
	// 	(ibuf_pop(0).valid === true.B) &
	// 	(ibuf_pop(1).valid === true.B) &
	// 	(ibuf_pop(0).bits(1,0) === "b11".U)
	// }

	def idx_2nd = Mux1H(Seq(
		is_1st00 -> 1.U,	//dontcare
		is_1st16 -> 1.U,
		is_1st32 -> 2.U
	))

	def is_2nd00 = {
		(is_1st00) |
		(~is_2nd16 & ~is_2nd32)
	}
	def is_2nd16: Bool // = (ibuf_pop(idx_2nd).valid === true.B) & ibuf_pop(idx_2nd).bits(1,0) =/= "b11".U

	def is_2nd32: Bool
	//  = {
	// 	(ibuf_pop(idx_2nd).valid === true.B) &
	// 	(ibuf_pop(idx_2nd+1.U).valid === true.B) &
	// 	(ibuf_pop(idx_2nd).bits(1,0) === "b11".U)
	// }

	def is_00p00 = is_2nd00 & is_1st00
	def is_00p16 = is_2nd00 & is_1st16
	def is_00p32 = is_2nd00 & is_1st32
	def is_16p16 = is_2nd16 & is_1st16
	def is_16p32 = is_2nd16 & is_1st32
	def is_32p16 = is_2nd32 & is_1st16
	def is_32p32 = is_2nd32 & is_1st32




	pd16(0).io.instr16 := ibuf_pop(0)
	pd32(0).io.instr32 := Cat( ibuf_pop(1), ibuf_pop(0))

	pd16(1).io.instr16 := ibuf_pop(idx_2nd)
	pd32(1).io.instr32 := Cat( ibuf_pop(idx_2nd+1.U), ibuf_pop(idx_2nd))


	// info(0).valid := is_1st16 | is_1st32
	// info(1).valid := is_2nd16 | is_2nd32

	// ibuf_pop(0).ready := Mux1H(Seq(
	// 			is_00p00 -> false.B,
	// 			is_00p16 -> info(0).ready,
	// 			is_16p16 -> info(0).ready,
	// 			is_32p16 -> info(0).ready,
	// 			is_00p32 -> info(0).ready,
	// 			is_16p32 -> info(0).ready,
	// 			is_32p32 -> info(0).ready
	// ))

	// ibuf_pop(1).ready := Mux1H(Seq(
	// 			is_00p00 -> false.B,
	// 			is_00p16 -> false.B,
	// 			is_16p16 -> info(1).ready,
	// 			is_32p16 -> info(1).ready,
	// 			is_00p32 -> info(0).ready,
	// 			is_16p32 -> info(0).ready,
	// 			is_32p32 -> info(0).ready
	// ))

	// ibuf_pop(2).ready := Mux1H(Seq(
	// 			is_00p00 -> false.B,
	// 			is_00p16 -> false.B,
	// 			is_16p16 -> false.B,
	// 			is_32p16 -> info(1).ready,
	// 			is_00p32 -> false.B,
	// 			is_16p32 -> info(1).ready,
	// 			is_32p32 -> info(1).ready
	// ))

	// ibuf_pop(3).ready := Mux1H(Seq(
	// 			is_00p00 -> false.B,
	// 			is_00p16 -> false.B,
	// 			is_16p16 -> false.B,
	// 			is_32p16 -> false.B,
	// 			is_00p32 -> false.B,
	// 			is_16p32 -> false.B,
	// 			is_32p32 -> info(1).ready
	// ))

	preDecode_info(0) := Mux( is_1st16, pd16(0).io.info, pd32(0).io.info ) //1st00 will not be care
	preDecode_info(1) := Mux( is_2nd16, pd16(1).io.info, pd32(1).io.info ) //2nd00 will not be care
}


