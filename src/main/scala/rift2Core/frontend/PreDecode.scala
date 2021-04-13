
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



class PreDecode16() extends Module{
	val io = IO(new Bundle {
		val instr16 = Input( UInt(16.W) )

		val info = Output(new Info_preDecode)
	})

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

class PreDecode32() extends Module{
	val io = IO(new Bundle {
		val instr32 = Input( UInt(32.W) )

		val info = Output(new Info_preDecode)
	})

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




