/*
* @Author: Ruige Lee
* @Date:   2021-03-29 14:38:05
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-29 14:39:39
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

package rift2Core.backend


import chisel3._
import chisel3.util._
import rift2Core.basicElement._

class Bru extends Module {
	val io = IO(new Bundle{
		val bru_iss_exe = Flipped(new DecoupledIO(new Bru_iss_info))
		val bru_exe_iwb = new DecoupledIO(new Exe_iwb_info)

		val bru_iq_info = Output(new Info_bru_id)

		val flush = Input(Bool())
	})

	val iwb_valid = Reg(Bool())
	val iwb_res = Reg(UInt(64.W))
	val iwb_rd0 = Reg(UInt(7.W))

	val jalr_valid = Reg(Bool())
	val jalr_pc = Reg(UInt(64.W))

	def iwb_ack = io.bru_exe_iwb.valid & io.bru_exe_iwb.ready


	val is_takenBranch_valid = Reg(Bool())
	val is_takenBranch_bits = Reg(Bool())

	def op1 = io.bru_iss_exe.bits.param.op1
	def op2 = io.bru_iss_exe.bits.param.op2
	
	def is_branchTaken = MuxCase(DontCare, Array(
		io.bru_iss_exe.bits.fun.beq  -> (op1 === op2),
		io.bru_iss_exe.bits.fun.bne  -> (op1 =/= op2),
		io.bru_iss_exe.bits.fun.blt  -> (op1.asSInt < op2.asSInt),
		io.bru_iss_exe.bits.fun.bge  -> (op1.asSInt >= op2.asSInt),
		io.bru_iss_exe.bits.fun.bltu -> (op1 <  op2),
		io.bru_iss_exe.bits.fun.bgeu -> (op1 >= op2)
	))



	when(reset.asBool() | io.flush) {
		iwb_valid := false.B
		iwb_res := 0.U
		iwb_rd0 := 0.U

		jalr_valid := false.B
		jalr_pc := 0.U

		is_takenBranch_valid := false.B
		is_takenBranch_bits := false.B
	}
	.elsewhen(io.bru_iss_exe.valid) {
		iwb_valid := true.B
		iwb_res := io.bru_iss_exe.bits.param.pc + Mux( io.bru_iss_exe.bits.param.is_rvc, 2.U, 4.U)
		iwb_rd0 := Cat( io.bru_iss_exe.bits.param.rd0_idx, io.bru_iss_exe.bits.param.rd0_raw ) 

		jalr_valid := io.bru_iss_exe.bits.fun.jalr
		jalr_pc := io.bru_iss_exe.bits.param.op1 + io.bru_iss_exe.bits.param.imm

		is_takenBranch_valid := io.bru_iss_exe.bits.fun.is_branch
		is_takenBranch_bits := is_branchTaken		
	}
	.elsewhen( ~io.bru_iss_exe.valid & iwb_ack) {
		iwb_valid := false.B
		jalr_valid := false.B
		is_takenBranch_valid := false.B
	}



	io.bru_iss_exe.ready := iwb_ack
	io.bru_exe_iwb.valid := iwb_valid
	io.bru_exe_iwb.bits.res := iwb_res
	io.bru_exe_iwb.bits.rd0_raw := iwb_rd0(4,0)
	io.bru_exe_iwb.bits.rd0_idx := iwb_rd0(6,5)

	io.bru_iq_info.is_takenBranch_bits := is_takenBranch_bits
	io.bru_iq_info.is_takenBranch_valid := is_takenBranch_valid
	io.bru_iq_info.jalr_pc := jalr_pc
	io.bru_iq_info.jalr_valid := jalr_valid

}

