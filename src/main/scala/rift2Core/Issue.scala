/*
* @Author: Ruige Lee
* @Date:   2021-03-25 17:55:52
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-25 17:57:46
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

package rift2Core


import chisel3._
import chisel3.util._
import rift2Core.basicElement._
import javax.swing.UIManager

abstract class Ele_issue(log: Vec[Vec[UInt]], dp: Int) {

	// check if the rs is wrote back
	def is_rs_ready( raw: UInt, idx: UInt ): Bool = (log(raw)(idx) === 2.U) | (raw === 0.U)

	// check if an instruction is RAW clearence, each instruction has different rs requirement
	def is_clearRAW( i: UInt ): Bool

	// check if there is a valid item can be issue in dpt buf or dpt fifo
	def issue_valid: Bool = {
		var res = false.B
		for ( i <- 0 until dp ) {
			res = res | is_clearRAW(i.U)
		}
		return res
	}

	// get one item idx that can be issue
	def switch_idx: UInt = {
		var res = 0.U
		var set = 0
		for ( i <- 0 until dp ) {
			if ( is_clearRAW(i.U) == true.B ) {
				if ( set == 1 ) {
					res = i.U
				}
				set = 1
			}
		}
		return res
	}
}



class Alu_issue(alu_dpt_info: Vec[Alu_dpt_info], buf_valid: Vec[Bool], log: Vec[Vec[UInt]], dp: Int) extends Ele_issue(log, dp) {
	val alu_iss_info = Wire(new Alu_iss_info)

	def rs1_raw(i: UInt) = alu_dpt_info(i).param.rs1_raw
	def rs2_raw(i: UInt) = alu_dpt_info(i).param.rs2_raw
	def rs1_idx(i: UInt) = alu_dpt_info(i).rn.rs1_idx
	def rs2_idx(i: UInt) = alu_dpt_info(i).rn.rs2_idx



	override def is_clearRAW( i: UInt ) = 
		if ( buf_valid(i) == false.B ) {
			false.B
		}
		else {
			MuxCase(false.B, Array(
				( alu_dpt_info(i).isa.lui   === true.B) -> true.B,
				( alu_dpt_info(i).isa.auipc === true.B) -> true.B,
				( alu_dpt_info(i).isa.addi  === true.B) -> is_rs_ready(rs1_raw(i), rs1_idx(i)),
				( alu_dpt_info(i).isa.addiw === true.B) -> is_rs_ready(rs1_raw(i), rs1_idx(i)),
				( alu_dpt_info(i).isa.slti  === true.B) -> is_rs_ready(rs1_raw(i), rs1_idx(i)),
				( alu_dpt_info(i).isa.sltiu === true.B) -> is_rs_ready(rs1_raw(i), rs1_idx(i)),
				( alu_dpt_info(i).isa.xori  === true.B) -> is_rs_ready(rs1_raw(i), rs1_idx(i)),
				( alu_dpt_info(i).isa.ori   === true.B) -> is_rs_ready(rs1_raw(i), rs1_idx(i)),
				( alu_dpt_info(i).isa.andi  === true.B) -> is_rs_ready(rs1_raw(i), rs1_idx(i)),
				( alu_dpt_info(i).isa.slli  === true.B) -> is_rs_ready(rs1_raw(i), rs1_idx(i)),
				( alu_dpt_info(i).isa.slliw === true.B) -> is_rs_ready(rs1_raw(i), rs1_idx(i)),
				( alu_dpt_info(i).isa.srli  === true.B) -> is_rs_ready(rs1_raw(i), rs1_idx(i)),
				( alu_dpt_info(i).isa.srliw === true.B) -> is_rs_ready(rs1_raw(i), rs1_idx(i)),
				( alu_dpt_info(i).isa.srai  === true.B) -> is_rs_ready(rs1_raw(i), rs1_idx(i)),
				( alu_dpt_info(i).isa.sraiw === true.B) -> is_rs_ready(rs1_raw(i), rs1_idx(i)),
				( alu_dpt_info(i).isa.add   === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
				( alu_dpt_info(i).isa.addw  === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
				( alu_dpt_info(i).isa.sub   === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
				( alu_dpt_info(i).isa.subw  === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
				( alu_dpt_info(i).isa.sll   === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
				( alu_dpt_info(i).isa.sllw  === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
				( alu_dpt_info(i).isa.slt   === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
				( alu_dpt_info(i).isa.sltu  === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
				( alu_dpt_info(i).isa.xor   === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
				( alu_dpt_info(i).isa.srl   === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
				( alu_dpt_info(i).isa.srlw  === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
				( alu_dpt_info(i).isa.sra   === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
				( alu_dpt_info(i).isa.sraw  === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
				( alu_dpt_info(i).isa.or    === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
				( alu_dpt_info(i).isa.and   === true.B) -> (is_rs_ready(rs1_raw(i), rs1_idx(i)) | is_rs_ready(rs2_raw(i), rs2_idx(i))),
			))	
		}
	
	{
		val i = switch_idx

		alu_iss_info.fun.imm := alu_dpt_info(i).isa.is_fun_imm
		alu_iss_info.fun.add := alu_dpt_info(i).isa.is_fun_add
		alu_iss_info.fun.sub := alu_dpt_info(i).isa.is_fun_sub
		alu_iss_info.fun.slt := alu_dpt_info(i).isa.is_fun_slt
		alu_iss_info.fun.xor := alu_dpt_info(i).isa.is_fun_xor
		alu_iss_info.fun.or  := alu_dpt_info(i).isa.is_fun_or
		alu_iss_info.fun.and := alu_dpt_info(i).isa.is_fun_and
		alu_iss_info.fun.sll := alu_dpt_info(i).isa.is_fun_sll
		alu_iss_info.fun.srl := alu_dpt_info(i).isa.is_fun_srl
		alu_iss_info.fun.sra := alu_dpt_info(i).isa.is_fun_sra

		alu_iss_info.param.is_32w  := alu_dpt_info(i).isa.is_32w
		alu_iss_info.param.is_usi  := alu_dpt_info(i).isa.is_usi
		alu_iss_info.param.is_imm  := alu_dpt_info(i).isa.is_imm
		alu_iss_info.param.reg_raw.rd0_raw := alu_dpt_info(i).param.rd0_raw
		alu_iss_info.param.reg_raw.rs1_raw := alu_dpt_info(i).param.rs1_raw
		alu_iss_info.param.reg_raw.rs2_raw := alu_dpt_info(i).param.rs2_raw

		alu_iss_info.param.reg_idx.rd0_idx := alu_dpt_info(i).rn.rd0_idx
		alu_iss_info.param.reg_idx.rs1_idx := alu_dpt_info(i).rn.rs1_idx
		alu_iss_info.param.reg_idx.rs2_idx := alu_dpt_info(i).rn.rs2_idx

		alu_iss_info.param.pc  := alu_dpt_info(i).param.pc
		alu_iss_info.param.imm := alu_dpt_info(i).param.imm
	}


}

class bru_issue(bru_dpt_info: Bru_dpt_info, buf_valid: Bool, log: Vec[Vec[UInt]]) extends Ele_issue(log, 1) {
	val bru_iss_info = Wire(new Bru_iss_info)

	def rs1_raw = bru_dpt_info.param.rs1_raw
	def rs2_raw = bru_dpt_info.param.rs2_raw
	def rs1_idx = bru_dpt_info.rn.rs1_idx
	def rs2_idx = bru_dpt_info.rn.rs2_idx

	override def is_clearRAW( i: UInt ) = 
		if ( buf_valid(i) == false.B ) {
			false.B
		}
		else {
			MuxCase(false.B, Array(

}

class lsu_issue extends Module {
	val io = IO(new Bundle{
	})
}

class csr_issue extends Module {
	val io = IO(new Bundle{
	})
}

class mul_issue extends Module {
	val io = IO(new Bundle{
	})
}

class fpu_issue extends Module {
	val io = IO(new Bundle{
	})
}

class Issue(alu_dp: Int, mul_dp: Int, fpu_dp: Int) extends Module {
	val io = IO(new Bundle{
		val alu_dpt_iss = Flipped(new DecoupledIO(new Alu_dpt_info))
		val bru_dpt_iss = Flipped(new DecoupledIO(new Bru_dpt_info))
		val lsu_dpt_iss = Flipped(new DecoupledIO(new Lsu_dpt_info))
		val csr_dpt_iss = Flipped(new DecoupledIO(new Csr_dpt_info))
		val mul_dpt_iss = Flipped(new DecoupledIO(new Mul_dpt_info))
		val fpu_dpt_iss = Flipped(new DecoupledIO(new Fpu_dpt_info))


		val alu_iss_exe = new DecoupledIO(new Alu_iss_info)

		val regLog = Vec(32,Vec(4, Input(UInt(2.W))) )


		val flush = Input(Bool())
	})
		//dpt buf here



		//issue mux
		val alu_issue = new Alu_issue (alu_dpt_info: Vec[Alu_dpt_info], buf_valid: Vec[Bool], io.regLog, alu_dp)




		//issue info (exe param) register here
		val alu_iss_info = RegNext(alu_issue.alu_iss_info)
		val alu_iss_valid = Reg(Bool())

		def alu_exe_ack = io.alu_iss_exe.valid & io.alu_iss_exe.ready

		alu_iss_valid := MuxCase(alu_iss_valid, Array( 
									(reset.asBool() | io.flush) -> false.B,
									alu_issue.issue_valid       -> true.B,
									alu_exe_ack        -> false.B
								))

		io.alu_iss_exe.bits  := Mux(alu_iss_valid, alu_iss_info, DontCare)
		io.alu_iss_exe.valid := alu_iss_valid


}

