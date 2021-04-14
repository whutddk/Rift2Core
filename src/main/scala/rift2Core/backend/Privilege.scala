
/*
* @Author: Ruige Lee
* @Date:   2021-04-14 11:30:55
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-14 11:31:24
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

import rift2Core.basic._





trait  M_Privilege {
	assign mstatus_except_in[2:0] = 3'b0;
	assign mstatus_except_in[3] = (isTrap & 1'b0) | (isXRet & mstatus_csr_out[7]); //MIE
	assign mstatus_except_in[6:4] = 3'b0;
	assign mstatus_except_in[7] = (isTrap & mstatus_csr_out[3]) | (isXRet & 1'b1); //MPIE
	assign mstatus_except_in[10:8] = 3'b0;
	assign mstatus_except_in[12:11] = 2'b11; //MPP
	assign mstatus_except_in[63:13] = 51'b0;

	val is_Mret = Wire( Vec(2, Bool()) )


}

trait  H_Privilege {

}

trait  S_Privilege {
	val is_Sret = Wire( Vec(2, Bool()) )

}

trait  U_Privilege {
	val is_Uret = Wire( Vec(2, Bool()) )
}


class Privilege() extends M_Privilege with H_Privilege with S_Privilege with U_Privilege{



	val commit_pc = Wire(Vec(2, UInt(64.W)))
	val is_load_accessFault_ack = Wire( Vec(2, Bool()) )
	val is_store_accessFault_ack = Wire( Vec(2, Bool()) )
	val is_load_misAlign_ack = Wire( Vec(2, Bool()) )
	val is_store_misAlign_ack = Wire( Vec(2, Bool()) )

	val is_ecall = Wire( Vec(2, Bool()) )
	val is_ebreak = Wire( Vec(2, Bool()) )
	val is_instr_accessFault = Wire( Vec(2, Bool()) )
	val is_illeage = Wire( Vec(2, Bool()) )

	val lsu_trap_addr = Wire(UInt(64.W))

	val is_trap = Wire(Bool())
	val is_xRet = Wire(Bool())

	for ( i <- 0 until 2 ) yield {
		is_trap(i) := is_interrupt | is_exception(i)
		is_xRet(i) := is_Mret(i) | is_Sret(i) | is_Uret(i)
	}

	def is_interrupt = is_exInterrupt | is_timeInterrupt | is_softInterrupt;

	val is_exception = VecInit( for ( i <- 0 until 2 ) yield {
									is_ecall(i) |
									is_ebreak(i) |
									is_instr_accessFault(i) |
									is_illeage(i) |
									is_load_accessFault_ack(i) |
									is_store_accessFault_ack(i) |
									is_load_misAlign_ack(i) |
									is_store_misAlign_ack(i)
								})

	def mcause_except = Cat(
							is_interrupt.asUInt, 
							MuxCase( 0.U(63.W), Array(
									is_ecall.contains(true.B)        -> 11.U(63.W),
									is_ebreak(true.B)                -> 3.U(63.W),
									is_instr_accessFault(true.B)     -> 1.U(63.W),
									is_illeage(true.B)               -> 2.U(63.W),
									is_load_accessFault_ack(true.B)  -> 5.U(63.W),
									is_store_accessFault_ack(true.B) -> 7.U(63.W),
									is_load_misAlign_ack(true.B)     -> 4.U(63.W),
									is_store_misAlign_ack(true.B)	 -> 6.U(63.W)
								))
							)

	def mepc_except = MuxCase( 0.U, Array( is_exception -> commit_pc, is_interrupt -> commit_pc ) )
	def mtval_except = 
		Mux( 
			(is_load_accessFault_ack.contains(true.B) | 
			is_store_accessFault_ack.contains(true.B) | 
			is_load_misAlign_ack.contains(true.B) | 
			is_store_misAlign_ack.contains(true.B) ), 
			lsu_trap_addr, 0.U)

}

