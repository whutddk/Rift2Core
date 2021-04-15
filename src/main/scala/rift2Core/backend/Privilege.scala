
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





trait M_Privilege {

	val csrFiles: CsrFiles
	val is_trap: Vec[Bool]
	val is_xRet: Vec[Bool]

	val is_Mret = Wire( Vec(2, Bool()) )


	def mstatus_except = Cat(
		0.U(51.W),
		"b11".U(2.W), //MPP
		0.U(3.W),
		Mux(is_trap.contains(true.B), csrFiles.m_csrFiles.mstatus.value(3), Mux( is_Mret.contains(true.B), 1.U(1.W), 0.U(1.W) )),//MPIE
		0.U(3.W),
		Mux(is_trap.contains(true.B), 0.U(1.W), Mux( is_Mret.contains(true.B), csrFiles.m_csrFiles.mstatus.value(7), 0.U(1.W) )),//MIE
		0.U(3.W)
		)





}

trait H_Privilege {

}

trait  S_Privilege {
	val is_Sret = Wire( Vec(2, Bool()) )

}

trait U_Privilege {
	val is_Uret = Wire( Vec(2, Bool()) )

}


abstract class Privilege() extends Module with M_Privilege with H_Privilege with S_Privilege with U_Privilege{
	val csrFiles: CsrFiles


	csrFiles.m_csrFiles.clint_csr_info := 0.U.asTypeOf(new Info_clint_csr)

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

	val is_trap = Wire( Vec(2, Bool()) )
	val is_xRet = Wire(Vec(2, Bool()))



	def is_exInterrupt = csrFiles.m_csrFiles.mip.value(11) & csrFiles.m_csrFiles.mie.value(11) & csrFiles.m_csrFiles.mstatus.value(3)
	def is_timeInterrupt = csrFiles.m_csrFiles.mip.value(7) & csrFiles.m_csrFiles.mie.value(7) & csrFiles.m_csrFiles.mstatus.value(3)
	def is_softInterrupt = csrFiles.m_csrFiles.mip.value(3) & csrFiles.m_csrFiles.mie.value(3) & csrFiles.m_csrFiles.mstatus.value(3)



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

	for ( i <- 0 until 2 ) yield {
		is_trap(i) := is_interrupt | is_exception(i)
		is_xRet(i) := is_Mret(i) | is_Sret(i) | is_Uret(i)
	}



//
// only one exception will be accept once
//
//
//
//

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

	def is_interrupt = is_exInterrupt | is_timeInterrupt | is_softInterrupt;

	def mepc_except = MuxCase( 0.U, Array( is_exception.contains(true.B) -> commit_pc, is_interrupt -> commit_pc ) )
	def mtval_except = 
		Mux( 
			(is_load_accessFault_ack.contains(true.B) | 
			is_store_accessFault_ack.contains(true.B) | 
			is_load_misAlign_ack.contains(true.B) | 
			is_store_misAlign_ack.contains(true.B) ), 
			lsu_trap_addr, 0.U)

}


