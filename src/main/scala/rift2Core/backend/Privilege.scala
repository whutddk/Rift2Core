
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


abstract class Privilege() extends Module with CsrFiles{

	
	val commit_pc = Wire(UInt(64.W))
	val is_load_accessFault_ack = Wire( Vec(2, Bool()) )
	val is_store_accessFault_ack = Wire( Vec(2, Bool()) )
	val is_load_misAlign_ack = Wire( Vec(2, Bool()) )
	val is_store_misAlign_ack = Wire( Vec(2, Bool()) )

	val is_ecall = Wire( Vec(2, Bool()) )
	val is_ebreak = Wire( Vec(2, Bool()) )
	val is_instr_accessFault = Wire( Vec(2, Bool()) )
	val is_illeage = Wire( Vec(2, Bool()) )

	val lsu_trap_addr = Wire(UInt(64.W))

	val is_trap = Wire( Vec(2,Bool() ))
	val is_xRet = Wire(Vec(2, Bool()))





	object mp {

		val is_Mret = Wire( Vec(2, Bool()) )

	}

	object hp {

	}

	object sp {
		val is_Sret = Wire( Vec(2, Bool()) )

	}

	object up {
		val is_Uret = Wire( Vec(2, Bool()) )

	}

	object csr_access {

		val exe_port = Wire(new Exe_Port)

		m_csrFiles.clint_csr_info := 0.U.asTypeOf(new Info_clint_csr)

		m_csrFiles.exe_port := exe_port
		h_csrFiles.exe_port := exe_port
		s_csrFiles.exe_port := exe_port
		u_csrFiles.exe_port := exe_port
		d_csrFiles.exe_port := exe_port


		m_csrFiles.mcause_pri_port.en  := is_trap.contains(true.B)
		m_csrFiles.mcause_pri_port.dat := 
										Cat(
											is_interrupt.asUInt, 
											MuxCase( 0.U(63.W), Array(
													is_ecall.contains(true.B)        -> 11.U(63.W),
													is_ebreak.contains(true.B)                -> 3.U(63.W),
													is_instr_accessFault.contains(true.B)     -> 1.U(63.W),
													is_illeage.contains(true.B)               -> 2.U(63.W),
													is_load_accessFault_ack.contains(true.B)  -> 5.U(63.W),
													is_store_accessFault_ack.contains(true.B) -> 7.U(63.W),
													is_load_misAlign_ack.contains(true.B)     -> 4.U(63.W),
													is_store_misAlign_ack.contains(true.B)	 -> 6.U(63.W)
												))
										)

		m_csrFiles.mepc_pri_port.en  := is_trap.contains(true.B)
		m_csrFiles.mepc_pri_port.dat := commit_pc
		//MuxCase( 0.U, Array( is_exception.contains(true.B) -> commit_pc, is_interrupt -> commit_pc ) )

		m_csrFiles.mtval_pri_port.en  := is_trap.contains(true.B)
		m_csrFiles.mtval_pri_port.dat := Mux( 
												(is_load_accessFault_ack.contains(true.B) | 
												is_store_accessFault_ack.contains(true.B) | 
												is_load_misAlign_ack.contains(true.B) | 
												is_store_misAlign_ack.contains(true.B) ), 
												lsu_trap_addr, 0.U
											)

		m_csrFiles.mstatus_pri_port.en  := is_trap.contains(true.B) | is_xRet.contains(true.B)
		m_csrFiles.mstatus_pri_port.dat := Cat(
											0.U(51.W),
											"b11".U, //MPP
											0.U(3.W),
											(is_trap.contains(true.B) & m_csrFiles.mstatus(3)) | (is_xRet.contains(true.B) & true.B), //MPIE
											0.U(3.W),
											(is_trap.contains(true.B) & false.B) | (is_xRet.contains(true.B) & m_csrFiles.mstatus(7)), //MIE
											0.U(3.W)
										)


	}






















	def is_exInterrupt   = m_csrFiles.mip(11) & m_csrFiles.mie(11) & m_csrFiles.mstatus(3)
	def is_timeInterrupt = m_csrFiles.mip(7)  & m_csrFiles.mie(7)  & m_csrFiles.mstatus(3)
	def is_softInterrupt = m_csrFiles.mip(3)  & m_csrFiles.mie(3)  & m_csrFiles.mstatus(3)



	val is_exception = 	WireDefault(	VecInit( for (i <- 0 until 2) yield {
							is_ecall(i)  |
							is_ebreak(i)  |
							is_instr_accessFault(i)  |
							is_illeage(i) |
							is_load_accessFault_ack(i)  |
							is_store_accessFault_ack(i) |
							is_load_misAlign_ack(i)  |
							is_store_misAlign_ack(i)
						}
						))


	for ( i <- 0 until 2 ) yield {
		is_trap(i) := is_interrupt | is_exception(i)
		is_xRet(i) := mp.is_Mret(i) | sp.is_Sret(i) | up.is_Uret(i)
	}



//
// only one exception will be accept once
//
//
//
//



	def is_interrupt = is_exInterrupt | is_timeInterrupt | is_softInterrupt;


	def mtval_except = 
				Mux( 
					(is_load_accessFault_ack.contains(true.B) | 
					is_store_accessFault_ack.contains(true.B) | 
					is_load_misAlign_ack.contains(true.B) | 
					is_store_misAlign_ack.contains(true.B) ), 
					lsu_trap_addr, 0.U)

}


