


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


package rift2Core.privilege

import chisel3._
import chisel3.util._

import rift2Core.define._
import rift2Core.backend._


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


	val exe_port = Wire(new Exe_Port)





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


