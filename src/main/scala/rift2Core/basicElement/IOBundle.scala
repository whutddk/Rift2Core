package rift2Core.basicElement

/*
* @Author: Ruige Lee
* @Date:   2021-03-18 16:49:02
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-18 19:42:33
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


class Info_pc_if extends Bundle {
	val fetch_addr = Output(UInt(64.W))
	} 


class Info_if_iq extends Bundle {
	val ic_iq_pc = Output(UInt(64.W))
	val ic_iq_instr = Output(UInt(64.W)) 
}


class Info_iq_id extends Bundle {
	val iq_id_isRVC = Output(Bool())
	val iq_id_pc = Output(UInt(64.W))
	val iq_id_instr = Output(UInt(64.W)) 
}




trait Fpu_isa extends Bundle with Fextend_isa with Qextend_isa {

}

class Info_microInstr extends Bundle {


	val isIFAccessFault = Bool()
	val isIlleage = Bool()

	val isRVC = Bool()
	val pc = UInt(64.W)
	val imm = UInt(64.W)
	val shamt = UInt(6.W)
	val rd0 = UInt(5.W)
	val rs1 = UInt(5.W)
	val rs2 = UInt(5.W)
}


