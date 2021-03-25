package rift2Core.basicElement

/*
* @Author: Ruige Lee
* @Date:   2021-03-18 16:49:02
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-25 11:23:47
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
	val pc = Output(UInt(64.W))
	val instr = Output(UInt(64.W)) 
}

class Info_iq_id extends Bundle {
	val isRVC = Output(Bool())
	val pc = Output(UInt(64.W))
	val instr = Output(UInt(32.W)) 
}

class Info_id_dpt extends Bundle {

	val info = new Instruction_info()
	val isIFAccessFault = Bool()
	val isIlleage = Bool()

	val isRVC = Bool()


}

class Info_dpt_iss extends Bundle {

}
