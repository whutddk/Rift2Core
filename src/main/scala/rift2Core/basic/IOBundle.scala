

/*
* @Author: Ruige Lee
* @Date:   2021-03-18 16:49:02
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-19 17:54:17
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

package rift2Core.basic

import chisel3._
import chisel3.util._


class Info_pc_if extends Bundle {
	val addr = UInt(64.W)
} 

// class Info_if_iq extends Bundle {
// 	val pc = UInt(64.W)
// 	val instr = UInt(128.W)
// }








class Info_dpt_reg extends Bundle {
	val opr = Vec(32, Vec(4, Output(Bool())))
	val ptr = Vec(32, Input(UInt(2.W)))
	val log = Vec(32,Vec(4, Input(UInt(2.W))) )
}


class Info_dpt_iss extends Bundle {

}
