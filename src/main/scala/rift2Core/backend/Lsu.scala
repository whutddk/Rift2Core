/*
* @Author: Ruige Lee
* @Date:   2021-03-29 14:37:18
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-29 14:39:29
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

class Dcache( dw: Int, bk: Int, cb: Int, cl: Int ) {

}


class Lsu extends Module {
	val io = IO(new Bundle{
		val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
		val lsu_exe_iwb = new DecoupledIO(new Exe_iwb_info)

		val flush = Input(Bool())
	})

	val iwb_valid = Reg(Bool())
	val iwb_res = Reg(UInt(64.W))
	val iwb_rd0 = Reg(UInt(7.W))

	def iss_ack = io.lsu_exe_iwb.valid & io.lsu_exe_iwb.ready
	def iwb_ack = io.lsu_exe_iwb.valid & io.lsu_exe_iwb.ready

	val dcache = new Dcache(128, 1, 4, 64)



}
