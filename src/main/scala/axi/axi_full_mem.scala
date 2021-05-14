
/*
* @Author: Ruige Lee
* @Date:   2021-05-06 15:25:47
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-05-06 15:26:04
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

package axi

import chisel3._
import chisel3.util._

import base._

class AXI_full_mem(dw: Int, aw: Int) extends Module {
	val io = IO( new Bundle {
		val mem_chn_ar = Flipped(new DecoupledIO(new AXI_chn_a( 32, 1, 1 )))
		val mem_chn_r =  new DecoupledIO(new AXI_chn_r( dw, 1, 1))

		val mem_chn_aw = Flipped( new DecoupledIO(new AXI_chn_a( 32, 1, 1 )) )
		val mem_chn_w = Flipped( new DecoupledIO(new AXI_chn_w( dw, 1 )) )
		val mem_chn_b = new DecoupledIO(new AXI_chn_b( 1, 1 ))
	})

	val mem_slv_r = Module(new AXI_slv_r(32, dw))
	val mem_slv_w = Module(new AXI_slv_w(32, dw))
	val ram =  Module(new Sram(dw, aw))


	io.mem_chn_ar <> mem_slv_r.io.ar
	io.mem_chn_r  <> mem_slv_r.io.r

	io.mem_chn_aw <> mem_slv_w.io.aw
	io.mem_chn_w  <> mem_slv_w.io.w
	io.mem_chn_b  <> mem_slv_w.io.b


	mem_slv_r.io.r_info.id   := 0.U
	mem_slv_r.io.r_info.data := ram.io.data_r
	mem_slv_r.io.r_info.rsp  := 0.U
	mem_slv_r.io.r_info.last := DontCare
	mem_slv_r.io.r_info.user := 0.U




	ram.io.data_w := mem_slv_w.io.w.bits.data
	ram.io.addr_w := mem_slv_w.io.req_addr
	ram.io.data_wstrb := mem_slv_w.io.w.bits.strb
	ram.io.en_w   := mem_slv_w.io.w.fire

	ram.io.addr_r := mem_slv_r.io.req_addr
	ram.io.en_r   := mem_slv_r.io.is_busy


}

class AXI_sim_mem (dw: Int, aw: Int) extends AXI_full_mem(dw, aw) {
	override val ram = Module(new Sram(dw, aw))

	io.mem_chn_ar <> mem_slv_r.io.ar
	io.mem_chn_r  <> mem_slv_r.io.r

	io.mem_chn_aw <> mem_slv_w.io.aw
	io.mem_chn_w  <> mem_slv_w.io.w
	io.mem_chn_b  <> mem_slv_w.io.b


	mem_slv_r.io.r_info.id   := 0.U
	mem_slv_r.io.r_info.data := ram.io.data_r
	mem_slv_r.io.r_info.rsp  := 0.U
	mem_slv_r.io.r_info.last := DontCare
	mem_slv_r.io.r_info.user := 0.U




	ram.io.data_w := mem_slv_w.io.w.bits.data
	ram.io.addr_w := mem_slv_w.io.req_addr
	ram.io.data_wstrb := mem_slv_w.io.w.bits.strb
	ram.io.en_w   := mem_slv_w.io.w.fire

	ram.io.addr_r := mem_slv_r.io.req_addr
	ram.io.en_r   := mem_slv_r.io.is_busy
}
