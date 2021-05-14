
/*
* @Author: Ruige Lee
* @Date:   2021-04-21 14:36:40
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-05-11 10:08:54
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

package tilelink

import chisel3._
import chisel3.util._



class Tl_mem extends BlackBox with HasBlackBoxResource {
	val io = IO(new Bundle{
		val tlslv_a_opcode  = Input(UInt(3.W))
		val tlslv_a_param   = Input(UInt(3.W))
		val tlslv_a_size    = Input(UInt(8.W))
		val tlslv_a_source  = Input(UInt(3.W))
		val tlslv_a_address = Input(UInt(32.W))
		val tlslv_a_mask    = Input(UInt(16.W))
		val tlslv_a_data    = Input(UInt(128.W))
		val tlslv_a_corrupt = Input(Bool())

		val tlslv_a_valid = Input(Bool())
		val tlslv_a_ready = Output(Bool())

		val tlslv_d_opcode  = Output(UInt(3.W))
		val tlslv_d_param   = Output(UInt(2.W))
		val tlslv_d_size    = Output(UInt(8.W))
		val tlslv_d_source  = Output(UInt(3.W))
		val tlslv_d_sink    = Output(UInt(3.W))
		val tlslv_d_denied  = Output(Bool())
		val tlslv_d_data    = Output(UInt(128.W))
		val tlslv_d_corrupt = Output(Bool())

		val tlslv_d_valid   = Output(Bool())
		val tlslv_d_ready   = Input(Bool())

		val clk  = Input(Bool())
		val rst  = Input(Bool())		
	})

	addResource("/tl_mem.v")
}


class Tl_iccm extends Module{
	val io = IO(new Bundle{
		val ccm_chn_a = Flipped(new DecoupledIO(new TLchannel_a(128, 32)))
		val ccm_chn_d = new DecoupledIO( new TLchannel_d(128) )
	})	

	val tl_mem = Module(new Tl_mem)

		tl_mem.io.tlslv_a_opcode  := io.ccm_chn_a.bits.opcode
		tl_mem.io.tlslv_a_param   := io.ccm_chn_a.bits.param
		tl_mem.io.tlslv_a_size    := io.ccm_chn_a.bits.size
		tl_mem.io.tlslv_a_source  := io.ccm_chn_a.bits.source
		tl_mem.io.tlslv_a_address := io.ccm_chn_a.bits.address
		tl_mem.io.tlslv_a_mask    := io.ccm_chn_a.bits.mask
		tl_mem.io.tlslv_a_data    := io.ccm_chn_a.bits.data
		tl_mem.io.tlslv_a_corrupt := io.ccm_chn_a.bits.corrupt


		tl_mem.io.tlslv_a_valid   := io.ccm_chn_a.valid
		io.ccm_chn_a.ready        := tl_mem.io.tlslv_a_ready


		io.ccm_chn_d.bits.opcode := tl_mem.io.tlslv_d_opcode
		io.ccm_chn_d.bits.param := tl_mem.io.tlslv_d_param
		io.ccm_chn_d.bits.size := tl_mem.io.tlslv_d_size
		io.ccm_chn_d.bits.source := tl_mem.io.tlslv_d_source
		io.ccm_chn_d.bits.sink := tl_mem.io.tlslv_d_sink
		io.ccm_chn_d.bits.denied := tl_mem.io.tlslv_d_denied
		io.ccm_chn_d.bits.data := tl_mem.io.tlslv_d_data
		io.ccm_chn_d.bits.corrupt := tl_mem.io.tlslv_d_corrupt

		io.ccm_chn_d.valid := tl_mem.io.tlslv_d_valid
		tl_mem.io.tlslv_d_ready := io.ccm_chn_d.ready


		tl_mem.io.clk  := clock.asBool
		tl_mem.io.rst  := reset.asBool
}

