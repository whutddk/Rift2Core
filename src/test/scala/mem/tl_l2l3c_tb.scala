/*
* @Author: Ruige Lee
* @Date:   2021-05-07 17:28:10
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-05-07 17:37:11
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

package test.mem

import chisel3._
import chisel3.util._

import axi._
import rift2Core.cache._
import tilelink._


class Tl_l2l3c_tb extends Module {
  val io = IO( new Bundle {
	val is_il1_req = Input(Bool())
	val il1_a_info = Input(new TLchannel_a(128, 32))

	val is_dl1_req = Input(Bool())
	val dl1_a_info = Input(new TLchannel_a(128, 32))
	
	val il1_data = Output(UInt(128.W))
	val dl1_data = Output(UInt(128.W))
  })

	val il1_tb =  Module( new TileLink_mst_heavy(128, 32, 0))
	val dl1_tb =  Module( new TileLink_mst_heavy(128, 32, 1))

	il1_tb.io.is_req := io.is_il1_req
	il1_tb.io.a_info := io.il1_a_info

	dl1_tb.io.is_req := io.is_dl1_req
	dl1_tb.io.a_info := io.dl1_a_info


	io.il1_data := il1_tb.io.d.bits.data
	io.dl1_data := dl1_tb.io.d.bits.data


	val l2c = Module(new L2Cache)
	val l3c = Module( new L3Cache )
	val mem = Module( new AXI_full_mem(128, 12) )




	l2c.io.il1_chn_a <> il1_tb.io.a
	l2c.io.il1_chn_d <> il1_tb.io.d
	l2c.io.dl1_chn_a <> dl1_tb.io.a
	l2c.io.dl1_chn_d <> dl1_tb.io.d
	l2c.io.l2c_chn_a <> l3c.io.l2c_chn_a
	l2c.io.l2c_chn_d <> l3c.io.l2c_chn_d
	l2c.io.l2c_fence_req := false.B

	mem.io.mem_chn_ar <> l3c.io.mem_chn_ar
	mem.io.mem_chn_r <> l3c.io.mem_chn_r
	mem.io.mem_chn_aw <> l3c.io.mem_chn_aw
	mem.io.mem_chn_w <> l3c.io.mem_chn_w
	mem.io.mem_chn_b <> l3c.io.mem_chn_b


	l3c.io.l3c_fence_req := false.B



}


import chiseltest._
import org.scalatest._
import chisel3.iotesters._







class l2l3_tester(dut: Tl_l2l3c_tb) extends PeekPokeTester(dut){

	poke(dut.io.is_il1_req, false.B)
	poke(dut.io.il1_a_info.address, 0.U )
	poke(dut.io.il1_a_info.corrupt, false.B)
	poke(dut.io.il1_a_info.data, 0.U)
	poke(dut.io.il1_a_info.mask, "hffff".U)
	poke(dut.io.il1_a_info.opcode, 0.U)
	poke(dut.io.il1_a_info.param, 0.U)
	poke(dut.io.il1_a_info.size, 4.U)
	poke(dut.io.il1_a_info.source, 0.U)

	poke(dut.io.is_dl1_req, false.B)
	poke(dut.io.dl1_a_info.address, 0.U )
	poke(dut.io.dl1_a_info.corrupt, false.B)
	poke(dut.io.dl1_a_info.data, 0.U)
	poke(dut.io.dl1_a_info.mask, "hffff".U)
	poke(dut.io.dl1_a_info.opcode, 0.U)
	poke(dut.io.dl1_a_info.param, 0.U)
	poke(dut.io.dl1_a_info.size, 4.U)
	poke(dut.io.dl1_a_info.source, 0.U)
	step(10)


	poke(dut.io.is_dl1_req, true.B)
	poke(dut.io.dl1_a_info.address, 0.U )
	poke(dut.io.dl1_a_info.data, 0.U)
	poke(dut.io.dl1_a_info.opcode, 0.U)
	poke(dut.io.dl1_a_info.size, 4.U)
	step(1)
	poke(dut.io.is_dl1_req, false.B)
	step(200)

	poke(dut.io.is_dl1_req, true.B)
	poke(dut.io.dl1_a_info.address, 16.U )
	poke(dut.io.dl1_a_info.data, 1.U)
	poke(dut.io.dl1_a_info.opcode, 0.U)
	poke(dut.io.dl1_a_info.size, 4.U)
	step(1)
	poke(dut.io.is_dl1_req, false.B)
	step(200)

	poke(dut.io.is_dl1_req, true.B)
	poke(dut.io.dl1_a_info.address, 32.U )
	poke(dut.io.dl1_a_info.data, 2.U)
	poke(dut.io.dl1_a_info.opcode, 0.U)
	poke(dut.io.dl1_a_info.size, 4.U)
	step(1)
	poke(dut.io.is_dl1_req, false.B)
	step(200)

	poke(dut.io.is_dl1_req, true.B)
	poke(dut.io.dl1_a_info.address, 48.U )
	poke(dut.io.dl1_a_info.data, 3.U)
	poke(dut.io.dl1_a_info.opcode, 0.U)
	poke(dut.io.dl1_a_info.size, 4.U)
	step(1)
	poke(dut.io.is_dl1_req, false.B)
	step(200)

	poke(dut.io.is_dl1_req, true.B)
	poke(dut.io.dl1_a_info.address, 64.U )
	poke(dut.io.dl1_a_info.data, 4.U)
	poke(dut.io.dl1_a_info.opcode, 0.U)
	poke(dut.io.dl1_a_info.size, 4.U)
	step(1)
	poke(dut.io.is_dl1_req, false.B)
	step(200)

	poke(dut.io.is_dl1_req, true.B)
	poke(dut.io.dl1_a_info.address, 0.U )
	poke(dut.io.dl1_a_info.data, 4.U)
	poke(dut.io.dl1_a_info.opcode, 0.U)
	poke(dut.io.dl1_a_info.size, 5.U)
	step(1)
	poke(dut.io.is_dl1_req, false.B)
	step(200)

	poke(dut.io.is_dl1_req, true.B)
	poke(dut.io.dl1_a_info.address, 32.U )
	poke(dut.io.dl1_a_info.data, 4.U)
	poke(dut.io.dl1_a_info.opcode, 0.U)
	poke(dut.io.dl1_a_info.size, 5.U)
	step(1)
	poke(dut.io.is_dl1_req, false.B)
	step(200)

	step(1000)

}

class Waveforml2l3 extends FlatSpec with Matchers {
  "WaveformSpec" should "pass" in {
	chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on","--target-dir", "generated"), () => new Tl_l2l3c_tb()){
	  c => new l2l3_tester(c)
	} should be (true)
  }
}

