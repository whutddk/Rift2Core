
/*
* @Author: Ruige Lee
* @Date:   2021-05-06 16:02:23
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-05-06 16:02:42
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

class Tl_l3c_tb extends Module{
  val io = IO( new Bundle {
    val is_req = Input(Bool())
    val a_info = Input(new TLchannel_a(128, 32))

    val data = Output(UInt(128.W))

  })

  val l2c_tb =  Module( new TileLink_mst_heavy(128, 32, 1))
  val l3c = Module( new L3Cache )
  val mem = Module( new AXI_full_mem(128, 12) )

  io.data := l2c_tb.io.d.bits.data

  l2c_tb.io.is_req := io.is_req
  l2c_tb.io.a_info := io.a_info



  mem.io.mem_chn_ar <> l3c.io.mem_chn_ar
  mem.io.mem_chn_r <> l3c.io.mem_chn_r
  mem.io.mem_chn_aw <> l3c.io.mem_chn_aw
  mem.io.mem_chn_w <> l3c.io.mem_chn_w
  mem.io.mem_chn_b <> l3c.io.mem_chn_b

  l3c.io.l2c_chn_a <> l2c_tb.io.a
  l3c.io.l2c_chn_d <> l2c_tb.io.d

  l3c.io.l3c_fence_req := false.B
  // l3c.io.l3c_fence_end

}

import chiseltest._
import org.scalatest._
import chisel3.iotesters._

class WaveformTester(dut: Tl_l3c_tb) extends PeekPokeTester(dut){



  poke(dut.io.is_req, false.B)
  poke(dut.io.a_info.address, 0.U )
  poke(dut.io.a_info.corrupt, false.B)
  poke(dut.io.a_info.data, 0.U)
  poke(dut.io.a_info.mask, "hffff".U)
  poke(dut.io.a_info.opcode, 0.U)
  poke(dut.io.a_info.param, 0.U)
  poke(dut.io.a_info.size, 4.U)
  poke(dut.io.a_info.source, 0.U)


  step(10)
  poke(dut.io.is_req, true.B)
  poke(dut.io.a_info.address, 0.U )
  poke(dut.io.a_info.data, 0.U)
  poke(dut.io.a_info.opcode, 0.U)
  poke(dut.io.a_info.size, 4.U)
  step(1)
  poke(dut.io.is_req, false.B)

  step(200)
  poke(dut.io.is_req, true.B)
  poke(dut.io.a_info.address, 16.U )
  poke(dut.io.a_info.data, 1.U)
  poke(dut.io.a_info.opcode, 0.U)
  poke(dut.io.a_info.size, 4.U)
  step(1)
  poke(dut.io.is_req, false.B)

  step(100)
  poke(dut.io.is_req, true.B)
  poke(dut.io.a_info.address, 32.U )
  poke(dut.io.a_info.data, 2.U)
  poke(dut.io.a_info.opcode, 0.U)
  poke(dut.io.a_info.size, 4.U)
  step(1)
  poke(dut.io.is_req, false.B)

  step(100)
  poke(dut.io.is_req, true.B)
  poke(dut.io.a_info.address, 48.U )
  poke(dut.io.a_info.data, 3.U)
  poke(dut.io.a_info.opcode, 0.U)
  poke(dut.io.a_info.size, 4.U)
  step(1)
  poke(dut.io.is_req, false.B)


  step(100)
  poke(dut.io.is_req, true.B)
  poke(dut.io.a_info.address, 0.U )
  poke(dut.io.a_info.data, 0.U)
  poke(dut.io.a_info.opcode, 4.U)
  poke(dut.io.a_info.size, 7.U)
  step(1)
  poke(dut.io.is_req, false.B)



  step(100)
  poke(dut.io.is_req, true.B)
  poke(dut.io.a_info.address, 32.U )
  poke(dut.io.a_info.data, "haa".U)
  poke(dut.io.a_info.opcode, 3.U)
  poke(dut.io.a_info.param, 3.U)
  poke(dut.io.a_info.size, 4.U)
  step(1)
  poke(dut.io.is_req, false.B)

  step(100)
  poke(dut.io.is_req, true.B)
  poke(dut.io.a_info.address, 0.U )
  poke(dut.io.a_info.data, 0.U)
  poke(dut.io.a_info.opcode, 4.U)
  poke(dut.io.a_info.size, 7.U)
  step(1)
  poke(dut.io.is_req, false.B)


  step(1000)

}

class WaveformSpec extends FlatSpec with Matchers {
  "WaveformSpec" should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on","--target-dir", "generated"), () => new Tl_l3c_tb()){
      c => new WaveformTester(c)
    } should be (true)
  }
}


