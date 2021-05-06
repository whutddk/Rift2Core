
/*
* @Author: Ruige Lee
* @Date:   2021-05-06 16:02:23
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-05-06 16:02:42
*/

package test.mem


import chisel3._
import chisel3.util._

import axi._
import rift2Core.cache._
import tilelink._

class Tl_l3c_tb extends Module{
	val io = IO( new Bundle {
		val l2c_chn_a = Flipped(new DecoupledIO(new TLchannel_a(128, 32)))
		val l2c_chn_d = new DecoupledIO( new TLchannel_d(128))	
	})

	val l3c = Module( new L3Cache )
	val mem = Module( new AXI_full_mem(128, 12) )

	mem.io.mem_chn_ar <> l3c.io.mem_chn_ar
	mem.io.mem_chn_r <> l3c.io.mem_chn_r
	mem.io.mem_chn_aw <> l3c.io.mem_chn_aw
	mem.io.mem_chn_w <> l3c.io.mem_chn_w
	mem.io.mem_chn_b <> l3c.io.mem_chn_b

	l3c.io.l2c_chn_a <> io.l2c_chn_a
	l3c.io.l2c_chn_d <> io.l2c_chn_d

	l3c.io.l3c_fence_req := false.B
	// l3c.io.l3c_fence_end

}

import chiseltest._
import org.scalatest._
import chisel3.iotesters._

class WaveformTester(dut: Tl_l3c_tb) extends PeekPokeTester(dut){

	step(1000)

}

class WaveformSpec extends FlatSpec with Matchers {
  "WaveformSpec" should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on","--target-dir", "generated"), () => new Tl_l3c_tb()){
      c => new WaveformTester(c)
    } should be (true)
  }
}


