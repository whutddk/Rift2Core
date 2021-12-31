
/*
  Copyright (c) 2020 - 2022 Wuhan University of Technology <295054118@whut.edu.cn>

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



package rift2Chip

import chisel3._
import chisel3.util._

import axi._
import rift2Core.privilege._

class Clint extends Module {
  val io = new Bundle {

    val rtc_clock = Input(Bool())

    val is_swip = Output(Bool())
    val is_tmip = Output(Bool())

    val chn_ar = Flipped( new DecoupledIO(new AXI_chn_a(32)))
    val chn_r  = new DecoupledIO(new AXI_chn_r(64))
    val chn_aw = Flipped( new DecoupledIO(new AXI_chn_a(32)))
    val chn_w  = Flipped( new DecoupledIO(new AXI_chn_w(64)))
    val chn_b  = new DecoupledIO(new AXI_chn_b())
  }



  val clint_slv_r = Module(new AXI_slv_r(32, 64) )
  val clint_slv_w = Module(new AXI_slv_w(32, 64) )


  val swip = 
    RegEnable(
      io.chn_w.bits.data(0).asBool(),
      false.B,
      io.chn_aw.bits.addr === "h02000000".U & io.chn_aw.fire & io.chn_w.fire
      )

  val mtimecmp = 
    RegEnable(
      io.chn_w.bits.data,
      0.U(64.W),
      io.chn_aw.bits.addr === "h02004000".U & io.chn_aw.fire & io.chn_w.fire
      )

  val rtc_toggle = {
    val rtc_0 = ShiftRegister(io.rtc_clock, 3)
    val rtc_1 = ShiftRegister(io.rtc_clock, 4)
    rtc_0 ^ rtc_1
  }

  val mtime = RegInit(0.U(64.W))

  when( io.chn_aw.bits.addr === "h0200bff8".U & io.chn_aw.fire & io.chn_w.fire ) {
    mtime := io.chn_w.bits.data
  }
  .elsewhen( rtc_toggle ) {
    mtime := mtime + 1.U
  }

  clint_slv_r.io.r_info.data :=
    RegEnable(
      Mux1H(Seq(
        (io.chn_aw.bits.addr === "h02000000".U) -> swip.asUInt,
        (io.chn_aw.bits.addr === "h02004000".U) -> mtimecmp,
        (io.chn_aw.bits.addr === "h0200bff8".U) -> mtime
      )),
      io.chn_ar.fire
    )
  clint_slv_r.io.r_info.id := 0.U
  clint_slv_r.io.r_info.last := true.B
  clint_slv_r.io.r_info.rsp := 0.U
  clint_slv_r.io.r_info.user := 0.U
  
  

  io.is_swip := swip
  io.is_tmip := (mtime >= mtimecmp)


}

