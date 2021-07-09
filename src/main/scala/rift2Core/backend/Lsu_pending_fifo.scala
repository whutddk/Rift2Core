
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
import chisel3.util.random._
import rift2Core.define._
import rift2Core.cache._
import tilelink._
import axi._
import chisel3.experimental.ChiselEnum
import rift2Core.L1Cache._

import chisel3.experimental._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._


class lsu_pending_fifo(val entries: Int) extends Module{

  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Info_cache_sb))
    val deq = new DecoupledIO(new Info_cache_sb)
    val cmm = Flipped(new DecoupledIO(Bool()))
    val flush = Input(Bool())
  })

  def cnt_w = log2Ceil(entries)

  val buf = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(new Info_cache_sb))))

  val enq_ptr = RegInit(0.U((cnt_w+1).W))
  val deq_ptr = RegInit(0.U((cnt_w+1).W))
  val cmm_ptr = RegInit(0.U((cnt_w+1).W))


  val empty = deq_ptr === cmm_ptr
  val full = deq_ptr(cnt_w-1,0) === enq_ptr(cnt_w-1,0) & deq_ptr(cnt_w) =/= enq_ptr(cnt_w)

  val do_enq = WireDefault(io.enq.fire())
  val do_deq = WireDefault(io.deq.fire())
  val do_cmm = WireDefault( io.cmm.fire )

  when (do_enq) {
    buf(enq_ptr(cnt_w-1,0)) := io.enq.bits
    enq_ptr := enq_ptr + 1.U
  }
  when (do_deq) {
    deq_ptr := deq_ptr + 1.U
  }
  when( do_cmm ) {
    cmm_ptr := cmm_ptr + Mux(io.cmm.bits, 2.U, 1.U)
    assert( cmm_ptr =/= enq_ptr )
  }
  when( io.flush ) {
    enq_ptr := cmm_ptr
    assert (!do_enq)
  }

  io.deq.valid := !empty
  io.enq.ready := !full
  io.deq.bits := buf(deq_ptr(cnt_w-1,0))


  io.cmm.ready := true.B

}


