
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
import axi._
import chisel3.experimental.ChiselEnum
import rift2Core.L1Cache._

import chisel3.experimental._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._




class lsu_scoreBoard(implicit p: Parameters) extends DcacheModule {

  val io = IO(new Bundle{
    val lsu_push = Flipped(new DecoupledIO(new Info_cache_sb))
    val lsu_pop = new DecoupledIO(new Exe_iwb_info)

    val dcache_push = new DecoupledIO(new Info_cache_s0s1)
    val dcache_pop = Flipped(new DecoupledIO(new Info_cache_retn))

    val periph_push = new DecoupledIO(new Info_cache_s0s1)
    val periph_pop = Flipped(new DecoupledIO(new Info_cache_retn))

    // val is_hazard = Output(Bool())
    val is_empty = Output(Bool())
  })

  /** a paddr buff that store out-standing info */
  val paddr = RegInit(VecInit(Seq.fill(16)(0.U(64.W)) ))

  /** a rd0 buff that store out-standing info */
  val rd0 = RegInit(VecInit(Seq.fill(16)(0.U(6.W)) ))

  /** a flag indicated whether the buff is valid */
  val valid = RegInit(VecInit(Seq.fill(16)(false.B)))

  /** indexed an empty buff */
  val empty_idx = valid.indexWhere((x:Bool) => (x === false.B))

  /** indicated whether all buff in used */
  val full = valid.forall((x:Bool) => (x === true.B))

  /** indicated whether none buff in used */
  val is_empty = valid.forall((x:Bool) => (x === false.B))
  io.is_empty := is_empty

  /** indicated whether a paddr in a valid buff is equal to input */
  val is_hazard = valid.zip(paddr).map{ case(a,b) => (a === true.B) & (b(31,addr_lsb) === io.lsu_push.bits.param.op1(31,addr_lsb)) }.reduce(_|_)  
//  io.is_hazard := is_hazard

  when( io.lsu_push.fire & (io.lsu_push.bits.param.op1(31,29) === "b001".U | io.lsu_push.bits.param.op1(31) === 1.U)) {
    valid(empty_idx)  := true.B
    paddr(empty_idx)  := io.lsu_push.bits.param.op1
    rd0(empty_idx)    := io.lsu_push.bits.param.rd0_phy
  }
  when( io.dcache_pop.fire ) {
    valid(io.dcache_pop.bits.chk_idx) := false.B  
  }
  .elsewhen( io.periph_pop.fire ) {
    valid(io.periph_pop.bits.chk_idx) := false.B
  }




  io.dcache_push.bits.paddr   := io.lsu_push.bits.param.op1
  io.periph_push.bits.paddr   := io.lsu_push.bits.param.op1

  io.periph_push.bits.wmask   := io.dcache_push.bits.wmask
  io.dcache_push.bits.wmask   := {
    val paddr = io.lsu_push.bits.param.op1
    val op = io.lsu_push.bits.fun
    val lsu_wstrb_align =
      (Mux1H( Seq(
        op.is_byte -> "b00000001".U,
        op.is_half -> "b00000011".U,
        op.is_word -> "b00001111".U,
        op.is_dubl -> "b11111111".U
        )) << paddr(2,0))
    lsu_wstrb_align
  }

  for ( j <- 0 until bk ) yield {
    io.periph_push.bits.wdata(j) := io.dcache_push.bits.wdata(j)
    io.dcache_push.bits.wdata(j) := {
      val res = Wire(UInt(64.W))
      val paddr = io.lsu_push.bits.param.op1
      val shift = Wire(UInt(6.W))
      shift := Cat( paddr(2,0), 0.U(3.W) )

      res := io.lsu_push.bits.param.op2 << shift
      res
    }
  }

  io.periph_push.bits.op.fun <> io.lsu_push.bits.fun
  io.dcache_push.bits.op.fun <> io.lsu_push.bits.fun

  io.periph_push.bits.op.grant := false.B
  io.dcache_push.bits.op.grant := false.B

  io.periph_push.bits.op.probe := false.B
  io.dcache_push.bits.op.probe := false.B

  io.periph_push.bits.chk_idx := empty_idx
  io.dcache_push.bits.chk_idx := empty_idx



  io.lsu_pop.bits.rd0_phy := rd0( Mux( io.dcache_pop.valid, io.dcache_pop.bits.chk_idx, io.periph_pop.bits.chk_idx))
  io.lsu_pop.bits.res := Mux( io.dcache_pop.valid, io.dcache_pop.bits.res, io.periph_pop.bits.res )


  io.lsu_pop.valid :=
    (io.dcache_pop.fire & io.dcache_pop.bits.is_load_amo) |
    (io.periph_pop.fire & io.periph_pop.bits.is_load_amo)


  io.periph_push.valid := io.lsu_push.fire & io.lsu_push.bits.param.op1(31,29) === "b001".U
  io.dcache_push.valid := io.lsu_push.fire & io.lsu_push.bits.param.op1(31) === 1.U

  io.lsu_push.ready :=
    ~full & ~is_hazard &
    Mux1H(Seq(
      (io.lsu_push.bits.param.op1(31,29) === "b001".U) -> io.periph_push.ready,
      (io.lsu_push.bits.param.op1(31) === 1.U)        -> io.dcache_push.ready
    ))

  io.dcache_pop.ready := io.lsu_pop.ready
  io.periph_pop.ready := io.lsu_pop.ready & ~io.dcache_pop.valid

}

