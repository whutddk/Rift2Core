/*
  Copyright (c) 2020 - 2021 Ruige Lee <295054118@whut.edu.cn>

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

package rift2Core.cache

import chisel3._
import chisel3.util._

import tilelink._
import base._

class TLC_CrossBar_Nto1(slv: Int) extends Module {
  val io = IO(new Bundle{
    val slv_chn_a = Vec(slv, Flipped(new DecoupledIO(new TLchannel_a(128, 32))))
    val slv_chn_b = Vec(slv, new DecoupledIO(new TLchannel_b(128, 32)))
    val slv_chn_c = Vec(slv, Flipped(new DecoupledIO(new TLchannel_c(128, 32))))
    val slv_chn_d = Vec(slv, new DecoupledIO( new TLchannel_d(128)))
    val slv_chn_e = Vec(slv, Flipped(new DecoupledIO( new TLchannel_e)))

    val mst_chn_a = new DecoupledIO(new TLchannel_a(128, 32))
    val mst_chn_b = Flipped(new DecoupledIO(new TLchannel_b(128, 32)))
    val mst_chn_c = new DecoupledIO(new TLchannel_c(128, 32))
    val mst_chn_d = Flipped(new DecoupledIO( new TLchannel_d(128)))
    val mst_chn_e = new DecoupledIO( new TLchannel_e)
  })

  val is_a_chn_busy = RegInit(false.B)
  val a_chn_dnxt = Wire( UInt(log2Ceil(slv).W) )
  val a_chn_qout = Reg( UInt(log2Ceil(slv).W) )

  val is_c_chn_busy = RegInit(false.B)
  val c_chn_dnxt = Wire( UInt(log2Ceil(slv).W) )
  val c_chn_qout = Reg( UInt(log2Ceil(slv).W) )

  val is_e_chn_busy = RegInit(false.B)
  val e_chn_dnxt = Wire( UInt(log2Ceil(slv).W) )
  val e_chn_qout = Reg( UInt(log2Ceil(slv).W) )

  a_chn_dnxt := {
    val req = for ( i <- 0 until slv ) yield { io.slv_chn_a(i).valid }
    val no = for ( i <- 0 until slv ) yield { i.U }
    MuxCase( 0.U, req zip no )
  }

  c_chn_dnxt := {
    val req = for ( i <- 0 until slv ) yield { io.slv_chn_c(i).valid }
    val no = for ( i <- 0 until slv ) yield { i.U }
    MuxCase( 0.U, req zip no )
  }

  e_chn_dnxt := {
    val req = for ( i <- 0 until slv ) yield { io.slv_chn_e(i).valid }
    val no = for ( i <- 0 until slv ) yield { i.U }
    MuxCase( 0.U, req zip no )
  }

  when( ~is_a_chn_busy & io.slv_chn_a.exists( (x:DecoupledIO[TLchannel_a]) => (x.valid === true.B) ) ) {
    is_a_chn_busy := true.B
    a_chn_qout := a_chn_dnxt
  }
  .elsewhen( io.mst_chn_a.fire ) {
    is_a_chn_busy := false.B
  }

  when( ~is_c_chn_busy & io.slv_chn_c.exists( (x:DecoupledIO[TLchannel_c]) => (x.valid === true.B) ) ) {
    is_c_chn_busy := true.B
    c_chn_qout := c_chn_dnxt
  }
  .elsewhen( io.mst_chn_c.fire ) {
    is_c_chn_busy := false.B
  }
  
  when( ~is_e_chn_busy & io.slv_chn_e.exists( (x:DecoupledIO[TLchannel_e]) => (x.valid === true.B) ) ) {
    is_e_chn_busy := true.B
    e_chn_qout := e_chn_dnxt
  }
  .elsewhen( io.mst_chn_e.fire ) {
    is_e_chn_busy := false.B
  }


  val a_idx = Mux( is_a_chn_busy, a_chn_qout, a_chn_dnxt )
  val c_idx = Mux( is_c_chn_busy, c_chn_qout, c_chn_dnxt )
  val e_idx = Mux( is_e_chn_busy, e_chn_qout, e_chn_dnxt )
  // val b_idx = io.mst_chn_b.bits.source
  val d_idx = io.mst_chn_d.bits.source

  val is_boardcast_fire = RegInit( VecInit(Seq.fill(slv)(false.B)) )

  for ( i <- 0 until slv ) yield {
    when( io.mst_chn_b.fire ) { is_boardcast_fire(i) := false.B }
    .elsewhen( io.slv_chn_b(i).fire ) { is_boardcast_fire(i) := true.B }
  }


  for ( i <- 0 until slv ) yield {
    io.slv_chn_a(i).ready := Mux( i.U === a_idx, io.mst_chn_a.ready, false.B )
    io.slv_chn_c(i).ready := Mux( i.U === c_idx, io.mst_chn_c.ready, false.B )
    io.slv_chn_e(i).ready := Mux( i.U === e_idx, io.mst_chn_e.ready, false.B )

    io.slv_chn_b(i).valid := io.mst_chn_b.valid & ~is_boardcast_fire(i)
    io.slv_chn_b(i).bits  := io.mst_chn_b.bits

    io.slv_chn_d(i).valid := Mux( i.U === d_idx, io.mst_chn_d.valid, false.B )


  }



  io.mst_chn_a.valid := io.slv_chn_a(a_idx).valid
  io.mst_chn_a.bits  := io.slv_chn_a(a_idx).bits
  io.mst_chn_c.valid := io.slv_chn_c(c_idx).valid
  io.mst_chn_c.bits  := io.slv_chn_c(c_idx).bits
  io.mst_chn_e.valid := io.slv_chn_e(e_idx).valid
  io.mst_chn_e.bits  := io.slv_chn_e(e_idx).bits


  io.mst_chn_b.ready := is_boardcast_fire.forall((x:Bool) => (x === true.B))

  io.mst_chn_d.bits  := io.slv_chn_d(d_idx).bits
  io.mst_chn_d.ready := io.slv_chn_d(d_idx).ready


  assert( (d_idx < slv.U), "Assert Failed at TLC CrossBar, Cannot find the master agent!" )

}


