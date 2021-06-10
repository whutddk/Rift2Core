
/*
  Copyright (c) 2020 - 2021 Ruige Lee <295054118@qq.com>

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
import axi._
import base._


abstract class TLC_mst_axi( dw:Int, bk:Int, cb:Int, cl:Int, mst_num:Int, mst_size:Int ) extends TLC_slv_port( dw, bk, cb, cl, mst_num, mst_size ) {

  val mst_chn_ar = IO(DecoupledIO( new AXI_chn_a(32) ))
  val mst_chn_r  = IO(Flipped(DecoupledIO( new AXI_chn_w(128) )))
  val mst_chn_aw = IO(DecoupledIO( new AXI_chn_a(32) ))
  val mst_chn_w  = IO(DecoupledIO( new AXI_chn_w(128)))
  val mst_chn_b  = IO(Flipped(DecoupledIO( new AXI_chn_b() )))

  val cache_fence = IO(Flipped(DecoupledIO(Bool())))
  

  val ar_valid = RegInit(false.B)
  val r_ready  = RegInit(false.B)
  val aw_valid = RegInit(false.B)
  val w_valid  = RegInit(false.B)
  val b_ready  = RegInit(false.B)

  mst_chn_ar.valid := ar_valid
  mst_chn_r.ready  := r_ready
  mst_chn_aw.valid := aw_valid
  mst_chn_w.valid  := w_valid
  mst_chn_b.ready  := b_ready

  when( state_qout === cktag & state_dnxt === flash ) { ar_valid := true.B }
  .elsewhen( mst_chn_ar.fire ) { ar_valid := false.B }

  mst_chn_ar.bits.id    := 0.U
  mst_chn_ar.bits.addr  := flash_addr
  mst_chn_ar.bits.len   := (dw*bk/128).U
  mst_chn_ar.bits.size  := log2Ceil(128/8).U
  mst_chn_ar.bits.burst := "b01".U
  mst_chn_ar.bits.lock  := 0.U
  mst_chn_ar.bits.cache := 0.U
  mst_chn_ar.bits.port  := 0.U
  mst_chn_ar.bits.qos   := 0.U
  mst_chn_ar.bits.user  := 0.U


  when( mst_chn_r.fire ) { r_ready := false.B }
  .elsewhen( mst_chn_r.valid === true.B ) { r_ready := true.B }

  override val flash_data = mst_chn_r.bits.data 
  override val is_evict_bus_end = is_evict_bus_fire & evict_addr(addr_lsb-1, bus_lsb).andR
  override val is_fence_req = state_qout === cfree & cache_fence.valid
  override val is_flash_bus_fire = mst_chn_r.fire
  override val is_evict_bus_fire = mst_chn_w.fire


  when( state_qout === cktag & state_dnxt === evict ) { aw_valid := true.B  }
  .elsewhen( mst_chn_aw.fire ) { aw_valid := false.B }

  mst_chn_aw.bits.id    := 0.U
  mst_chn_aw.bits.addr  := evict_addr
  mst_chn_aw.bits.len   := (dw*bk/128).U
  mst_chn_aw.bits.size  := log2Ceil(128/8).U
  mst_chn_aw.bits.burst := "b01".U
  mst_chn_aw.bits.lock  := 0.U
  mst_chn_aw.bits.cache := 0.U
  mst_chn_aw.bits.port  := 0.U
  mst_chn_aw.bits.qos   := 0.U
  mst_chn_aw.bits.user  := 0.U


  when( mst_chn_w.fire ) { w_valid := false.B }
  .elsewhen( state_qout === evict ) { w_valid := true.B  }

  mst_chn_w.bits.data := mem_dat
  mst_chn_w.bits.strb := "hFFFF".U
  mst_chn_w.bits.last := is_evict_bus_end
  mst_chn_w.bits.user := 0.U

  when( mst_chn_b.fire ) { b_ready := false.B }
  .elsewhen( mst_chn_b.valid ) { b_ready := true.B }

  cache_fence.ready := cache_dirty

}


abstract class TLC_mst_tlc( dw:Int, bk:Int, cb:Int, cl:Int, mst_num:Int, mst_size:Int ) extends TLC_slv_port( dw, bk, cb, cl, mst_num, mst_size ) {

}
