/*
  Copyright (c) 2020 - 2021 Ruige Lee <m201772520@hust.edu.cn>

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

package rift2Core.cache.AXI

import chisel3._
import chisel3.util._

import axi._

import base._
import rift2Core.cache._
import rift2Core.cache.TLC.TLC_base


trait AXI_mst_Acquire extends TLC_base {
  val mst_chn_ar = IO(new DecoupledIO( new AXI_chn_a( 64, 1, 1 ) ))
 
  val mst_chn_ar_valid = RegInit(false.B)

  mst_chn_ar.valid := mst_chn_ar_valid

  when( mst_chn_ar.fire ) {
    mst_chn_ar_valid := false.B
    is_mstAcquire_StateOn := true.B
  }
  .elsewhen( is_mstAcquire_allowen ) { mst_chn_ar_valid := true.B }

   mst_chn_ar.bits.addr  := info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)
   mst_chn_ar.bits.burst := "b01".U
   mst_chn_ar.bits.cache := 0.U
   mst_chn_ar.bits.id    := 0.U
   mst_chn_ar.bits.len   := (dw*bk/bus_w-1).U
   mst_chn_ar.bits.lock  := 0.U
   mst_chn_ar.bits.port  := 0.U
   mst_chn_ar.bits.qos   := 0.U
   mst_chn_ar.bits.size  := bus_lsb.U
   mst_chn_ar.bits.user  := 0.U


}

trait AXI_mst_grantData extends TLC_base {
  val mst_chn_r  = IO( Flipped( new DecoupledIO(new AXI_chn_r( 128, 1, 1 )) ))

  val mst_chn_r_ready = RegInit(false.B)
  mst_chn_r.ready := mst_chn_r_ready

  val is_mstGrantData_addrend = info_slvAcquire_address(addr_lsb-1, bus_lsb).andR

  when( is_mstGrantData_allowen ) {
    info_mstGrantData_address := info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)
  }
  .elsewhen( mst_chn_r.fire & ~is_mstGrantData_addrend ) {
    info_mstGrantData_address := info_mstGrantData_address + (1.U << bus_lsb)
  }

  when( is_mstGrantData_allowen ) { is_mstGrantData_StateOn := true.B }

  when( mst_chn_r.fire ) { mst_chn_r_ready := false.B }
  .elsewhen( is_mstGrantData_StateOn & mst_chn_r.valid ) { mst_chn_r_ready := true.B }

  when( is_mstGrantData_addrend & mst_chn_r.fire ) {
    is_mstGrantData_StateOn := false.B
    cache_inv(info_slvAcquire_cl)(info_slvAcquire_cb) := false.B
    for ( i <- 0 until bk ) yield {
      cache_mdf(info_slvAcquire_cl)(info_slvAcquire_cb)(i) := false.B
    }
  }

  for ( i <- 0 until cb ) yield {
    info_mstGrantData_cache_dat_wen(i) := 
      ( i.U === info_slvAcquire_cb ) & ( mst_chn_r.fire )
  }

  info_mstGrantData_cache_dat_waddr := info_mstGrantData_address
  info_mstGrantData_cache_dat_wstrb := "hFFFF".U
  info_mstGrantData_cache_dat_winfo := mst_chn_r.bits.data


  for ( i <- 0 until cb ) yield {
    info_mstGrantData_cache_tag_wen(i) := 
      ( i.U === info_slvAcquire_cb ) & ( is_mstGrantData_addrend & mst_chn_r.fire )
  }
  info_mstGrantData_cache_tag_waddr := info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    info_mstGrantData_cache_coh_wen(i)(j) := 
      ( i.U === info_slvAcquire_cb ) & ( j.U === info_slvAcquire_bk ) &
      ( is_mstGrantData_addrend & mst_chn_r.fire )
  }

  info_mstGrantData_cache_coh_waddr := info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)
  info_mstGrantData_cache_coh_winfo := 0.U


  override val is_mstGrantAck_Waiting = false.B
  override val is_mstGrantAck_StateOn = false.B
  override val is_mstGrantAck_allowen = false.B



  
}
