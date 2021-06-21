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

package rift2Core.cache.TLC

import chisel3._
import chisel3.util._

import tilelink._

import base._
import rift2Core.cache._

trait TLC_mst_Acquire extends TLC_base{
  val mst_chn_a = IO(new DecoupledIO(new TLchannel_a(128, 32)))


  val mst_chn_a_valid = RegInit(false.B)
  mst_chn_a.valid := mst_chn_a_valid

  when( mst_chn_a.fire ) {
    mst_chn_a_valid := false.B
    is_mstAcquire_StateOn := true.B
  }
  .elsewhen( is_mstAcquire_allowen ) { mst_chn_a_valid := true.B }

  mst_chn_a.bits.address := info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)
  mst_chn_a.bits.corrupt := false.B
  mst_chn_a.bits.data := DontCare
  mst_chn_a.bits.mask := DontCare
  mst_chn_a.bits.opcode := Opcode.AcquireBlock
  mst_chn_a.bits.param := TLparam.NtoT
  mst_chn_a.bits.size := addr_lsb.U
  mst_chn_a.bits.source := agent_no.U


}


trait TLC_mst_grantData extends TLC_base{
  val mst_chn_d0 = IO(Flipped(new DecoupledIO( new TLchannel_d(128))))

  val mst_chn_d0_ready = RegInit(false.B)
  mst_chn_d0.ready := mst_chn_d0_ready

  val is_mstGrantData_addrend = info_slvAcquire_address(addr_lsb-1, bus_lsb).andR

  when( is_mstGrantData_allowen ) {
    info_mstGrantData_address := info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)
  }
  .elsewhen( mst_chn_d0.fire & ~is_mstGrantData_addrend ) {
    info_mstGrantData_address := info_mstGrantData_address + (1.U << bus_lsb)
  }

  when( is_mstGrantData_allowen ) { is_mstGrantData_StateOn := true.B }

  when( mst_chn_d0.fire ) { mst_chn_d0_ready := false.B }
  .elsewhen( is_mstGrantData_StateOn & mst_chn_d0.valid ) { mst_chn_d0_ready := true.B }

  when( is_mstGrantData_addrend & mst_chn_d0.fire ) {
    is_mstGrantAck_Waiting := true.B
    for ( i <- 0 until bk ) yield {
      cache_inv(info_slvAcquire_cl)(info_slvAcquire_cb)(i) := false.B
      cache_mdf(info_slvAcquire_cl)(info_slvAcquire_cb)(i) := false.B
    }


  }



  for ( i <- 0 until cb ) yield {
    info_mstGrantData_cache_dat_wen(i) := 
      ( i.U === info_slvAcquire_cb ) & ( mst_chn_d0.fire )
  }

  info_mstGrantData_cache_dat_waddr := info_mstGrantData_address
  info_mstGrantData_cache_dat_wstrb := "hFFFF".U
  info_mstGrantData_cache_dat_winfo := mst_chn_d0.bits.data

  for ( i <- 0 until cb ) yield {
    info_mstGrantData_cache_tag_wen(i) := 
      ( i.U === info_slvAcquire_cb ) & ( is_mstGrantData_addrend & mst_chn_d0.fire )
  }
  info_mstGrantData_cache_tag_waddr := info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)

}


trait TLC_mst_grantAck extends TLC_base{
  val mst_chn_e = IO(new DecoupledIO( new TLchannel_e))



  val mst_chn_e_valid = RegInit(false.B)
  mst_chn_e.valid := mst_chn_e_valid

  when( mst_chn_e.fire ) { mst_chn_e_valid := false.B }
  .elsewhen( is_mstGrantAck_allowen ) { mst_chn_e_valid := true.B }

  mst_chn_e.bits.sink := DontCare

  when( is_mstGrantAck_allowen ) { is_mstGrantAck_Waiting := false.B }
  when( mst_chn_e.fire ) { is_mstGrantData_StateOn := false.B; is_mstAcquire_StateOn := false.B}

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    info_mstGrantAck_cache_coh_wen(i)(j) := 
      ( i.U === info_slvAcquire_cb ) & ( j.U === info_slvAcquire_bk ) &
      ( mst_chn_e.fire )
  }

  info_mstGrantAck_cache_coh_waddr := info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)
  info_mstGrantAck_cache_coh_winfo := 0.U
}


trait TLC_mst_A extends TLC_base with TLC_mst_Acquire with TLC_mst_grantAck with TLC_mst_grantData

