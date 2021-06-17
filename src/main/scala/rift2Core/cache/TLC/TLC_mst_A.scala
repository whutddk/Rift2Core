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

trait mst_Acquire extends TLC_base{
    val mst_chn_a = IO(new DecoupledIO(new TLchannel_a(128, 32)))




  is_mstAcquire_allowen :=
    is_slvAcquire_StateOn &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn &
    ~is_slvProbeData_StateOn &
    ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn &
    ~is_mstProbeData_StateOn &
    ~is_mstProbeAck_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    // ~is_slvAcquire_valid &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    ~is_slvProbe_Waiting &
    ~is_slvProbeData_valid &
    ~is_slvProbeAck_valid &
    ~is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting &
    is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    ~is_mstProbeAck_Waiting &
    ~is_mstProbeData_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid

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


trait mst_grantData extends TLC_base{
  val mst_chn_d0 = IO(Flipped(new DecoupledIO( new TLchannel_d(128))))


  is_mstGrantData_allowen := 
     is_slvAcquire_StateOn & 
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn &
    ~is_slvProbeData_StateOn &
    ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn &
    ~is_mstProbeData_StateOn &
    ~is_mstProbeAck_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    ~is_slvProbe_Waiting &
    ~is_slvProbeData_valid &
    ~is_slvProbeAck_valid &
    ~is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting &
    ~is_mstAcquire_Waiting &
    is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    ~is_mstProbeAck_Waiting &
    ~is_mstProbeData_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid

  val mst_chn_d0_ready = RegInit(false.B)
  mst_chn_d0.ready := mst_chn_d0_ready

  val info_mstGrantData_addr = RegInit(0.U(64.W))
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
    apply_cache_invalid(info_slvAcquire_address, info_slvAcquire_cb, false.B)
    apply_cache_modified(info_slvAcquire_address, info_slvAcquire_cb, false.B)
  }

  for ( i <- 0 until cb ) yield {
    info_mstGrantData_cache_coh_wen(i) := 
      ( i.U === info_slvAcquire_cb ) & ( is_mstGrantData_addrend & mst_chn_d0.fire )
  }

  info_mstGrantData_cache_coh_waddr := info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)
  info_mstGrantData_cache_coh_winfo := 0.U

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


trait mst_grantAck extends TLC_base{
  val mst_chn_e = IO(new DecoupledIO( new TLchannel_e))

  is_mstGrantAck_allowen :=
    is_slvAcquire_StateOn &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn &
    ~is_slvProbeData_StateOn &
    ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    is_mstAcquire_StateOn &
    is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn &
    ~is_mstProbeData_StateOn &
    ~is_mstProbeAck_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    // ~is_SlvProbe_Waiting &
    // ~is_SlvProbeData_valid &
    // ~is_SlvProbeAck_valid &
    // ~is_SlvReleaseData_valid &
    // ~is_SlvReleaseAck_Waiting &
    ~is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    is_mstGrantAck_Waiting
    // ~is_mstProbe_valid &
    // ~is_mstProbeAck_Waiting &
    // ~is_mstProbeData_Waiting &
    // ~is_mstReleaseData_Waiting &
    // ~is_mstReleaseAck_valid

  val mst_chn_e_valid = RegInit(false.B)
  mst_chn_e.valid := mst_chn_e_valid

  when( mst_chn_e.fire ) { mst_chn_e_valid := false.B }
  .elsewhen( is_mstGrantAck_allowen ) { mst_chn_e_valid := true.B }

  mst_chn_e.bits.sink := DontCare

  when( is_mstGrantAck_allowen ) { is_mstGrantAck_Waiting := false.B }
  when( mst_chn_e.fire ) { is_mstGrantData_StateOn := false.B; is_mstAcquire_StateOn := false.B}

}


