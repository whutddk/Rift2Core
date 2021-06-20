

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


trait TLC_slv_probe extends TLC_base {
  val slv_chn_b = IO(new DecoupledIO(new TLchannel_b(128, 32)))



  val b_valid = RegInit(false.B)


  when( slv_chn_b.fire ) {
    b_valid := false.B


  }
  when( is_slvProbe_allowen ) {
    b_valid := true.B
    is_slvProbe_Waiting := false.B
    is_slvProbe_StateOn := true.B
  }

  slv_chn_b.bits.address :=
    MuxCase( DontCare, Array(
      is_mstProbe_StateOn   -> info_mstRecProbe_address,
      is_slvAcquire_StateOn -> info_slvAcquire_address
    ))

  slv_chn_b.bits.corrupt := false.B
  slv_chn_b.bits.data    := DontCare
  slv_chn_b.bits.mask    := DontCare
  slv_chn_b.bits.opcode  := Opcode.ProbeBlock
  slv_chn_b.bits.param   := TLparam.toN
  slv_chn_b.bits.size    := mst_lsb.U
  slv_chn_b.bits.source  :=
    MuxCase( DontCare, Array(
      is_mstProbe_StateOn   -> info_mstRecProbe_exclusive,
      is_slvAcquire_StateOn -> info_slvGrantData_exclusive
    ))

  info_slvProbe_cb :=
    RegEnable(
      MuxCase( DontCare, Array(
        is_mstProbe_StateOn   -> info_mstRecProbe_cb,
        is_slvAcquire_StateOn -> info_slvAcquire_cb
      )),
    slv_chn_b.fire
    )

  info_slvProbe_address :=
    RegEnable(
      MuxCase( DontCare, Array(
        is_mstProbe_StateOn   -> info_mstRecProbe_address,
        is_slvAcquire_StateOn -> info_slvAcquire_address
      )),
    slv_chn_b.fire
    )

  info_slvProbe_bk := info_slvProbe_address(addr_lsb-1, addr_lsb-log2Ceil(bk) )
  info_slvProbe_cl := info_slvProbe_address(addr_lsb+line_w-1, addr_lsb)

  info_slvProbe_exclusive :=
    RegEnable(
      MuxCase( DontCare, Array(
        is_mstProbe_StateOn   -> info_mstRecProbe_exclusive,
        is_slvAcquire_StateOn -> info_slvGrantData_exclusive
      )),
    slv_chn_b.fire
    )


}


trait TLC_slv_probeAckData extends TLC_base {
  val slv_chn_c0 = IO(Flipped(new DecoupledIO(new TLchannel_c(128, 32))))




  is_slvProbeData_valid := slv_chn_c0.valid & slv_chn_c0.bits.opcode === Opcode.ProbeAckData
  is_slvProbeAck_valid  := slv_chn_c0.valid & slv_chn_c0.bits.opcode === Opcode.ProbeAck

  val c0_ready = RegInit(false.B)
  slv_chn_c0.ready := c0_ready

  val slvProbeData_addr = RegInit(0.U(64.W))
  val is_slvProbeData_addrend = slvProbeData_addr( mst_lsb-1, bus_lsb ).andR

  when( is_slvProbeAck_valid | is_slvProbeData_valid ) { c0_ready := true.B }
  .elsewhen( slv_chn_c0.fire ) {
    c0_ready := false.B
  }

  when( is_slvProbeData_valid & ~is_slvProbeData_StateOn ) {
    is_slvProbeData_StateOn := true.B
    slvProbeData_addr := info_slvProbe_address
  }
  .elsewhen( slv_chn_c0.fire & is_slvProbeData_StateOn ) {
    slvProbeData_addr := slvProbeData_addr + (1.U << bus_lsb)
  }
  .elsewhen( slv_chn_c0.fire & is_slvProbeData_addrend ) {
    is_slvProbeData_StateOn := false.B
    is_slvProbe_StateOn := false.B
  }


  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    info_slvProbeAck_Data_cache_coh_wen(i)(j) := 
    ( i.U === info_slvProbe_cb ) & ( j.U === info_slvProbe_bk ) & 
    is_slvProbeData_StateOn & slv_chn_c0.fire & is_slvProbeData_addrend
  }
  info_slvProbeAck_Data_cache_coh_waddr := info_slvProbe_address
  info_slvProbeAck_Data_cache_coh_winfo := 0.U

  when( is_slvProbeData_StateOn & slv_chn_c0.fire & is_slvProbeData_addrend ) {
    cache_mdf(info_slvProbe_cl)(info_slvProbe_cb)(info_slvProbe_bk) := true.B
  }


  for ( i <- 0 until cb ) yield {
    info_slvProbeAck_Data_cache_dat_wen(i) :=
    ( i.U === info_slvProbe_cb ) & 
    is_slvProbeData_StateOn & slv_chn_c0.fire    
  }

  info_slvProbeAck_Data_cache_dat_waddr := info_slvProbe_address
  info_slvProbeAck_Data_cache_dat_wstrb := "hffff".U
  info_slvProbeAck_Data_cache_dat_winfo := slv_chn_c0.bits.data



}

trait TLC_slv_P extends TLC_base with TLC_slv_probe with TLC_slv_probeAckData



