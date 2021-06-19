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

trait TLC_mst_Probe extends TLC_base {
  val mst_chn_b = IO(Flipped(new DecoupledIO(new TLchannel_b(128, 32))))



  val mst_chn_b_ready = RegInit(false.B)
  mst_chn_b.ready := mst_chn_b_ready

  when( is_mstProbe_allowen ) { mst_chn_b_ready := true.B; is_mstProbe_StateOn := true.B }
  .elsewhen( mst_chn_b.fire ) { mst_chn_b_ready := false.B }

  when( mst_chn_b.fire ) { is_mstProbeAck_Data_Waiting := true.B }

  info_mstProbe_address := RegEnable( mst_chn_b.bits.address, is_mstProbe_allowen )
  
  info_mstProbe_cl := info_mstProbe_address(addr_lsb+line_w-1, addr_lsb)



  info_mstProbe_cache_tag_ren := is_mstProbe_allowen
  info_mstProbe_cache_tag_raddr := mst_chn_b.bits.address
  info_mstProbe_cb := {
    val tag_info = mst_chn_b.bits.address( 31, 32-tag_w )
    val is_cb_hit = VecInit( for ( i <- 0 until cb ) yield cache_tag.tag_info_r(i) === tag_info )
    val hit_cb = OHToUInt(is_cb_hit.asUInt)

    assert( ~(mst_chn_b.fire &
      is_cb_hit.forall((x:Bool) => (x === false.B))), "Assert Failed at TLC_mst_P.scala!! Your Slave agent request to Probe a missing block!"  )
    
    RegEnable( hit_cb, mst_chn_b.fire )
  }

}


trait TLC_mst_probeAckData extends TLC_base {
  val mst_chn_c0 = IO(new DecoupledIO(new TLchannel_c(128, 32)))




  val mst_probe_Ack_Data_state_dnxt = Wire(UInt(2.W))
  val mst_probe_Ack_Data_state_qout = RegNext(mst_probe_Ack_Data_state_dnxt, 0.U)


  val is_mstProbeData_addrend = info_mstProbeData_address(addr_lsb-1, bus_lsb).andR
  val is_mstProbeData_dirty =
    RegEnable(
      cache_mdf(info_mstProbe_cl)(info_mstProbe_cb).forall( (x:Bool) => (x === false.B) ),
      mst_probe_Ack_Data_state_qout === 1.U & mst_probe_Ack_Data_state_dnxt === 2.U
    )


  val mst_chn_c0_valid = RegInit(false.B)

  mst_probe_Ack_Data_state_dnxt := 
    Mux1H(Seq(
      (mst_probe_Ack_Data_state_qout === 0.U) -> Mux( is_mstProbeAck_Data_allowen, 1.U, 0.U ),
      (mst_probe_Ack_Data_state_qout === 1.U) -> Mux( cache_coh.coh_info_r.forall( (x:UInt) => (x === 0.U) ), 2.U, 0.U ),
      (mst_probe_Ack_Data_state_qout === 2.U) -> Mux( mst_chn_c0.fire & (is_mstProbeData_addrend | ~is_mstProbeData_dirty), 0.U, 2.U )
    ))

  when( mst_probe_Ack_Data_state_dnxt === 2.U ) { is_mstProbeAck_Data_StateOn := true.B }
  .elsewhen( mst_probe_Ack_Data_state_dnxt === 0.U ){ is_mstProbeAck_Data_StateOn := false.B }

  when( mst_probe_Ack_Data_state_qout === 1.U & mst_probe_Ack_Data_state_dnxt === 2.U ) {
    info_mstProbeData_address := info_mstProbe_address
  }
  .elsewhen( mst_chn_c0.fire & ~is_mstProbeData_addrend ){
    info_mstProbeData_address := info_mstProbeData_address + (1.U << bus_lsb)
  }

  when( mst_probe_Ack_Data_state_qout === 2.U & ~mst_chn_c0.valid ) { mst_chn_c0_valid := true.B }
  .elsewhen( mst_chn_c0.fire ) { mst_chn_c0_valid := false.B }

  mst_chn_c0.bits.address := info_mstProbe_address
  mst_chn_c0.bits.corrupt := false.B
  mst_chn_c0.bits.data := cache_dat.dat_info_r( info_mstProbe_cb )
  mst_chn_c0.bits.opcode := 
    Mux( is_mstProbeData_dirty, Opcode.ProbeAckData, Opcode.ProbeAck )
  mst_chn_c0.bits.param := TLparam.TtoN
  mst_chn_c0.bits.size := addr_lsb.U
  mst_chn_c0.bits.source := agent_no.U
  
  when( mst_probe_Ack_Data_state_qout === 2.U & mst_probe_Ack_Data_state_dnxt === 0.U ) {
    for ( i <- 0 until bk ) yield {
      cache_mdf(info_mstProbe_cl)(info_mstProbe_cb)(i) := false.B
      cache_inv(info_mstProbe_cl)(info_mstProbe_cb)(i) := true.B
    }
    is_mstProbe_StateOn := false.B
    is_mstProbeAck_Data_Waiting := false.B
  }

  val exclusive_bk = Wire( UInt(log2Ceil(bk).W) )
  exclusive_bk := cache_coh.coh_info_r.indexWhere( (x:UInt) => x =/= 0.U )

  when( (mst_probe_Ack_Data_state_qout === 1.U) & (mst_probe_Ack_Data_state_dnxt === 0.U) ) {
    is_slvProbe_Waiting := true.B
    
    info_mstRecProbe_address := Cat( info_mstProbe_address(63,addr_lsb), exclusive_bk, Fill( addr_lsb-log2Ceil(bk), 0.U) )
    info_mstRecProbe_exclusive := cache_coh.coh_info_r(exclusive_bk)
    info_mstRecProbe_cb := info_mstProbe_cb

  }

}

trait TLC_mst_P extends TLC_base with TLC_mst_Probe with TLC_mst_probeAckData










