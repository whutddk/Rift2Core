
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

package rift2Core.cache.TLC

import chisel3._
import chisel3.util._

import tilelink._

import base._
import rift2Core.cache._



trait slv_release_release_data extends TLC_base {
  val slv_chn_c1 = IO(Flipped(new DecoupledIO(new TLchannel_c(128, 32))))

  is_slvReleaseData_allowen :=
    // ~is_slvAcquire_StateOn &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    // ~is_slvProbe_StateOn &
    ~is_slvProbeData_StateOn &
    ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    // ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    // ~is_mstProbe_StateOn & 
    ~is_mstProbeData_StateOn &
    ~is_mstProbeAck_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    // ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    // ~is_slvProbe_Waiting &
    ~is_slvProbeData_valid &
    ~is_slvProbeAck_valid &
    is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting
    // ~is_mstAcquire_Waiting &
    // ~is_mstGrantData_valid &
    // ~is_mstGrantAck_Waiting &
    // ~is_mstProbe_valid &
    // ~is_mstProbeAck_Waiting &
    // ~is_mstProbeData_Waiting &
    // ~is_mstReleaseData_Waiting &
    // ~is_mstReleaseAck_valid


  is_slvReleaseData_valid := slv_chn_c1.valid


  val c1_ready = RegInit(false.B)
  slv_chn_c1.ready := c1_ready

  val info_slvReleaseData_cb = Wire( UInt(log2Ceil(cb).W) )
  val info_slvReleaseData_addr = RegInit(0.U(64.W))
  val is_slvReleaseData_addrend = info_slvReleaseData_addr( mst_lsb-1, bus_lsb ).andR


  val slvReleaseData_state_dnxt = Wire(UInt(2.W))
  val slvReleaseData_state_qout = RegNext(slvReleaseData_state_dnxt, 0.U)

  slvReleaseData_state_dnxt := 
    Mux1H(Seq(
      ( slvReleaseData_state_qout === 0.U ) -> Mux( is_slvReleaseData_valid, 1.U, 0.U ),
      ( slvReleaseData_state_qout === 1.U ) -> 2.U,
      ( slvReleaseData_state_qout === 2.U ) -> Mux( is_slvReleaseData_addrend | (slv_chn_c1.bits.opcode === Opcode.ReleaseAck), 0.U, 2.U )
    ))
    

  when( slv_chn_c1.fire ) { c1_ready := false.B }
  .elsewhen( slv_chn_c1.valid & slv_ReleaseData_state_qout === 2.U ) { c1_ready := true.B }

  when( slv_ReleaseData_state_dnxt =/= 0.U ) { is_slvReleaseData_StateOn := true.B }
  .otherwise { is_slvReleaseData_StateOn := false.B }


  when( slv_ReleaseData_state_qout === 0.U & slv_ReleaseData_state_dnxt === 1.U ) {
    info_slvReleaseData_addr := slv_chn_c1.bits.address
  }
  .elsewhen( slv_chn_c1.fire ) {
    info_slvReleaseData_addr := info_slvReleaseData_addr + (1.U << bus_lsb)
  }

  when( slv_ReleaseData_state_qout === 2.U & slv_ReleaseData_state_dnxt === 0.U ) {
    apply_cache_modified(info_slvReleaseData_addr, info_slvReleaseData_cb, true.B)
  }


  info_slvReleaseData_cb := {
    val tag_info = slv_chn_c1.bits.address( 31, 32-tag_w )
    val is_cb_hit =
          VecInit(
            for ( i <- 0 until cb ) yield cache_tag.tag_info_r(i) === tag_info
          )
    val hit_cb = OHToUInt(is_cb_hit.asUInt)

    RegEnable( hit_cb, slv_ReleaseData_state_qout === 1.U & slv_ReleaseData_state_dnxt === 2.U )

    assert( ~(slv_ReleaseData_state_qout === 1.U & slv_ReleaseData_state_dnxt === 2.U &
      is_cb_hit.forall((x:Bool) => (x === false.B))), "Assert Failed at TLC_slv_R.scala!! Your master agent release a missing block!"  )
    
  }

  for ( i <- 0 until cb ) yield {
      info_slvReleaseData_cache_coh_wen() := slv_ReleaseData_state_qout === 2.U & slv_ReleaseData_state_dnxt === 0.U
  }

  info_slvReleaseData_cache_coh_waddr := info_slvReleaseData_addr
  info_slvReleaseData_cache_coh_winfo := 0.U

  for ( i <- 0 until cb ) yield {
    info_slvReleaseData_cache_dat_wen()   := slv_ReleaseData_state_qout === 2.U & slv_chn_c1.bits.opcode === Opcode.ReleaseData & slv_chn_c1.fire
  }
  info_slvReleaseData_cache_dat_waddr := info_slvReleaseData_addr
  info_slvReleaseData_cache_dat_wstrb := "hffff".U
  info_slvReleaseData_cache_dat_winfo := slv_chn_c1.bits.data


}


trait slv_releaseAck extends TLC_base {
}
