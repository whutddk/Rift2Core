
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



trait TLC_mst_releaseReleaseData extends TLC_base {
  val mst_chn_c1 = IO(new DecoupledIO(new TLchannel_c(128, 32)))



  val mst_chn_c1_valid = RegInit(false.B)
  mst_chn_c1.valid := mst_chn_c1_valid


  val is_mstReleaseData_invalid = cache_inv(info_slvAcquire_cl)(info_slvAcquire_cb) === true.B
  val is_mstReleaseData_clean = cache_mdf(info_slvAcquire_cl)(info_slvAcquire_cb).forall( (x:Bool) => (x === false.B) )
  val is_mstReleaseData_addrend = info_mstReleaseData_address( addr_lsb-1, bus_lsb ).andR

  val is_mstReleaseData_state_dnxt = Wire( UInt(1.W) )
  val is_mstReleaseData_state_qout = RegNext( is_mstReleaseData_state_dnxt, 0.U )

  is_mstReleaseData_state_dnxt := 
    Mux1H(Seq(
      ( is_mstReleaseData_state_qout === 0.U ) -> Mux( is_slvReleaseData_allowen, 1.U, 0.U ),
      ( is_mstReleaseData_state_qout === 1.U ) -> Mux( mst_chn_c1.fire & ( is_mstReleaseData_addrend | is_mstReleaseData_clean | is_mstReleaseData_invalid ), 0.U, 1.U) 
    ))

  when( is_mstReleaseData_state_qout === 0.U & is_mstReleaseData_state_dnxt === 1.U ) {
    info_mstReleaseData_address := info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)
  }
  .elsewhen( mst_chn_c1.fire & ~is_mstReleaseData_addrend ) {
    info_mstReleaseData_address := info_mstReleaseData_address + (1.U << bus_lsb)
  }

  when( is_mstReleaseData_state_qout === 0.U & is_mstReleaseData_state_dnxt === 1.U ) {
    is_mstReleaseData_StateOn := true.B;
    is_mstReleaseData_Waiting := false.B
  }
  

  when( is_mstReleaseData_state_qout === 1.U & ~mst_chn_c1.valid ) { mst_chn_c1_valid := true.B }
  .elsewhen( mst_chn_c1.fire ) { mst_chn_c1_valid := false.B }



  mst_chn_c1.bits.address := info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)
  mst_chn_c1.bits.corrupt := false.B
  mst_chn_c1.bits.data := cache_dat.dat_addr_r(info_slvAcquire_cb)
  mst_chn_c1.bits.opcode := 
    Mux( is_mstReleaseData_clean, Opcode.ReleaseAck, Opcode.ReleaseData )
  mst_chn_c1.bits.param := TLparam.TtoN
  mst_chn_c1.bits.size := addr_lsb.U
  mst_chn_c1.bits.source := agent_no.U



  for ( i <- 0 until cb ) yield {
    info_mstReleaseData_cache_dat_ren(i) := 
      i.U === info_slvAcquire_cb & is_mstReleaseData_state_qout === 1.U
  }

  info_mstReleaseData_cache_dat_raddr := info_mstReleaseData_address

  when( mst_chn_c1.fire & ( is_mstReleaseData_addrend | is_mstReleaseData_clean | is_mstReleaseData_invalid ) ) {
    cache_inv(info_slvAcquire_cl)(info_slvAcquire_cb) := true.B
    for ( i <- 0 until bk ) yield {

      cache_mdf(info_slvAcquire_cl)(info_slvAcquire_cb)(i) := false.B
    }
  }
}

trait TLC_mst_releaseAck extends TLC_base {
  val mst_chn_d1 = IO(Flipped(new DecoupledIO( new TLchannel_d(128))))



  val mst_chn_d1_ready = RegInit(false.B)
  mst_chn_d1.ready := mst_chn_d1_ready


  when( mst_chn_d1.fire ) {
    mst_chn_d1_ready := false.B;
    is_mstReleaseData_StateOn := false.B
  }
  .elsewhen( is_mstReleaseAck_allowen ) {
    mst_chn_d1_ready := true.B
  }

}

trait TLC_mst_R extends TLC_base with TLC_mst_releaseReleaseData with TLC_mst_releaseAck




