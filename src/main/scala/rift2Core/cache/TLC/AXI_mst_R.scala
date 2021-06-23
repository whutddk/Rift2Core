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

import axi._

import base._
import rift2Core.cache._

trait AXI_mst_releaseReleaseData extends TLC_base {
  val mst_chn_aw0 = new DecoupledIO(new AXI_chn_a( 64, 1, 1 ))
  val mst_chn_w0  = new DecoupledIO(new AXI_chn_w( 128, 1 )) 


  val mst_chn_aw0_valid = RegInit(false.B)
  val mst_chn_w0_valid = RegInit(false.B)

  mst_chn_aw0.valid := mst_chn_aw0_valid
  mst_chn_w0.valid  := mst_chn_w0_valid

  val is_mstReleaseData_invalid = cache_inv(info_slvAcquire_cl)(info_slvAcquire_cb) === true.B
  val is_mstReleaseData_clean = cache_mdf(info_slvAcquire_cl)(info_slvAcquire_cb).forall( (x:Bool) => (x === false.B) )
  val is_mstReleaseData_addrend = info_mstReleaseData_address( addr_lsb-1, bus_lsb ).andR

  val is_mstReleaseData_state_dnxt = Wire( UInt(2.W) )
  val is_mstReleaseData_state_qout = RegNext( is_mstReleaseData_state_dnxt, 0.U )

  is_mstReleaseData_state_dnxt := 
    Mux1H(Seq(
      ( is_mstReleaseData_state_qout === 0.U ) -> Mux( is_slvReleaseData_allowen, 1.U, 0.U ),
      ( is_mstReleaseData_state_qout === 1.U ) -> Mux( (is_mstReleaseData_clean | is_mstReleaseData_invalid), 0.U, Mux( mst_chn_aw0.fire, 2.U, 1.U )),
      ( is_mstReleaseData_state_qout === 2.U ) -> Mux( is_mstReleaseData_addrend & mst_chn_w0.fire, 0.U, 2.U )
    ))

  when( is_mstReleaseData_state_qout === 0.U & is_mstReleaseData_state_dnxt === 1.U ) {
    info_mstReleaseData_address := info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)
  }
  .elsewhen( mst_chn_w0.fire & ~is_mstReleaseData_addrend ) {
    info_mstReleaseData_address := info_mstReleaseData_address + (1.U << bus_lsb)
  }

  when( is_mstReleaseData_state_qout === 0.U & is_mstReleaseData_state_dnxt === 1.U ) {
    when( ~is_mstReleaseData_clean & ~is_mstReleaseData_invalid ) {
      mst_chn_aw0_valid := true.B
    }
  }
  .elsewhen(mst_chn_aw0.fire) { mst_chn_aw0_valid := false.B }

  when( is_mstReleaseData_state_qout === 2.U & ~mst_chn_w0.valid ) { mst_chn_w0_valid := true.B }
  .elsewhen(mst_chn_w0.fire) { mst_chn_w0_valid := false.B }


  when( is_mstReleaseData_state_qout === 0.U & is_mstReleaseData_state_dnxt === 1.U ) {
    is_mstReleaseData_StateOn := true.B;
    is_slvGrantData_Waiting := false.B
  }
  
  mst_chn_aw0.bits.addr := info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)
  mst_chn_aw0.bits.burst := "b01".U
  mst_chn_aw0.bits.cache := 0.U
  mst_chn_aw0.bits.id := 0.U
  mst_chn_aw0.bits.len := (dw*bk/bus_w-1).U
  mst_chn_aw0.bits.lock := 0.U
  mst_chn_aw0.bits.port := 0.U
  mst_chn_aw0.bits.qos  := 0.U
  mst_chn_aw0.bits.size := bus_lsb.U
  mst_chn_aw0.bits.user := 0.U

  mst_chn_w0.bits.data := cache_dat.dat_addr_r(info_slvAcquire_cb)
  mst_chn_w0.bits.last := is_mstReleaseData_addrend
  mst_chn_w0.bits.strb := Fill(16, 1.U)
  mst_chn_w0.bits.user := 0.U



  for ( i <- 0 until cb ) yield {
    info_mstReleaseData_cache_dat_ren(i) := 
      i.U === info_slvAcquire_cb & is_mstReleaseData_state_qout === 1.U
  }
  info_mstReleaseData_cache_dat_raddr := info_mstReleaseData_address

  when( is_mstReleaseData_state_qout =/= 0.U & is_mstReleaseData_state_dnxt === 0.U ) {
      cache_inv(info_slvAcquire_cl)(info_slvAcquire_cb) := true.B
    for ( i <- 0 until bk ) yield {

      cache_mdf(info_slvAcquire_cl)(info_slvAcquire_cb)(i) := false.B
    }
  }

}

trait AXI_mst_releaseAck extends TLC_base {
  val mst_chn_b0 = Flipped(new DecoupledIO( new AXI_chn_b( 1, 1 )))



  val mst_chn_b0_ready = RegInit(false.B)
  mst_chn_b0.ready := mst_chn_b0_ready


  when( mst_chn_b0.fire ) {
    mst_chn_b0_ready := false.B;
    is_mstReleaseData_StateOn := false.B
  }
  .elsewhen( is_mstReleaseAck_allowen ) {
    mst_chn_b0_ready := true.B
  }


}


trait AXI_mst_R extends TLC_base with AXI_mst_releaseReleaseData with AXI_mst_releaseAck

