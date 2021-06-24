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

trait AXI_mst_releaseReleaseData extends AXI_mst_evict {

  val is_mstReleaseData_invalid = cache_inv(info_slvAcquire_cl)(info_slvAcquire_cb) === true.B
  val is_mstReleaseData_clean = cache_mdf(info_slvAcquire_cl)(info_slvAcquire_cb).forall( (x:Bool) => (x === false.B) )



  when( is_mstReleaseData_allowen ) {
    is_mstReleaseData_Waiting := false.B

    when( (is_mstReleaseData_clean | is_mstReleaseData_invalid) ) {
      cache_inv(info_slvAcquire_cl)(info_slvAcquire_cb) := true.B
      // for ( i <- 0 until bk ) yield { cache_mdf(info_slvAcquire_cl)(info_slvAcquire_cb)(i) := false.B }
    }
    .otherwise {
      is_mstReleaseEvict_Waiting := true.B
      is_mstReleaseData_StateOn := true.B
    }
  }

}

trait AXI_mst_releaseAck extends TLC_base {
  is_mstReleaseAck_allowen := false.B
  is_mstReleaseAck_StateOn := false.B
  is_mstReleaseAck_valid   := false.B

}







trait AXI_mst_evict extends TLC_base {
  val mst_chn_aw = IO( new DecoupledIO(new AXI_chn_a( 64, 1, 1 )) )
  val mst_chn_w  = IO(new DecoupledIO(new AXI_chn_w( 128, 1 )) )
  val mst_chn_b = IO(Flipped(new DecoupledIO( new AXI_chn_b( 1, 1 ))))

  val is_mstReleaseEvict_Waiting = RegInit(false.B)
  val is_mstReleaseEvict_allowen = Wire(Bool())
  val is_mstReleaseEvict_StateOn = RegInit(false.B)
  val info_mstRlseEvict_address = RegInit(0.U(64.W))

  val is_mstEvict_StateOn = is_mstReleaseEvict_StateOn | is_mstProbeAckData_StateOn

  val mstEvict_state_dnxt = Wire( UInt(2.W) )
  val mstEvict_state_qout = RegNext( mstEvict_state_dnxt, 0.U )

  val info_mstEvict_address =
    Mux1H(Seq(
      is_mstReleaseEvict_StateOn -> (info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)),
      is_mstProbeAckData_StateOn -> info_mstRecProbe_address
    ))

  val is_mstProbeData_addrend = info_mstProbeData_address( addr_lsb-1, bus_lsb ).andR
  val is_mstRlseEvict_addrend = info_mstRlseEvict_address( addr_lsb-1, bus_lsb ).andR



  when( ~is_mstEvict_StateOn & is_mstProbeAckData_allowen ) {
    info_mstProbeData_address := info_mstRecProbe_address
  } .elsewhen( is_mstReleaseEvict_StateOn & mst_chn_w.fire & ~is_mstProbeData_addrend ) {
    info_mstProbeData_address := info_mstProbeData_address + (1.U << bus_lsb)
  }

  when( ~is_mstEvict_StateOn & is_mstReleaseEvict_allowen ) {
    info_mstRlseEvict_address := info_slvAcquire_address & (Fill(64, 1.U) << addr_lsb)
  } .elsewhen( is_mstProbeAckData_StateOn & mst_chn_w.fire & ~is_mstRlseEvict_addrend ) {
    info_mstRlseEvict_address := info_mstRlseEvict_address + (1.U << bus_lsb)
  }


  when( ~is_mstEvict_StateOn ) {
    when( is_mstReleaseEvict_allowen ) {
      is_mstReleaseEvict_Waiting := false.B
      is_mstReleaseEvict_StateOn := true.B
    }
    .elsewhen( is_mstProbeAckData_allowen ) {
      is_mstProbeAckData_Waiting := false.B;
      is_mstProbeAckData_StateOn := true.B
    }
  }










  mstEvict_state_dnxt := 
    Mux1H(Seq(
      ( mstEvict_state_qout === 0.U ) -> Mux( is_mstEvict_StateOn, 1.U, 0.U),
      ( mstEvict_state_qout === 1.U ) -> Mux( mst_chn_aw.fire, 2.U, 1.U ),
      ( mstEvict_state_qout === 2.U ) -> Mux( mst_chn_b.fire, 0.U, 2.U ),
    ))


  when( mstEvict_state_qout === 2.U & mstEvict_state_dnxt === 0.U ) {
    when( is_mstReleaseEvict_StateOn ) {
      is_mstReleaseEvict_StateOn := false.B
      is_mstReleaseData_StateOn := false.B
    }
    when( is_mstProbeAckData_StateOn ) {
      is_mstProbeAckData_StateOn := false.B

    }
  }












  val mst_chn_aw_valid = RegInit(false.B)
  val mst_chn_w_valid = RegInit(false.B)
  val mst_chn_b_ready = RegInit(false.B)

  mst_chn_aw_valid := mst_chn_aw_valid
  mst_chn_w_valid  := mst_chn_w_valid
  mst_chn_b_ready  := mst_chn_b_ready

  when( mstEvict_state_qout === 0.U & mstEvict_state_dnxt === 1.U ) { mst_chn_aw_valid := true.B }
  .elsewhen( mst_chn_aw.fire ) { mst_chn_aw_valid := false.B }

  when( mstEvict_state_qout === 2.U & ~mst_chn_w.valid ) { mst_chn_w_valid := true.B }
  .elsewhen( mst_chn_w.fire ) { mst_chn_w_valid := false.B }

  when( mst_chn_b.valid ) { mst_chn_b_ready := true.B }
  .elsewhen( mst_chn_b.fire ) { mst_chn_b_ready := false.B }

  mst_chn_aw.bits.addr := 
    Mux1H(Seq(
      is_mstProbeAckData_StateOn -> info_mstProbeData_address,
      is_mstReleaseEvict_StateOn -> info_mstRlseEvict_address
    ))

  mst_chn_aw.bits.burst := "b01".U
  mst_chn_aw.bits.cache := 0.U
  mst_chn_aw.bits.id := 0.U
  mst_chn_aw.bits.len :=
    Mux1H(Seq(
      is_mstProbeAckData_StateOn -> (dw*bk/bus_w-1).U,
      is_mstReleaseEvict_StateOn -> (dw/bus_w-1).U
    ))

  mst_chn_aw.bits.lock := 0.U
  mst_chn_aw.bits.port := 0.U
  mst_chn_aw.bits.qos  := 0.U
  mst_chn_aw.bits.size := bus_lsb.U
  mst_chn_aw.bits.user := 0.U
  
  mst_chn_w.bits.data :=
    Mux1H(Seq(
      is_mstProbeAckData_StateOn -> cache_dat.dat_addr_r(info_mstProbe_cb ),
      is_mstReleaseEvict_StateOn  -> cache_dat.dat_addr_r(info_slvAcquire_cb)
    ))

  mst_chn_w.bits.last :=
    Mux1H(Seq(
      is_mstProbeAckData_StateOn -> is_mstProbeData_addrend,
      is_mstReleaseEvict_StateOn -> is_mstRlseEvict_addrend      
    ))

  mst_chn_w.bits.strb := Fill(16, 1.U)
  mst_chn_w.bits.user := 0.U






  for ( i <- 0 until cb ) yield {
    info_mstProbeData_cache_dat_ren(i) :=
      i.U === info_mstProbe_cb & mstEvict_state_qout === 2.U
  }
  info_mstProbeData_cache_dat_raddr := info_mstProbeData_address

  for ( i <- 0 until cb ) yield {
    info_mstReleaseData_cache_dat_ren(i) := 
      i.U === info_slvAcquire_cb & mstEvict_state_qout === 2.U
  }
  info_mstReleaseData_cache_dat_raddr := info_mstReleaseData_address




}



trait AXI_mst_R extends TLC_base with AXI_mst_releaseReleaseData with AXI_mst_releaseAck




