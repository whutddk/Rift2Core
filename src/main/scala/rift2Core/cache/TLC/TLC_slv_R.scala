
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



trait TLC_slv_releaseReleaseData extends TLC_base {
  val slv_chn_c1 = IO(Flipped(new DecoupledIO(new TLchannel_c(128, 32))))

  is_slvReleaseData_valid := slv_chn_c1.valid


  val c1_ready = RegInit(false.B)
  slv_chn_c1.ready := c1_ready


  val is_slvReleaseData_addrend = info_slvReleaseData_address( mst_lsb-1, bus_lsb ).andR

  info_slvReleaseData_bk := info_slvReleaseData_address(addr_lsb-1, addr_lsb-log2Ceil(bk) )
  info_slvReleaseData_cl := info_slvReleaseData_address(addr_lsb+line_w-1, addr_lsb)


  val slvReleaseData_state_dnxt = Wire(UInt(2.W))
  val slvReleaseData_state_qout = RegNext(slvReleaseData_state_dnxt, 0.U)

  slvReleaseData_state_dnxt := 
    Mux1H(Seq(
      ( slvReleaseData_state_qout === 0.U ) -> Mux( is_slvReleaseData_valid, 1.U, 0.U ),
      ( slvReleaseData_state_qout === 1.U ) -> 2.U,
      ( slvReleaseData_state_qout === 2.U ) -> Mux( is_slvReleaseData_addrend | (slv_chn_c1.bits.opcode === Opcode.ReleasePerm), 0.U, 2.U )
    ))
    

  when( slv_chn_c1.fire ) { c1_ready := false.B }
  .elsewhen( slv_chn_c1.valid & slvReleaseData_state_qout === 2.U ) { c1_ready := true.B }

  when( slvReleaseData_state_dnxt =/= 0.U ) { is_slvReleaseData_StateOn := true.B }


  info_slvReleaseData_source := RegEnable( slv_chn_c1.bits.source, slvReleaseData_state_dnxt === 1.U )

  when( slvReleaseData_state_qout === 0.U & slvReleaseData_state_dnxt === 1.U ) {
    info_slvReleaseData_address := slv_chn_c1.bits.address
  }
  .elsewhen( slv_chn_c1.fire ) {
    info_slvReleaseData_address := info_slvReleaseData_address + (1.U << bus_lsb)
  }

  when( slvReleaseData_state_qout === 2.U & slvReleaseData_state_dnxt === 0.U ) {
    cache_mdf(info_slvReleaseData_cl)(info_slvReleaseData_cb)(info_slvReleaseData_bk) := true.B
    is_slvReleaseAck_Waiting := true.B
  }


  info_slvReleaseData_cb := {
    val tag_info = slv_chn_c1.bits.address( 31, 32-tag_w )
    val is_cb_hit =
          VecInit(
            for ( i <- 0 until cb ) yield cache_tag.tag_info_r(i) === tag_info
          )
    val hit_cb = OHToUInt(is_cb_hit.asUInt)


    assert( ~(slvReleaseData_state_qout === 1.U & slvReleaseData_state_dnxt === 2.U &
      is_cb_hit.forall((x:Bool) => (x === false.B))), "Assert Failed at TLC_slv_R.scala!! Your master agent release a missing block!"  )
    
    RegEnable( hit_cb, slvReleaseData_state_qout === 1.U & slvReleaseData_state_dnxt === 2.U ) 
  }

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
      info_slvReleaseData_cache_coh_wen(i)(j) := 
        ( i.U === info_slvReleaseData_cb ) & ( j.U === info_slvReleaseData_bk ) &
        (slvReleaseData_state_qout === 2.U & slvReleaseData_state_dnxt === 0.U)
  }

  info_slvReleaseData_cache_coh_waddr := info_slvReleaseData_address
  info_slvReleaseData_cache_coh_winfo := 0.U

  for ( i <- 0 until cb ) yield {
    info_slvReleaseData_cache_dat_wen(i)   := 
      ( i.U === info_slvReleaseData_cb ) &
      slvReleaseData_state_qout === 2.U & slv_chn_c1.bits.opcode === Opcode.ReleaseData & slv_chn_c1.fire
  }
  info_slvReleaseData_cache_dat_waddr := info_slvReleaseData_address
  info_slvReleaseData_cache_dat_wstrb := "hffff".U
  info_slvReleaseData_cache_dat_winfo := slv_chn_c1.bits.data


}


trait TLC_slv_releaseAck extends TLC_base {
  val slv_chn_d1 = IO(new DecoupledIO( new TLchannel_d(128)))



  val slv_chn_d1_valid = RegInit(false.B)

  when( slv_chn_d1.fire ) {
    slv_chn_d1_valid := false.B;
    is_slvReleaseData_StateOn := false.B
  }
  .elsewhen( is_slvReleaseAck_allowen ) {
    slv_chn_d1_valid := true.B
    is_slvReleaseAck_Waiting := false.B
  }

  slv_chn_d1.valid := slv_chn_d1_valid
  slv_chn_d1.bits.corrupt := false.B
  slv_chn_d1.bits.data := DontCare
  slv_chn_d1.bits.denied := false.B
  slv_chn_d1.bits.opcode := Opcode.ReleaseAck
  slv_chn_d1.bits.param  := 0.U
  slv_chn_d1.bits.sink   := DontCare
  slv_chn_d1.bits.source := info_slvReleaseData_source
  slv_chn_d1.bits.size   := mst_lsb.U


}

trait TLC_slv_R extends TLC_base with TLC_slv_releaseReleaseData with TLC_slv_releaseAck




