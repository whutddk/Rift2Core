


/*
  Copyright (c) 2020 - 2021 Ruige Lee <295054118@qq.com>

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
import chisel3.util.random._

//产生寄生消息由Waiting
//发生总线请求用valid
//操作占用用stateOn




/**
  * this operation will happen once when free
  */
trait TLC_slv_acquire extends TLC_base{
  val slv_chn_a = IO(Flipped(new DecoupledIO(new TLchannel_a(128, 32))))


  val a_ready = RegInit(false.B)
  slv_chn_a.ready := a_ready


  when( slv_chn_a.fire ) { a_ready := false.B }
  .elsewhen( is_slvAcquire_allowen ) { a_ready := true.B }

  for( i <- 0 until cb ) yield {
    info_slvAcquire_cache_tag_ren(i) := is_slvAcquire_allowen    
  }
  info_slvAcquire_cache_tag_raddr := slv_chn_a.bits.address

  info_slvAcquire_address := RegEnable( slv_chn_a.bits.address, is_slvAcquire_allowen )
  info_slvAcquire_source  := RegEnable( slv_chn_a.bits.source,  is_slvAcquire_allowen )
  info_slvAcquire_cb := {
    val tag_info = slv_chn_a.bits.address( 31, 32-tag_w )
    val is_cb_hit =
          VecInit(
            for ( i <- 0 until cb ) yield cache_tag.tag_info_r(i) === tag_info
          )

    val hit_cb = OHToUInt(is_cb_hit.asUInt)
    val empty_cb = cache_tag.tag_info_r.indexWhere((x:UInt) => (x === 0.U))
    val cb_sel =
      Mux( is_cb_hit.contains(true.B), hit_cb,
        Mux( cache_tag.tag_info_r.contains(0.U), empty_cb,
          LFSR(log2Ceil(16), true.B )
      ))
    RegEnable( cb_sel, slv_chn_a.fire )
  }

  info_slvAcquire_bk := info_slvAcquire_address(addr_lsb-1, addr_lsb-log2Ceil(bk) )
  info_slvAcquire_cl := info_slvAcquire_address(addr_lsb+line_w-1, addr_lsb)


  val is_SlvAcquire_StateOn = RegInit( false.B )

  when( slv_chn_a.fire ) {
    is_slvAcquire_StateOn := true.B
    is_slvGrantData_Waiting := true.B
  }

}

/**
  * this operation will happened every times when free and is_pending_slv_acquire
  */
trait TLC_slv_grantData extends TLC_base{
  val slv_chn_d0 = IO(new DecoupledIO( new TLchannel_d(128)))

  val slvGrantData_State_dnxt = Wire(UInt(3.W))
  val slvGrantData_State_qout = RegNext(slvGrantData_State_dnxt, 0.U)
  val is_slvGrantData_hit_clearen = cache_tag.tag_info_r(info_slvAcquire_cb) === info_slvGrantData_address(31, 32-tag_w)
  val is_slvGrantData_coh_clearen =
    cache_coh.coh_info_r( info_slvAcquire_bk ) =/= 0.U &
    cache_inv(info_slvAcquire_cl)(info_slvAcquire_cb)(info_slvAcquire_bk) === false.B
    // cache_mdf(info_slvAcquire_cl)(info_slvAcquire_cb)(info_slvAcquire_bk) === false.B


  val is_slvGrantData_clearen = is_slvGrantData_hit_clearen & is_slvGrantData_coh_clearen
  val is_slvGrantData_addrend = info_slvGrantData_address( mst_lsb-1, bus_lsb ).andR

  for ( i <- 0 until cb ) yield {
    info_slvGrantData_cache_tag_ren(i) :=
      i.U === info_slvAcquire_cb & 
      slvGrantData_State_qout === 0.U & slvGrantData_State_dnxt === 1.U
  }
 
  info_slvGrantData_cache_tag_raddr := info_slvAcquire_address

  for ( i <- 0 until bk ) yield {
    info_slvGrantData_cache_coh_ren(i) :=
      i.U === info_slvAcquire_bk & 
      slvGrantData_State_qout === 0.U & slvGrantData_State_dnxt === 1.U
  }
  
  
  info_slvGrantData_cache_coh_raddr := info_slvAcquire_address

  for ( i <- 0 until cb ) yield {
    info_slvGrantData_cache_dat_ren(i) :=
      i.U === info_slvAcquire_cb &
      slvGrantData_State_qout === 2.U
  } 
 
  info_slvGrantData_cache_dat_raddr := info_slvGrantData_address

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    info_slvGrantData_cache_coh_wen(i)(j) := 
      (i.U === info_slvAcquire_cb) & (j.U === info_slvAcquire_bk) &
      slvGrantData_State_qout === 2.U & slvGrantData_State_dnxt === 0.U
  }

  info_slvGrantData_cache_coh_waddr := info_slvAcquire_address
  info_slvGrantData_cache_coh_winfo := info_slvAcquire_source



  val d_valid = RegInit(false.B)



  slv_chn_d0.valid := d_valid

  slv_chn_d0.bits.opcode  := Opcode.GrantData
  slv_chn_d0.bits.param   := TLparam.toT
  slv_chn_d0.bits.size    := mst_lsb.U
  slv_chn_d0.bits.source  := info_slvAcquire_source
  slv_chn_d0.bits.sink    := DontCare
  slv_chn_d0.bits.denied  := false.B
  slv_chn_d0.bits.data    := cache_dat.dat_info_r(info_slvAcquire_cb)
  slv_chn_d0.bits.corrupt := false.B

  when( slvGrantData_State_qout === 2.U & slv_chn_d0.valid === false.B ) {
    d_valid := true.B
  }
  .elsewhen( slv_chn_d0.fire ) {
    d_valid := false.B
  }






  when( slvGrantData_State_qout === 1.U & slvGrantData_State_dnxt === 2.U ) {
    info_slvGrantData_address := info_slvAcquire_address
  }
  .elsewhen( slvGrantData_State_qout === 2.U ) {
    info_slvGrantData_address := Mux( slv_chn_d0.fire, info_slvGrantData_address + (1.U << bus_lsb) , info_slvGrantData_address )
  }


  slvGrantData_State_dnxt :=
    Mux1H(Seq(
      (slvGrantData_State_qout === 0.U) -> Mux( is_slvGrantData_allowen, 1.U, 0.U ),
      (slvGrantData_State_qout === 1.U) -> Mux( is_slvGrantData_clearen, 2.U, 0.U ),
      (slvGrantData_State_qout === 2.U) -> Mux( slv_chn_d0.fire & is_slvGrantData_addrend, 0.U, 2.U )
    ))
  when( true.B ) { is_slvGrantData_StateOn := slvGrantData_State_dnxt =/= 0.U }


  when( slvGrantData_State_qout === 1.U ) {
    when( cache_inv(info_slvAcquire_cl)(info_slvAcquire_cb)(info_slvAcquire_bk) === true.B ) {
      is_mstAcquire_Waiting := true.B
    }
    .otherwise {
      when( cache_coh.coh_info_r(info_slvAcquire_bk) =/= 0.U ) {
        is_slvProbe_Waiting := true.B
        info_slvGrantData_exclusive := cache_coh.coh_info_r(info_slvAcquire_bk)
      }
      .otherwise {
        when( cache_tag.tag_info_r(info_slvAcquire_cb) =/= info_slvGrantData_address(31, 32-tag_w) ) {
          is_mstReleaseData_Waiting := true.B
        }
      }
    }
  }

  when( slvGrantData_State_qout === 1.U & slvGrantData_State_dnxt === 2.U ) {
    is_slvGrantData_StateOn := true.B
    is_slvGrantData_Waiting := false.B
  }





}

trait TLC_slv_grantAck extends TLC_base {
  val slv_chn_e = IO(Flipped(new DecoupledIO( new TLchannel_e)))

  val e_ready = RegInit(false.B)
  slv_chn_e.ready := e_ready

  when( is_slvGrantAck_allowen ) { e_ready := true.B }

  when( slv_chn_e.fire ) {
    is_slvAcquire_StateOn   := false.B
    is_slvGrantData_StateOn := false.B
  }




}


trait TLC_slv_A extends TLC_base with TLC_slv_acquire with TLC_slv_grantAck with TLC_slv_grantData






