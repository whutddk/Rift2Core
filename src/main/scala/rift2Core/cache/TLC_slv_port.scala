
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

package rift2Core.cache

import chisel3._
import chisel3.util._
import chisel3.util.random._


import tilelink._
import axi._
import base._





abstract class TLC_slv_port( dw:Int = 1024, bk:Int = 4, cb:Int = 4, cl:Int = 25, mst_num:Int = 3, mst_size:Int )  extends TLC_ram( dw, bk, cb, cl, mst_size ){

  val slv_chn_a = Vec(mst_num, Flipped(new DecoupledIO(new TLchannel_a(128, 32))))
  val slv_chn_b = Vec(mst_num, new DecoupledIO(new TLchannel_b(128, 32)))
  val slv_chn_c = Vec(mst_num, Flipped(new DecoupledIO(new TLchannel_c(128, 32))))
  val slv_chn_d = Vec(mst_num, new DecoupledIO( new TLchannel_d(128)))
  val slv_chn_e = Vec(mst_num, Flipped(new DecoupledIO( new TLchannel_e)))

 val aqblk_agent_no = {
    if ( mst_num == 1 ) {
      0.U(1.W)
    }
    else {
      val value = RegInit(0.U((log2Ceil(mst_num)).W))

      val req = for ( i <- 0 until mst_num ) yield { slv_chn_a(i).valid }
      val no = for ( i <- 0 until mst_num ) yield { i.U }

      when( state_qout === cfree & state_dnxt === cktag ) {
        value := PriorityMux( req zip no )
      }
      value
    }
  }
  





  val wbblk_agent_no = {
    if ( mst_num == 1 ) {
      0.U(1.W)
    }
    else {
      val value = RegInit(0.U((log2Ceil(mst_num)).W))

      val req = for ( i <- 0 until mst_num ) yield { slv_chn_c(i).valid }
      val no = for ( i <- 0 until mst_num ) yield { i.U }

      when( state_qout === rlese & release_addr === 0.U & slv_chn_c.exists((x:DecoupledIO[TLchannel_c]) => (x.valid === true.B))  ) {
        value := PriorityMux( req zip no )
        
      }

      value
    }
  }

  assert( mst_num > 0, "Invalid Setting at TLC-L3, mst number must larger than 0" )


  when( state_qout === rlese ) {
    when( slv_chn_c.exists((x:DecoupledIO[TLchannel_c]) => (x.valid === true.B)) & release_addr === 0.U ) {

      val req = for ( i <- 0 until mst_num ) yield { slv_chn_c(i).valid }
      val no = for ( i <- 0 until mst_num ) yield { i.U }
      val idx = PriorityMux( req zip no )

      release_addr := slv_chn_c(idx).bits.address
    }
    .elsewhen( is_release_bus_fire & ~is_release_bus_end ) { release_addr := release_addr + ( 1.U << bus_lsb ) }

    .elsewhen( is_release_bus_end & (slv_chn_c(wbblk_agent_no).bits.opcode === Opcode.ProbeAck | slv_chn_c(wbblk_agent_no).bits.opcode === Opcode.ProbeAckData) ) { release_addr := 0.U }
    .elsewhen( slv_chn_d(wbblk_agent_no).fire & slv_chn_d(wbblk_agent_no).bits.opcode === Opcode.ReleaseAck ) { release_addr := 0.U }
  
  }

  for ( i <- 0 until mst_num ) yield {
    assert( ~(slv_chn_c(i).valid & slv_chn_c(i).bits.address(mst_lsb-1,0) =/= 0.U), "Assert Failed at tlc, the address of chn_c is misalign" )
  }






  override val is_aqblk_req = (state_qout === cfree) & slv_chn_a.exists((x:DecoupledIO[TLchannel_a]) => (x.valid === true.B) )
  override val is_wbblk_req = (state_qout === cfree) & slv_chn_c.exists((x:DecoupledIO[TLchannel_c]) => (x.valid === true.B) )

  assert( ~((is_op_aqblk | is_op_wbblk | is_op_fence) & (state_qout === cfree)), "Assert Failed at TLC_slv_port, when cfree, no op can be valid" )




  override val is_release_bus_fire = slv_chn_c(wbblk_agent_no).fire
  override val is_grant_bus_fire = slv_chn_d(aqblk_agent_no).fire



  override val is_probe_rtn = RegInit(VecInit(Seq.fill(mst_num)(false.B)))
  when( state_qout === probe & state_dnxt === rlese ) { is_probe_rtn := 0.U.asBools }
  .elsewhen( state_qout === rlese ) {
      when(
        is_release_bus_end & (
          slv_chn_c(wbblk_agent_no).bits.opcode === Opcode.ProbeAckData |
          slv_chn_c(wbblk_agent_no).bits.opcode === Opcode.ProbeAck
        )
      ) { is_probe_rtn(wbblk_agent_no) := true.B }

  }

  override val is_release_with_block = 
    is_release_bus_fire & (
      slv_chn_c(wbblk_agent_no).bits.opcode === Opcode.ReleaseData |
      slv_chn_c(wbblk_agent_no).bits.opcode === Opcode.ProbeAckData
    )

  override val release_data = slv_chn_c(wbblk_agent_no).bits.data

  override val is_probe_fire = RegInit( VecInit(Seq.fill(mst_num)(false.B)) )

  val a_ready = RegInit(VecInit(Seq.fill(mst_num)(false.B)))
  val b_valid = RegInit(VecInit(Seq.fill(mst_num)(false.B)))

  val c_ready = RegInit(VecInit(Seq.fill(mst_num)(false.B)))
  val d_valid = RegInit(VecInit(Seq.fill(mst_num)(false.B)))
  val d_bits  = RegInit(0.U.asTypeOf(new TLchannel_d(128)))
  val e_ready = RegInit(VecInit(Seq.fill(mst_num)(false.B)))

  for ( i <- 0 until mst_num ) yield {
    slv_chn_a(i).ready := a_ready(i)
    slv_chn_b(i).valid := b_valid(i)

    slv_chn_c(i).ready := c_ready(i)
    slv_chn_d(i).valid := d_valid(i)
    slv_chn_d(i).bits  := d_bits 
    slv_chn_e(i).ready := e_ready(i)
  }




  when( state_qout === cktag & state_dnxt === grant & is_op_aqblk ) { a_ready(aqblk_agent_no) := true.B }
  .elsewhen( slv_chn_a(aqblk_agent_no).fire ) { a_ready(aqblk_agent_no) := false.B }
  
  for ( i <- 0 until mst_num ) yield {
    when( state_qout === cktag & state_dnxt === probe ) { b_valid(i) := true.B; is_probe_fire(i) := false.B }
    .elsewhen( slv_chn_b(i).fire ) { b_valid(i) := false.B; is_probe_fire(i) := true.B }
  }

  for ( i <- 0 until mst_num ) yield {
    slv_chn_b(i).bits.opcode  := Opcode.ProbeBlock
    slv_chn_b(i).bits.param   := TLparam.toN
    slv_chn_b(i).bits.size    := log2Ceil(mst_size/8).U
    slv_chn_b(i).bits.source  := DontCare
    slv_chn_b(i).bits.address := probe_addr
    slv_chn_b(i).bits.mask    := DontCare
    slv_chn_b(i).bits.data    := DontCare
    slv_chn_b(i).bits.corrupt := false.B
  } 


  when( state_qout === rlese & release_addr =/= 0.U  ) {
    when( slv_chn_c(wbblk_agent_no).valid & ~slv_chn_c(wbblk_agent_no).ready ) { c_ready(wbblk_agent_no) := true.B }
    .elsewhen( slv_chn_c(wbblk_agent_no).fire ) { c_ready(wbblk_agent_no) := false.B }
  }


  when( state_qout === rlese ) {
    when( is_release_bus_end & slv_chn_c(wbblk_agent_no).bits.opcode === Opcode.ReleaseData) { d_valid(wbblk_agent_no) := true.B }
    .elsewhen( slv_chn_d(wbblk_agent_no).fire ) { d_valid(wbblk_agent_no) := false.B }
  }
  .elsewhen( state_qout === grant ) {
    when( slv_chn_d(aqblk_agent_no).valid === false.B ) { d_valid(aqblk_agent_no) := true.B }
    .elsewhen( slv_chn_d(aqblk_agent_no).fire ) { d_valid(aqblk_agent_no) := false.B }
  }


  d_bits.opcode  := Mux1H(Seq(
    ( state_qout === rlese ) -> Opcode.ReleaseAck,
    ( state_qout === grant ) -> Opcode.GrantData
  ))
  
  d_bits.param   := Mux1H(Seq(
    ( state_qout === rlese ) -> 0.U,
    ( state_qout === grant ) -> TLparam.toT
  ))

  d_bits.size    := log2Ceil(mst_size/8).U
  d_bits.source  := DontCare
  d_bits.sink    := DontCare
  d_bits.denied  := false.B
  d_bits.data    := mem_dat
  d_bits.corrupt := false.B



  when( slv_chn_e(aqblk_agent_no).valid ) { e_ready(aqblk_agent_no) := true.B }
  .elsewhen(slv_chn_e(aqblk_agent_no).fire) { e_ready(aqblk_agent_no) := false.B }



}


