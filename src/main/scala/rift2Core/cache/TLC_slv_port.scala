
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





abstract class TLC_slv_port( dw:Int = 1024, bk:Int = 4, cb:Int = 4, cl:Int = 25, mst_num:Int, mst_size:Int ) extends TLC_ram( dw, bk, cb, cl, mst_size ){

  val slv_chn_a = IO(Flipped(new DecoupledIO(new TLchannel_a(128, 32))))
  val slv_chn_b = IO(new DecoupledIO(new TLchannel_b(128, 32)))
  val slv_chn_c = IO(Flipped(new DecoupledIO(new TLchannel_c(128, 32))))
  val slv_chn_d = IO(new DecoupledIO( new TLchannel_d(128)))
  val slv_chn_e = IO(Flipped(new DecoupledIO( new TLchannel_e)))






  when( state_qout === rlese ) {
    when( slv_chn_c.valid === true.B & release_addr === 0.U ) {
      release_addr := slv_chn_c.bits.address
    }
    .elsewhen( is_release_bus_fire & ~is_release_bus_end ) { release_addr := release_addr + ( 1.U << bus_lsb ) }

    .elsewhen( is_release_bus_end & (slv_chn_c.bits.opcode === Opcode.ProbeAck | slv_chn_c.bits.opcode === Opcode.ProbeAckData) ) { release_addr := 0.U }
    .elsewhen( slv_chn_d.fire & slv_chn_d.bits.opcode === Opcode.ReleaseAck ) { release_addr := 0.U }
  
  }


  assert( ~(slv_chn_c.valid & slv_chn_c.bits.address(mst_lsb-1,0) =/= 0.U), "Assert Failed at tlc, the address of chn_c is misalign" )







  override val is_aqblk_req = (state_qout === cfree & slv_chn_a.valid === true.B)
  override val is_wbblk_req = (state_qout === cfree & slv_chn_c.valid === true.B)

  assert( ~((is_op_aqblk | is_op_wbblk | is_op_fence) & (state_qout === cfree)), "Assert Failed at TLC_slv_port, when cfree, no op can be valid" )




  override val is_release_bus_fire = slv_chn_c.fire
  override val is_grant_bus_fire = slv_chn_d.fire



  override val is_probe_rtn = RegInit(VecInit(Seq.fill(mst_num)(false.B)))
  when( state_qout === probe & state_dnxt === rlese ) { is_probe_rtn := 0.U.asBools }
  .elsewhen( state_qout === rlese ) {
      when(
        is_release_bus_end & (
          slv_chn_c.bits.opcode === Opcode.ProbeAckData |
          slv_chn_c.bits.opcode === Opcode.ProbeAck
        )
      ) { is_probe_rtn := true.B }

  }

  override val is_release_with_block = 
    is_release_bus_fire & (
      slv_chn_c.bits.opcode === Opcode.ReleaseData |
      slv_chn_c.bits.opcode === Opcode.ProbeAckData
    )

  override val release_data = slv_chn_c.bits.data

  override val is_probe_fire = slv_chn_b.fire

  val a_ready = RegInit(false.B)
  val b_valid = RegInit(false.B)

  val c_ready = RegInit(false.B)
  val d_valid = RegInit(false.B)
  val d_bits  = RegInit(0.U.asTypeOf(new TLchannel_d(128)))
  val e_ready = RegInit(false.B)


    slv_chn_a.ready := a_ready
    slv_chn_b.valid := b_valid

    slv_chn_c.ready := c_ready
    slv_chn_d.valid := d_valid
    slv_chn_d.bits  := d_bits 
    slv_chn_e.ready := e_ready





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








  := slv_chn_a().bits.opcode  
  := slv_chn_a().bits.param   
  := slv_chn_a().bits.size    
  := slv_chn_a().bits.source  
  := slv_chn_a().bits.address 
  := slv_chn_a().bits.mask    
  := slv_chn_a().bits.data    
  := slv_chn_a().bits.corrupt 



  := slv_chn_c().bits.opcode  
  := slv_chn_c().bits.param   
  := slv_chn_c().bits.size    
  := slv_chn_c().bits.source  
  := slv_chn_c().bits.address 
  := slv_chn_c().bits.data    
  := slv_chn_c().bits.corrupt 

  := slv_chn_e().bits.sink









}


