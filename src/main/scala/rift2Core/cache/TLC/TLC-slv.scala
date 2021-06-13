


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
import axi._
import base._

//state: free grant ack_release req_probe ack_probe

// free flash evict


trait TLC_slv extends MultiIOModule {

  val slv_chn_a = IO(Flipped(new DecoupledIO(new TLchannel_a(128, 32))))
  val slv_chn_b = IO(new DecoupledIO(new TLchannel_b(128, 32)))
  val slv_chn_c = IO(Flipped(new DecoupledIO(new TLchannel_c(128, 32))))
  val slv_chn_d = IO(new DecoupledIO( new TLchannel_d(128)))
  val slv_chn_e = IO(Flipped(new DecoupledIO( new TLchannel_e)))


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

  when( slv_chn_e.valid ) { e_ready := true.B }
  .elsewhen(slv_chn_e.fire) { e_ready := false.B }


  val is_in_grant = RegInit(false.B)

  when( slv_chn_c.fire & slv_chn_c.bits.opcode === Opcode.ReleaseData ) {
    d_valid := true.B
    d_bits_opcode := Opcode.ReleaseAck
    d_bits_param := 0.U
    d_bits_source := slv_chn_c.bits.source
  }
  .elsewhen( is_in_grant & ~d_valid ) {
    d_valid := true.B
    d_bits_opcode := Opcode.GrantData
    d_bits_param := TLparam.toT
    d_bits_source := slv_chn_a.bits.source
  }
  .elsewhen( slv_chn_d.fire ) { d_valid := false.B }

  d_bits.opcode := d_bits_opcode
  d_bits.param := d_bits_param
  d_bits.size    := log2Ceil(mst_size/8).U
  d_bits.source  := slv_chn_a.bits_source
  d_bits.sink    := DontCare
  d_bits.denied  := false.B
  d_bits.data    := mem_dat
  d_bits.corrupt := false.B


  when( slv_chn_c.valid === true.B & release_addr === 0.U ) {
    release_addr := slv_chn_c.bits.address
  }
  .elsewhen( is_release_bus_fire & ~is_release_bus_end ) { release_addr := release_addr + ( 1.U << bus_lsb ) }
  .elsewhen( is_release_bus_end_with_probe ) { release_addr := 0.U }
  .elsewhen( is_release_ack ) { release_addr := 0.U }
  
  when( state_qout === rlese & release_addr =/= 0.U  ) {
    when( slv_chn_c.valid & ~slv_chn_c.ready ) { c_ready := true.B }
    .elsewhen( slv_chn_c.fire ) { c_ready := false.B }
  }


  assert( ~(slv_chn_c.valid & slv_chn_c.bits.address(mst_lsb-1,0) =/= 0.U), "Assert Failed at tlc, the address of chn_c is misalign" )


  val probe_req:
  val probe_addr
  val probe_
    when( state_qout === cktag & state_dnxt === probe ) { b_valid := true.B;  }
    .elsewhen( slv_chn_b.fire ) { b_valid := false.B }



  when( state_qout === probe & state_dnxt === rlese ) { is_probe_rtn := 0.U.asBools }
  .elsewhen( state_qout === rlese ) {
      when( is_release_bus_end_with_probe ) { 
        val idx = slv_chn_c.bits.source
        is_probe_rtn(idx) := true.B
      }

  }













  when( state_qout === cktag & state_dnxt === grant & is_op_aqblk ) { a_ready := true.B }
  .elsewhen( slv_chn_a.fire ) { a_ready := false.B }
  




    slv_chn_b.bits.opcode  := Opcode.ProbeBlock
    slv_chn_b.bits.param   := TLparam.toN
    slv_chn_b.bits.size    := log2Ceil(mst_size/8).U
    slv_chn_b.bits.source  := DontCare
    slv_chn_b.bits.address := probe_addr
    slv_chn_b.bits.mask    := DontCare
    slv_chn_b.bits.data    := DontCare
    slv_chn_b.bits.corrupt := false.B










}






