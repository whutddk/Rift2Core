


/*
  Copyright (c) 2020 - 2021 Ruige Lee <295054118@whut.edu.cn>

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


class TLC_Slv(dw:Int, aw:Int, id:Int) {
  val a = Flipped(new DecoupledIO(new TLchannel_a(dw, aw)))
  val b = new DecoupledIO(new TLchannel_b(dw, aw))
  val c = Flipped(new DecoupledIO(new TLchannel_c(dw, aw)))
  val d = new DecoupledIO( new TLchannel_d(dw))
  val e = Flipped(new DecoupledIO( new TLchannel_e))

  val w = DecoupledIO()
  val r = DecoupledIO()
  val r_ack = Flipped(DecoupledIO())

  val probe = Flipped(DecoupledIO())

  val b_bits = RegInit(0.U.asTypeOf(new TLchannel_b(dw, aw)))
  val d_bits = RegInit(0.U.asTypeOf(new TLchannel_d(dw)))

  val a_ready = RegInit(false.B)
  val b_valid = RegInit(false.B)
  val c_ready = RegInit(false.B)
  val d_valid = RegInit(false.B)
  val e_ready = RegInit(true.B)

  a.ready := a_ready
  b.valid := b_valid; b.bits := b_bits
  c.ready := c_ready
  d.valid := d_valid; d.bits := d_bits
  e.ready := e_ready



  when( e.valid ) {
    // grant ack
    e_ready := true.B
    
  }
  .elsewhen( c.valid ) {
    // release or  probe ack
    when( c.bits.opcode === Opcode.Release | c.bits.opcode === Opcode.ReleaseData ) {

    }
    .elsewhen( c.bits.opcode === Opcode.ProbeAckData | c.bits.opcode === Opcode.ProbeAck ){

    }
  }
  .elsewhen( a.valid ) {
    //grant
  }

}

class TLC_Mst(dw:Int, aw:Int, id:Int) {
  val slv_chn_a = new DecoupledIO(new TLchannel_a(128, 32))
  val slv_chn_b = Flipped(new DecoupledIO(new TLchannel_b(128, 32)))
  val slv_chn_c = new DecoupledIO(new TLchannel_c(128, 32))
  val slv_chn_d = Flipped(new DecoupledIO( new TLchannel_d(128)))
  val slv_chn_e = new DecoupledIO( new TLchannel_e)

  when( l2c_chn_d(i).valid ) {
    // grant or release ack
  }
  .elsewhen( l2c_chn_b(i).valid ) {
    // probe req
  }


} 


class TLC_Xbar(slv:Int, mst: Int) {

}

