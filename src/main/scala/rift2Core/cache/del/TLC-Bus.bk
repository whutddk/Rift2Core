


// /*
//   Copyright (c) 2020 - 2021 Ruige Lee <295054118@whut.edu.cn>

//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at

//      http://www.apache.org/licenses/LICENSE-2.0

//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
// */

// package rift2Core.cache



// import chisel3._
// import chisel3.util._

// class ram_w_port(dw:Int, aw:Int) extends Bundle {
//   val data = UInt(dw.W)
//   val strb = UInt((dw/8).W)
//   val addr = UInt(aw.W)
// }

// class ram_probe(aw:Int) extends Bundle {
//   val addr = UInt(aw.W)
//   val contex = UInt(2.W)
// }


// /**
//   * define the ram access port for aquire
//   *
//   */
// class Info_context_cache extends Bundle {
//   val context = UInt(2.W)
//   val addr    = UInt(aw.W)
// }

// class Info_rd_cache extends Bundle {
//   val data = UInt(dw.W)
// }

// class Info_wr_cache extends Bundle {
//   val addr = UInt(aw.W)
//   val data = UInt(dw.W)
//   val strb = UInt((dw/8).W)
// }

// class Acquire_Slv(dw:Int, aw:Int, id:Int) {
//   val a = Flipped(new DecoupledIO(new TLchannel_a(dw, aw)))
//   val d = new DecoupledIO( new TLchannel_d(dw))
//   val e = Flipped(new DecoupledIO( new TLchannel_e))

//   val toCache = DecoupledIO( new Info_context_cache )
//   val fromCache = Flipped(DecoupledIO( new Info_rd_cache ))

//   toCache.valid := a.valid
//   toCache.bits.addr := a.bits.address
//   toCache.bits.context := 0.U
//   a.ready := toCache.ready

//   d.valid := fromCache.valid
//   d.bits.data := fromCache.bits.data
//   fromCache.ready := d.ready

//   e.ready := true.B

//   assert( ~(a.valid === true.B & a.bits.opcode =/= Opcode.AcquireBlock), "Assert Failed at acquire, only support acquire Block in this version" )
//   assert( ~(a.valid === true.B & a.bits.param =/= 0.U), "Assert Failed at acquire, only support acquire Block NtoT in this version" )

// }


// class Release_Slv(dw:Int, aw:Int, id:Int) {
//   val c = Flipped(new DecoupledIO(new TLchannel_c(dw, aw)))
//   val d = new DecoupledIO( new TLchannel_d(dw))

//   val toCache = DecoupledIO( new Info_wr_cache )

//   val c_addr_dnxt = Wire(UInt(aw.W))
//   val c_addr_en   = Wire(Bool())
//   val c_addr_qout = RegEnable(c_addr_dnxt, c_addr_en)

//   toCache.valid := c.valid
//   toCache.bits.addr := c.bits.address
//   toCache.bits.data := c.bits.data
//   toCache.bits.strb := c.bits.strb

// }


// /**
//   * @note just boardcast the probe, wait for release
//   *
//   */
// class Probe_Slv(dw:Int, aw:Int, id:Int) {
//   val b = new DecoupledIO(new TLchannel_b(dw, aw))

//   val fromCache = Flipped(DecoupledIO( new Info_context_cache))

// }










// class TLC_Slv(dw:Int, aw:Int, id:Int) {
//   val a = Flipped(new DecoupledIO(new TLchannel_a(dw, aw)))
//   val b = new DecoupledIO(new TLchannel_b(dw, aw))
//   val c = Flipped(new DecoupledIO(new TLchannel_c(dw, aw)))
//   val d = new DecoupledIO( new TLchannel_d(dw))
//   val e = Flipped(new DecoupledIO( new TLchannel_e))

//   val w = DecoupledIO(new ram_w_port(dw, aw))
//   val r_req = DecoupledIO(UInt(aw.W))
//   val r_rsp = Flipped(DecoupledIO(UInt(dw.W)))   

//   val g = DecoupledIO(new ram_probe(aw))
//   val p = Flipped(DecoupledIO(new ram_probe(aw)))

//   val b_bits = RegInit(0.U.asTypeOf(new TLchannel_b(dw, aw)))
//   val d_bits = RegInit(0.U.asTypeOf(new TLchannel_d(dw)))

//   val a_ready = RegInit(false.B)
//   val b_valid = RegInit(false.B)
//   val c_ready = RegInit(false.B)
//   val d_valid = RegInit(false.B)
//   val e_ready = RegInit(true.B)

//   a.ready := a_ready
//   b.valid := b_valid; b.bits := b_bits
//   c.ready := c_ready
//   d.valid := d_valid; d.bits := d_bits
//   e.ready := e_ready


//   val acquire = new Bundle {
//     val is_valid = RegInit(false.B)
//     val addr = RegInit(false.B)
//     val size = RegInit(0.U(8.W))
//     val context = RegInit(0.U(3.W))
//     val is_block = RegInit(false.B)

//     assert(addr(,0) === 0.U, "Assert Fail!!! acquire address is misalign.")
//   }

//   val release = new Bundle {
//     val is_valid = RegInit(false.B)
//   }

//   val grant = new Bundle {

//   }

//   val probe = new Bundle {

//   }


//   when( e.valid ) {
//     // grant ack
//     e_ready := true.B
//     acquire.is_valid := false.B
    
//   }
//   .elsewhen( c.valid ) {
//     // release or  probe ack
//     when( c.bits.opcode === Opcode.Release | c.bits.opcode === Opcode.ProbeAck ) {
//       c_ready := true.B
//     }
//     .elsewhen( c.bits.opcode === Opcode.ReleaseData | c.bits.opcode === Opcode.ProbeAckData ) {
//       when( w.fire ) {
//         c_ready := true.B
//       }
//     }
//     .elsewhen( c.fire ) {
//       c_ready := false.B
//     }

//   }
//   .elsewhen( a.valid ) {
//     //acquire
//     when( acquire.is_valid === false.B ) {
//       a_ready := true.B
//       acquire.is_valid := true.B
//       acquire.addr := a.bits.address
//       acquire.size := a.bits.size
//       acquire.context := a.bits.param
//       acquire.is_block := (a.bits.opcode === 6.U)
//     }
//     .elsewhen( a.fire ) {
//       a_ready := false.B
//     }
//   }







// }






// class TLC_Mst(dw:Int, aw:Int, id:Int) {
//   val slv_chn_a = new DecoupledIO(new TLchannel_a(128, 32))
//   val slv_chn_b = Flipped(new DecoupledIO(new TLchannel_b(128, 32)))
//   val slv_chn_c = new DecoupledIO(new TLchannel_c(128, 32))
//   val slv_chn_d = Flipped(new DecoupledIO( new TLchannel_d(128)))
//   val slv_chn_e = new DecoupledIO( new TLchannel_e)

//   when( l2c_chn_d(i).valid ) {
//     // grant or release ack
//   }
//   .elsewhen( l2c_chn_b(i).valid ) {
//     // probe req
//   }


// } 


// class TLC_Xbar(slv:Int, mst: Int) {

// }

