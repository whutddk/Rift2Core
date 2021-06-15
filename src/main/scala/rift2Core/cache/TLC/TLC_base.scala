
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

package rift2Core.cache.TLC

import chisel3._
import chisel3.util._

import tilelink._
import axi._
import base._
import rift2Core.cache._

// abstract class Cache_setting extends Bundle {
//   val level: Int

//   val dw: Int
//   val aw: Int
//   val bk: Int
//   val cb: Int
//   val cl: Int

//   val addr_lsb = log2Ceil(dw*bk/8)
//   val line_w   = log2Ceil(cl)
//   val tag_w    = 32 - addr_lsb - line_w
//   val bus_w    = 128
//   val bus_lsb  = log2Ceil(bus_w/8)

//   val mst_lsb  = log2Ceil(dw)
// }


class Coher extends Bundle{
  // def NONE = 0.U
  // def TRNK = 1.U
  // def TTIP = 2.U

  val modified = Bool()
  val exclusive = Bool()
  val shared = false.B
  val invalidn = Bool()


}


abstract class TLC_base extends MultiIOModule {

  val dw: Int
  val aw: Int
  val bk: Int
  val cb: Int
  val cl: Int


  val addr_lsb = log2Ceil(dw*bk/8)
  val line_w   = log2Ceil(cl)
  val tag_w    = 32 - addr_lsb - line_w
  val bus_w    = 128
  val bus_lsb  = log2Ceil(bus_w/8)

  val mst_lsb  = log2Ceil(dw/8)




  // val slv_chn_a = IO(Flipped(new DecoupledIO(new TLchannel_a(128, 32))))
  // val slv_chn_b = IO(new DecoupledIO(new TLchannel_b(128, 32)))
  // val slv_chn_c0 = IO(Flipped(new DecoupledIO(new TLchannel_c(128, 32))))
  // val slv_chn_d0 = IO(new DecoupledIO( new TLchannel_d(128)))
  // val slv_chn_e = IO(Flipped(new DecoupledIO( new TLchannel_e)))
  // val slv_chn_c1 = IO(Flipped(new DecoupledIO(new TLchannel_c(128, 32))))
  // val slv_chn_d1 = IO(new DecoupledIO( new TLchannel_d(128)))


  // val mst_chn_a = IO(new DecoupledIO(new TLchannel_a(128, 32)))
  // val mst_chn_b = IO(Flipped(new DecoupledIO(new TLchannel_b(128, 32))))
  // val mst_chn_c = IO(new DecoupledIO(new TLchannel_c(128, 32)))
  // val mst_chn_d = IO(Flipped(new DecoupledIO( new TLchannel_d(128))))
  // val mst_chn_e = IO(new DecoupledIO( new TLchannel_e))



//产生寄生消息由Waiting
//发生总线请求用valid
//操作占用用stateOn

  val is_slvAcquire_valid = Wire(Bool())
  val is_slvAcquire_StateOn = RegInit(false.B)
  val is_slvAcquire_allowen = Wire(Bool())

  val is_slvGrantData_Waiting = RegInit(false.B)
  val is_slvGrantData_StateOn = RegInit(false.B)
  val is_slvGrantData_allowen = Wire(Bool())

  val is_slvGrantAck_valid = Wire(Bool())
  val is_slvGrantAck_StateOn = RegInit(false.B)
  val is_slvGrantAck_allowen = Wire(Bool())

  val is_slvProbe_Waiting = RegInit(false.B)
  val is_slvProbe_StateOn = RegInit(false.B)
  val is_slvProbe_allowen = Wire(Bool())

  val is_slvProbeAck_valid = Wire(Bool())
  val is_slvProbeAck_StateOn = RegInit(false.B)
  val is_slvProbeAck_allowen = Wire(Bool())

  val is_slvProbeData_valid = Wire(Bool())
  val is_slvProbeData_StateOn = RegInit(false.B)
  val is_slvProbeData_allowen = Wire(Bool())

  val is_slvReleaseData_valid = Wire(Bool())
  val is_slvReleaseData_StateOn = RegInit(false.B)
  val is_slvReleaseData_allowen = Wire(Bool())

  val is_slvReleaseAck_Waiting = RegInit(false.B)
  val is_slvReleaseAck_StateOn = RegInit(false.B)
  val is_slvReleaseAck_allowen = Wire(Bool())



  val is_mstAcquire_Waiting = RegInit(false.B)
  val is_mstAcquire_StateOn = RegInit(false.B)
  val is_mstAcquire_allowen = Wire(Bool())

  val is_mstGrantData_valid = Wire(Bool())
  val is_mstGrantData_StateOn = RegInit(false.B)
  val is_mstGrantData_allowen = Wire(Bool())

  val is_mstGrantAck_Waiting = RegInit(false.B)
  val is_mstGrantAck_StateOn = RegInit(false.B)
  val is_mstGrantAck_allowen = Wire(Bool())

  val is_mstProbe_valid = Wire(Bool())
  val is_mstProbe_StateOn = RegInit(false.B)
  val is_mstProbe_allowen = Wire(Bool())

  val is_mstProbeAck_Waiting = RegInit(false.B)
  val is_mstProbeAck_StateOn = RegInit(false.B)
  val is_mstProbeAck_allowen = Wire(Bool())

  val is_mstProbeData_Waiting = RegInit(false.B)
  val is_mstProbeData_StateOn = RegInit(false.B)
  val is_mstProbeData_allowen = Wire(Bool())

  val is_mstReleaseData_Waiting = RegInit(false.B)
  val is_mstReleaseData_StateOn = RegInit(false.B)
  val is_mstReleaseData_allowen = Wire(Bool())

  val is_mstReleaseAck_valid = Wire(Bool())
  val is_mstReleaseAck_StateOn = RegInit(false.B)
  val is_mstReleaseAck_allowen = Wire(Bool())


  val cache_dat = new Cache_dat( dw, aw, bk, cb, cl )
  val cache_tag = new Cache_tag( dw, aw, bk, cb, cl )
  val cache_coh = new Cache_coh( dw, aw, bk, cb, cl )



  val info_slvAcquire_cb = Wire( UInt(log2Ceil(cb).W) )
  val info_slvAcquire_address = Wire( UInt(64.W) )
  val info_slvAcquire_source  = Wire( UInt(8.W) )

  val info_mstProbe_address = Wire( UInt(64.W) )
}


