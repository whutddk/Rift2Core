
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




trait Lx_param {
  def dw: Int
  def aw: Int
  def bk: Int
  def cb: Int
  def cl: Int
  def agent_no: Int 

}


abstract class TLC_base extends MultiIOModule with Lx_param {



  val addr_lsb = log2Ceil(dw*bk/8)
  val line_w   = log2Ceil(cl)
  val tag_w    = 32 - addr_lsb - line_w
  val bus_w    = 128
  val bus_lsb  = log2Ceil(bus_w/8)

  val mst_lsb  = log2Ceil(dw/8)







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

  val is_slvProbeAckData_valid = Wire(Bool())
  val is_slvProbeAckData_StateOn = RegInit(false.B)
  val is_slvProbeAckData_allowen = Wire(Bool())

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

  val is_mstProbeAckData_Waiting = RegInit(false.B)
  val is_mstProbeAckData_StateOn = RegInit(false.B)
  val is_mstProbeAckData_allowen = Wire(Bool())

  val is_mstReleaseData_Waiting = RegInit(false.B)
  val is_mstReleaseData_StateOn = RegInit(false.B)
  val is_mstReleaseData_allowen = Wire(Bool())

  val is_mstReleaseAck_valid = Wire(Bool())
  val is_mstReleaseAck_StateOn = RegInit(false.B)
  val is_mstReleaseAck_allowen = Wire(Bool())


  val cache_dat = new Cache_dat( dw, aw, bk, cb, cl )
  val cache_tag = new Cache_tag( dw, aw, bk, cb, cl )
  val cache_coh = new Cache_coh( dw, aw, bk, cb, cl )
  val cache_inv = RegInit( VecInit( Seq.fill(cl)( VecInit(Seq.fill(cb)(false.B)))))
  val cache_mdf = RegInit( VecInit( Seq.fill(cl)( VecInit(Seq.fill(cb)( VecInit( Seq.fill(bk)(false.B)))))))



  val info_slvAcquire_cl = Wire( UInt(log2Ceil(cl).W) )
  val info_slvAcquire_cb = Wire( UInt(log2Ceil(cb).W) )
  val info_slvAcquire_bk = Wire( UInt(log2Ceil(bk).W) )

  val info_slvAcquire_address = Wire( UInt(64.W) )
  val info_slvAcquire_source  = Wire( UInt(8.W) )

  val info_slvAcquire_cache_tag_ren   = Wire(Vec(cb, Bool()))
  val info_slvAcquire_cache_tag_raddr = Wire(UInt(64.W))

  val info_slvGrantData_exclusive = RegInit( 0.U(8.W) )
  val info_slvGrantData_address   = RegInit( 0.U(64.W) )

  val info_slvGrantData_cache_tag_ren   = Wire(Vec(cb, Bool()))
  val info_slvGrantData_cache_tag_raddr = Wire(UInt(64.W))
  val info_slvGrantData_cache_coh_ren   = Wire(Vec(cb, Bool()))
  val info_slvGrantData_cache_coh_raddr = Wire(UInt(64.W))
  val info_slvGrantData_cache_dat_ren   = Wire(Vec(cb, Bool()))
  val info_slvGrantData_cache_dat_raddr = Wire(UInt(64.W))

  val info_slvGrantData_cache_coh_wen   = Wire(Vec(cb, Vec(bk, Bool())))
  val info_slvGrantData_cache_coh_waddr = Wire(UInt(64.W))
  val info_slvGrantData_cache_coh_winfo = Wire(UInt(8.W))

  val info_slvProbe_address = Wire( UInt(64.W) )
  val info_slvProbe_cl = Wire( UInt(log2Ceil(cl).W) )
  val info_slvProbe_cb = Wire( UInt(log2Ceil(cb).W) )
  val info_slvProbe_bk = Wire( UInt(log2Ceil(bk).W) )
  val info_slvProbe_exclusive = Wire( UInt(8.W) )


  val info_slvProbeAck_Data_cache_coh_wen   = Wire(Vec(cb, Vec(bk, Bool())))
  val info_slvProbeAck_Data_cache_coh_waddr = Wire(UInt(64.W))
  val info_slvProbeAck_Data_cache_coh_winfo = Wire(UInt(8.W))

  val info_slvProbeAck_Data_cache_dat_wen   = Wire(Vec(cb, Bool()))
  val info_slvProbeAck_Data_cache_dat_waddr = Wire(UInt(64.W))
  val info_slvProbeAck_Data_cache_dat_wstrb = Wire(UInt(8.W))
  val info_slvProbeAck_Data_cache_dat_winfo = Wire(UInt(128.W))

  val info_slvReleaseData_cl = Wire( UInt(log2Ceil(cl).W) )
  val info_slvReleaseData_cb = Wire( UInt(log2Ceil(cb).W) )
  val info_slvReleaseData_bk = Wire( UInt(log2Ceil(bk).W) )
  val info_slvReleaseData_address = RegInit(0.U(64.W))

  val info_slvReleaseData_cache_coh_wen   = Wire(Vec(cb, Vec(bk, Bool())))
  val info_slvReleaseData_cache_coh_waddr = Wire(UInt(64.W))
  val info_slvReleaseData_cache_coh_winfo = Wire(UInt(8.W))

  val info_slvReleaseData_cache_dat_wen   = Wire(Vec(cb, Bool()))
  val info_slvReleaseData_cache_dat_waddr = Wire(UInt(64.W))
  val info_slvReleaseData_cache_dat_wstrb = Wire(UInt(8.W))
  val info_slvReleaseData_cache_dat_winfo = Wire(UInt(128.W))

  val info_slvReleaseData_source  = Wire( UInt(8.W) )





  // val info_mstAcquire_address = Wire( UInt(64.W) )
  val info_mstGrantData_address = RegInit( 0.U(64.W) )


  val info_mstGrantData_cache_coh_wen   = Wire(Vec(cb, Vec(bk, Bool())))
  val info_mstGrantData_cache_coh_waddr = Wire(UInt(64.W))
  val info_mstGrantData_cache_coh_winfo = Wire(UInt(8.W))

  val info_mstGrantData_cache_dat_wen   = Wire(Vec(cb, Bool()))
  val info_mstGrantData_cache_dat_waddr = Wire(UInt(64.W))
  val info_mstGrantData_cache_dat_wstrb = Wire(UInt(8.W))
  val info_mstGrantData_cache_dat_winfo = Wire(UInt(128.W))

  val info_mstGrantData_cache_tag_wen   = Wire(Vec(cb, Bool()))
  val info_mstGrantData_cache_tag_waddr = Wire(UInt(64.W))

  val info_mstProbe_address = Wire( UInt(64.W) )
  val info_mstProbe_cl = Wire( UInt(log2Ceil(cl).W) )
  val info_mstProbe_cb = Wire( UInt(log2Ceil(cb).W) )

  val info_mstProbe_cache_tag_ren   = Wire(Vec(cb, Bool()))
  val info_mstProbe_cache_tag_raddr = Wire(UInt(64.W))

  val info_mstProbeData_cache_coh_ren   = Wire(Vec(cb, Bool()))
  val info_mstProbeData_cache_coh_raddr = Wire(UInt(64.W))
  val info_mstProbeData_cache_dat_ren   = Wire(Vec(cb, Bool()))
  val info_mstProbeData_cache_dat_raddr = Wire(UInt(64.W))


  val info_mstProbeData_address = RegInit( 0.U(64.W) )

  val info_mstRecProbe_address = RegInit( 0.U(64.W) )
  val info_mstRecProbe_exclusive = RegInit( 0.U(8.W) )
  val info_mstRecProbe_cb = RegInit( 0.U(log2Ceil(cb).W) )

  val info_mstReleaseData_address = RegInit( 0.U(64.W) )

  val info_mstReleaseData_cache_dat_ren   = Wire(Vec(cb, Bool()))
  val info_mstReleaseData_cache_dat_raddr = Wire(UInt(64.W))


}


