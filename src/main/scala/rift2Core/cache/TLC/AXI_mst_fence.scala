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

package rift2Core.cache.TLC

import chisel3._
import chisel3.util._

import axi._

import base._
import rift2Core.cache._


trait AXI_mst_fence extends TLC_base {
  val mst_chn_aw1 = new DecoupledIO(new AXI_chn_a( 64, 1, 1 ))
  val mst_chn_w1  = new DecoupledIO(new AXI_chn_w( 128, 1 )) 
  val mst_chn_b1 = Flipped(new DecoupledIO( new AXI_chn_b( 1, 1 )))

  val fence = IO( new DecoupledIO(Bool()) )

  // assert( cb == 1, "Assert Failed at AXI_fence, invalid configuration of cb" )

  // val cache_inv = RegInit( VecInit( Seq.fill(cl)( VecInit(Seq.fill(cb)(false.B)))))))
  // val cache_mdf = RegInit( VecInit( Seq.fill(cl)( VecInit(Seq.fill(cb)( VecInit( Seq.fill(bk)(false.B)))))))

  val is_mstfence_cache_inv = cache_inv.exists( (x:Vec[Bool]) => x.contains(false.B) )
  val info_mstFence_cl = cache_inv.indexWhere( (x:Vec[Bool]) => x.contains(false.B) )
  val info_mstFence_cb = cache_inv(info_mstFence_cl).indexWhere((x:Bool) => (x === false.B))
  val info_mstFence_bk = cache_coh.coh_info_r.indexWhere( (x:UInt) => (x =/= 0.U) )

  info_mstProbe_address := 
    RegEnable(
      Cat( cache_tag.tag_info_r(cb), info_mstFence_cl, info_mstFence_bk, 0.U(mst_lsb.W) ),
      mstFence_state_qout === 1.U
    )

    

  val mstFence_state_dnxt = Wire( UInt(3.W) )
  val mstFence_state_qout = RegNext( mstFence_state_dnxt, 0.U )

  mstFence_state_dnxt := 
    Mux1H(Seq(
      (mstFence_state_qout === 0.U) -> Mux( fence.valid & is_mstfence_cache_inv, 1.U, 0.U ), //read coh to get source
      (mstFence_state_qout === 1.U) ->
        Mux( cache_coh.coh_info_r.exists( (x:UInt) => (x =/= 0.U) ), 1.U,
          Mux( cache_mdf(info_mstFence_cl)(info_mstFence_cb)(info_mstFence_bk) === true.B, 2.U, 0.U )
        ), //check all bank to slvprobe, //check if mstRelease is needed
      (mstFence_state_qout === 2.U) -> Mux( mst_chn_b1.fire, 0.U, 2.U ) //mstpobe data
    ))


  when( mstFence_state_qout === 0.U & mstFence_state_dnxt === 1.U ) {

  }

  val is_mstProbe_valid = Wire(Bool())
  val is_mstProbe_StateOn = RegInit(false.B)
  val is_mstProbe_allowen = Wire(Bool())

  val is_mstProbeAckData_Waiting = RegInit(false.B)
  val is_mstProbeAckData_StateOn = RegInit(false.B)
  val is_mstProbeAckData_allowen = Wire(Bool())


  val info_mstProbe_cl = Wire( UInt(log2Ceil(cl).W) )
  val info_mstProbe_cb = Wire( UInt(log2Ceil(cb).W) )

  for ( i <- 0 until cb ) yield {
    info_mstProbe_cache_tag_ren := 
      i.U === info_mstFence_cb &
      mstFence_state_qout === 0.U & mstFence_state_dnxt === 1.U
  }
  info_mstProbe_cache_tag_raddr := 
    Cat( info_mstFence_cl, 0.U(addr_lsb.U)  )

  for ( i <- 0 until cb ) yield {
    info_mstProbeData_cache_coh_ren(i) := 
      mstFence_state_qout === 0.U & mstFence_state_dnxt === 1.U
  }
  info_mstProbeData_cache_coh_raddr :=
    Cat( info_mstFence_cl, 0.U(addr_lsb.U)  )

  val info_mstProbeData_cache_dat_ren   = Wire(Vec(cb, Bool()))
  val info_mstProbeData_cache_dat_raddr = Wire(UInt(64.W))


  val info_mstProbeData_address = RegInit( 0.U(64.W) )

  val info_mstRecProbe_address = RegInit( 0.U(64.W) )
  val info_mstRecProbe_exclusive = RegInit( 0.U(8.W) )
  val info_mstRecProbe_cb = RegInit( 0.U(log2Ceil(cb).W) )


}


