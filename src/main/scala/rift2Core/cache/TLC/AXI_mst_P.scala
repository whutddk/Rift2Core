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


trait AXI_mst_P extends TLC_base {

  val fence = IO( new DecoupledIO(Bool()) )

  

  is_mstProbe_allowen := 
    ~is_slvAcquire_StateOn       &
    ~is_slvGrantData_StateOn     &
    ~is_slvGrantAck_StateOn      &
    ~is_slvProbe_StateOn         &
    ~is_slvProbeAckData_StateOn  &
    ~is_slvReleaseData_StateOn   &
    ~is_slvReleaseAck_StateOn    &
    ~is_mstAcquire_StateOn       &
    ~is_mstGrantData_StateOn     &
    ~is_mstGrantAck_StateOn      &
    ~is_mstProbe_StateOn         &
    ~is_mstProbeAckData_StateOn  &
    ~is_mstReleaseData_StateOn   &
    ~is_mstReleaseAck_StateOn    &
    ~is_slvProbe_Waiting         &
    is_mstProbe_valid 


  fence.ready := fence.valid & ~is_mstfence_cache_inv

  val is_mstFence_stateOn = RegInit(false.B)
  val is_mstfence_cache_inv = cache_inv.exists( (x:Vec[Bool]) => x.contains(false.B) )
  val info_mstProbe_bk = cache_coh.coh_info_r.indexWhere( (x:UInt) => (x =/= 0.U) )

  val mstProbe_state_dnxt = Wire( UInt(3.W) )
  val mstProbe_state_qout = RegNext( mstProbe_state_dnxt, 0.U )

  mstProbe_state_dnxt := 
    Mux1H(Seq(
      (mstProbe_state_qout === 0.U) -> Mux( is_mstProbe_allowen & is_mstfence_cache_inv, 1.U, 0.U ), //read coh to get source
      (mstProbe_state_qout === 1.U) -> Mux( cache_coh.coh_info_r.exists( (x:UInt) => (x =/= 0.U) ), 0.U, 2.U ),
      (mstProbe_state_qout === 2.U) -> 0.U
    ))


  info_mstProbe_cl := 
    RegEnable(
      cache_inv.indexWhere( (x:Vec[Bool]) => x.contains(false.B) ),
      mstProbe_state_qout === 0.U & mstProbe_state_dnxt === 1.U
    )
    
  info_mstProbe_cb := {
    val cl_sel = cache_inv.indexWhere( (x:Vec[Bool]) => x.contains(false.B) )
    RegEnable(
      cache_inv(cl_sel).indexWhere((x:Bool) => (x === false.B)),
      mstProbe_state_qout === 0.U & mstProbe_state_dnxt === 1.U
    )    
  }

  info_mstProbe_address := 
    RegEnable(
      Cat( cache_tag.tag_info_r(cb), info_mstProbe_cl, 0.U(addr_lsb.W) ),
      mstProbe_state_qout === 1.U
    )

     
  for ( i <- 0 until cb ) yield { info_mstProbe_cache_tag_ren := i.U === info_mstProbe_cb & mstProbe_state_qout === 0.U & mstProbe_state_dnxt === 1.U }
  info_mstProbe_cache_tag_raddr := Cat( info_mstProbe_cl, 0.U(addr_lsb.U)  )

  for ( i <- 0 until cb ) yield { info_mstProbeData_cache_coh_ren(i) := mstProbe_state_qout === 0.U & mstProbe_state_dnxt === 1.U }
  info_mstProbeData_cache_coh_raddr := Cat( info_mstProbe_cl, 0.U(addr_lsb.U)  )

  when( mstProbe_state_qout === 1.U & mstProbe_state_dnxt === 0.U ) {
    is_slvProbe_Waiting := true.B
    info_mstRecProbe_address := Cat( cache_tag.tag_info_r(cb), info_mstProbe_cl, info_mstProbe_bk, 0.U(mst_lsb.W) )
    info_mstRecProbe_exclusive := cache_coh.coh_info_r(info_mstProbe_bk)
    info_mstRecProbe_cb := info_mstProbe_cb
  }


  when( mstProbe_state_qout === 2.U ) {
    cache_inv(info_mstProbe_cl)(info_mstProbe_cb) := 
      Mux(
        cache_mdf(info_mstProbe_cl)(info_mstProbe_cb).forall( (x:Bool) => (x === false.B)),
        true.B,
        cache_inv(info_mstProbe_cl)(info_mstProbe_cb)
      )
  }

  when( mstProbe_state_qout === 0.U & mstProbe_state_dnxt === 1.U ) { is_mstFence_stateOn := true.B }
  .elsewhen( fence.fire )  { is_mstFence_stateOn := false.B }

  when( mstProbe_state_dnxt =/= 0.U  ) {is_mstProbe_StateOn := true.B}
  .otherwise {is_mstProbe_StateOn := false.B}
  
}


