/*
  Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

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


object Coher {
  def NONE = 0.U
  def TRNK = 1.U
  def TTIP = 2.U
}



abstract class TLC_fsm extends MultiIOModule{

  val is_fence_req: Bool
  val is_wbblk_req: Bool
  val is_aqblk_req: Bool

  val cfree = 0.U
  val cktag = 1.U
  val flash = 2.U
  val evict = 3.U
  val probe = 4.U
  val grant = 6.U
  val rlese = 7.U
  

  val state_dnxt = Wire( UInt(3.W) )
  val state_qout = RegNext( state_dnxt, cfree )


  val is_op_aqblk = RegInit(false.B)
  val is_op_wbblk = RegInit(false.B)
  val is_op_fence = RegInit(false.B)

  assert(PopCount(Cat(is_op_aqblk, is_op_wbblk, is_op_fence)) <= 1.U, "Assert Failed at TLC, cannot op in 2 stage at a time." )

  when( state_dnxt === cfree ) {
    is_op_aqblk := false.B
    is_op_wbblk := false.B
    is_op_fence := false.B
  }
  .elsewhen( PopCount(Cat(is_op_aqblk, is_op_wbblk, is_op_fence)) === 0.U ) {
    when( is_fence_req ) {
      is_op_fence := true.B
    }
    .elsewhen( is_wbblk_req ) {
      is_op_wbblk := true.B
    }
    .elsewhen( is_aqblk_req ) {
      is_op_aqblk := true.B
    }
  }

  /**
    * @note when fence, goto fence
    * @note when l2c require release, goto release_ack
    * @note when l2c aquire, goto cktag 
    */ 
  val tlc_state_dnxt_in_cfree = 
    Mux1H(Seq(
      is_fence_req -> cktag,
      is_wbblk_req -> rlese,
      is_aqblk_req -> cktag
    ))

  /**
    * @note the only cache line is none? just aquire next level memory
    * @note the only cache line is t or b, no matter hit or no-hit, porbe perivious level cache
    */
    val tlc_state_dnxt_in_cktag = Wire(UInt(3.W))

    /**
      * @note when probe rtn in chn c, exit fsm.probe
      * @note when cache_miss goto fsm.evice, when cache_hit goto fsm.grant
      */
    val tlc_state_dnxt_in_probe = Wire(UInt(3.W))

    /**
      * @note when axi_w rtn, exit evict, if entry from fence, goto fence, or goto cktag 
      */
    val tlc_state_dnxt_in_evict = Wire(UInt(3.W))

    /**
      * @note waitting for axi rtn and goto grant
      */
    val tlc_state_dnxt_in_flash = Wire(UInt(3.W))

    /**
      * @note when grant rtn, goto cfree
      */
    val tlc_state_dnxt_in_grant = Wire(UInt(3.W))
      
    val tlc_state_dnxt_in_rlese = Wire(UInt(3.W))



  state_dnxt :=
    Mux1H( Seq(
      (state_qout === cfree) -> tlc_state_dnxt_in_cfree,
      (state_qout === cktag) -> tlc_state_dnxt_in_cktag,
      (state_qout === flash) -> tlc_state_dnxt_in_flash,
      (state_qout === evict) -> tlc_state_dnxt_in_evict,
      (state_qout === probe) -> tlc_state_dnxt_in_probe,
      (state_qout === grant) -> tlc_state_dnxt_in_grant,
      (state_qout === rlese) -> tlc_state_dnxt_in_rlese
    ))

}

