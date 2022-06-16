
/*
  Copyright (c) 2020 - 2022 Wuhan University of Technology <295054118@whut.edu.cn>

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

package rift2Core.privilege


import chisel3._
import chisel3.util._

import chisel3.util.random._

import rift._
import chipsalliance.rocketchip.config._

class Info_tlb_tag extends Bundle {
  val is_valid = Bool()
  val asid = UInt( 16.W )
  val vpn = Vec( 3, UInt(9.W) )

}


/** 
  * translation lookaside buffer for instance both itlb and dtlb
  * @param entry the entries number of fully associative tlb
  * 
  * 
  */ 
class TLB( entry: Int = 32 )(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{

    val req = Flipped(ValidIO( new Info_mmu_req ))
    val pte_o = Output(new Info_pte_sv39)
    val is_hit = Output(Bool())

    val asid_i = Input(UInt(16.W))

    val tlb_renew = Flipped(ValidIO(new Info_pte_sv39))

    val sfence_vma = Input(Bool())
  })

  /** The tag including *is_valid*, *asid*, and *vpn[8:0]* X 3 */
  val tag = RegInit( VecInit( Seq.fill(entry)(0.U.asTypeOf( new Info_tlb_tag  ))))

  /** The PTE info of page */
  val pte = RegInit( VecInit( Seq.fill(entry)(0.U.asTypeOf( new Info_pte_sv39 ))))

  when( io.sfence_vma ) {
    for( i <- 0 until entry ) yield tag(i) := 0.U.asTypeOf( new Info_tlb_tag )
  }
  .elsewhen(io.tlb_renew.valid) {
    tag(tlb_update_idx).is_valid := true.B
    tag(tlb_update_idx).asid := io.asid_i
    tag(tlb_update_idx).vpn(0) := io.req.bits.vaddr(20,12)
    tag(tlb_update_idx).vpn(1) := io.req.bits.vaddr(29,21)
    tag(tlb_update_idx).vpn(2) := io.req.bits.vaddr(38,30)
    pte(tlb_update_idx) := io.tlb_renew.bits
  }






  /** 
    * tlb replace in random 
    * @return idx one idx to update tlb entry
    * 
    */
  def tlb_update_idx: UInt = {
    val is_runout = tag.forall( (x:Info_tlb_tag) => (x.is_valid === true.B) )

    val random_idx = LFSR( log2Ceil(entry) )
    val empty_idx  = tag.indexWhere( (x:Info_tlb_tag) => (x.is_valid === false.B) )

    return Mux( is_runout, random_idx, empty_idx )
  }



  /** check whether the tlb is hit in
   * @note lvl2_hit
   * @note lvl1_hit
   * @note lvl0_hit 
   */
  val tlb_hit = VecInit(
    for ( i <- 0 until entry ) yield {
      val lvl2 = tag(i).is_valid & io.asid_i === tag(i).asid & io.req.bits.vaddr(38,30) === tag(i).vpn(2)
      val lvl1 = io.req.bits.vaddr(29,21) === tag(i).vpn(1)
      val lvl0 = io.req.bits.vaddr(20,12) === tag(i).vpn(0)

      lvl2 & ( pte(i).is_giga_page | ( lvl1 & (pte(i).is_mega_page | lvl0 ) ) )
    }
  )
  
  assert( PopCount( tlb_hit.asUInt ) <= 1.U, "Assert Fail at tlb, more than 1 entry hit!"  )

  io.pte_o := Mux1H( tlb_hit zip pte )
  io.is_hit := tlb_hit.contains(true.B) & io.req.valid

}




