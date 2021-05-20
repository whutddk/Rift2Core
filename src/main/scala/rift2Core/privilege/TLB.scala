
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

package rift2Core.privilege


import chisel3._
import chisel3.util._

import chisel3.util.random._



class Info_tlb_tag extends Bundle {
  val is_valid = Bool()
  val asid = UInt( 16.W )
  val vpn = Vec( 3, UInt(9.W) )
  val is_4K_page = Bool()
  val is_mega_page = Bool()
  val is_giga_page = Bool()

}


/** 
  * translation lookaside buffer for instance both itlb and dtlb
  * @param entry the entries number of fully associative tlb
  * 
  * 
  */ 
class TLB( entry: Int = 32 ) extends Module {
  val io = IO(new Bundle{

    val pte_o  = Output( new Info_pte_sv39 )
    val vaddr  = Flipped(ValidIO( UInt(64.W) ))
    val asid_i = Input(UInt(16.W))

    val tlb_renew = Flipped(ValidIO(new Info_ptw_tlb))
    val is_hit = Output(Bool())

    val flush = Input(Bool())
  })

  val tag = RegInit( VecInit( Seq.fill(entry)(0.U.asTypeOf( new Info_tlb_tag  ))))
  val pte = RegInit( VecInit( Seq.fill(entry)(0.U.asTypeOf( new Info_pte_sv39 ))))

  when( io.flush ) {
    for( i <- 0 until entry ) yield tag(i) := 0.U.asTypeOf( new Info_tlb_tag )
  }
  .elsewhen(io.tlb_renew.valid){
    tag(tlb_update_idx).is_valid := true.B
    tag(tlb_update_idx).asid := io.asid_i
    tag(tlb_update_idx).is_4K_page   := io.tlb_renew.bits.is_4K_page
    tag(tlb_update_idx).is_giga_page := io.tlb_renew.bits.is_giga_page
    tag(tlb_update_idx).is_mega_page := io.tlb_renew.bits.is_mega_page
    tag(tlb_update_idx).vpn(0) := io.vaddr.bits(20,12)
    tag(tlb_update_idx).vpn(1) := io.vaddr.bits(29,21)
    tag(tlb_update_idx).vpn(2) := io.vaddr.bits(38,30)


    pte(tlb_update_idx) := io.tlb_renew.bits.pte
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




  val tlb_hit = Wire(Vec( entry, Bool()))
  for ( i <- 0 until entry ) yield {
    tlb_hit :=
      tag(i).is_valid & io.asid_i === tag(i).asid & io.vaddr(38,30) === tag(i).vpn(2) & (
        tag(i).is_giga_page | (
          io.vaddr(29,21) === tag(i).vpn(1) & (
            tag(i).is_mega_page |
            io.vaddr(20,12) === tag(i).vpn(0)
          )
        )
      )
  }

  assert( PopCount( tlb_hit.asUInt ) <= 1.U, "Assert Fail at tlb, more than 1 entry hit!"  )

  io.pte_o := Mux1H( tlb_hit zip pte )
  io.is_hit := tlb_hit.contains(true.B) & io.vaddr.valid



}




