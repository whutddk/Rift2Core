
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
import chisel3.util.random._


import tilelink._
import axi._
import base._



abstract class TLC_ram ( dw:Int = 1024, bk:Int = 4, cb:Int = 4, cl:Int = 25, mst_size:Int ) extends TLC_fsm {
 
  def addr_lsb = log2Ceil(dw*bk/8)
  def line_w   = log2Ceil(cl)
  def tag_w    = 32 - addr_lsb - line_w

  def mst_lsb = log2Ceil(mst_size/8)

  def bus_lsb = log2Ceil(128/8)

  val cache_dat = new Cache_dat( dw, 32, bk, 1, cl )
  val cache_tag = new Cache_tag( dw, 32, bk, 1, cl )

  val random_res = {
    val increment = ~is_op_aqblk
    LFSR(log2Ceil(16), increment )
  }

  val cache_coherence = RegInit(VecInit(Seq.fill(cl)(VecInit(Seq.fill(cb)(0.U(3.W))))))
  val cache_dirty = RegInit(VecInit(Seq.fill(cl)(VecInit(Seq.fill(cb)(false.B)))))

  val req_addr = RegInit(0.U(64.W))
  val req_cl = req_addr(addr_lsb+line_w-1, addr_lsb)

  val probe_addr = RegInit(0.U(64.W))
  val flash_addr = RegInit(0.U(64.W))
  val release_addr = RegInit(0.U(64.W))
  val grant_addr = RegInit(0.U(64.W))
  val evict_addr = RegInit(0.U(64.W))

  val is_probe_process_end = probe_addr(addr_lsb-1, mst_lsb).andR
  val is_flash_bus_fire: Bool
  // is_flash_bus_fire = mem_mst_r.io.r.fire

  val is_release_bus_fire: Bool
  //release_valid = l2c_slv.io.c.valid
  val release_bus_addr = Wire(UInt(64.W))
  // release_bus_addr = l2c_slv.io.c.bits.address

  val is_release_bus_end = is_release_bus_fire & release_addr(mst_lsb-1, bus_lsb).andR
  val is_grant_bus_fire: Bool
  //is_grant_bus_fire = l2c_slv.io.d.fire
  val is_evict_bus_fire: Bool
  //is_evict_bus_fire = mem_mst_w.io.w.fire
  val is_flash_bus_end = is_flash_bus_fire & flash_addr(addr_lsb-1, bus_lsb).andR
  // is_flash_bus_end = mem_mst_r.io.end
  val is_grant_bus_end = is_grant_bus_fire & grant_addr(mst_lsb-1, bus_lsb).andR
  // is_grant_bus_end = io.l2c_chn_c(req_no).e.fire
  val is_probe_fire: Vec[Bool]
  val is_probe_rtn:Vec[Bool]
  val is_release_with_block: Bool
  val flash_data: UInt
  // flash_data = mem_mst_r.io.r.bits.data
  val release_data: UInt
  // release_data = l2c_slv.io.c.bits.data

  val abandon_addr = Cat(cache_tag.tag_info_r(cb_sel), req_cl, 0.U(addr_lsb))
  val is_evict_bus_end: Bool
    // is_evict_bus_end = mem_mst_w.io.end


  // assert( aqblk_req_addr(mst_lsb-1,0) === 0.U,  "Assert Failed at TLC, aquire request addr misalign!" )
  // assert( wbblk_req_addr(mst_lsb-1,0) === 0.U, "Assert Failed at TLC, release request addr misalign!" )
  // assert( fence_req_addr(addr_lsb-1,0) === 0.U, "Assert Failed at TLC, fence request addr misalign!" )




  val is_cb_hit = {
    val tag_info = req_addr(31, 32-tag_w)
    VecInit(
      for ( i <- 0 until cb ) yield {
        cache_tag.tag_info_r(i) === tag_info
      }
    )
  }




  assert( PopCount(is_cb_hit.asUInt) <= 1.U, "Assert Failed, More than one block hit is not allowed!" )

  val cb_sel = 
    Mux( is_cb_hit.contains(true.B), UIntToOH(is_cb_hit.asUInt),
      Mux( cache_tag.tag_info_r.contains(0.U), cache_tag.tag_info_r.indexWhere((x:UInt) => (x === 0.U)),
        random_res
    ))



  val mem_dat = Mux1H( is_cb_hit zip cache_dat.dat_info_r )




  when(state_qout === cktag & state_dnxt === probe) { probe_addr := req_addr }
  when(state_qout === rlese & state_dnxt === probe) { probe_addr := probe_addr + ( 1.U << mst_lsb ) }

  when(state_qout === cktag & state_dnxt === flash) { flash_addr := req_addr }
  when(state_qout === flash & is_flash_bus_fire) { flash_addr := flash_addr + ( 1.U << bus_lsb ) }

  
  when(state_qout === cktag & state_dnxt === grant) { grant_addr := req_addr }
  when(state_qout === grant & is_grant_bus_fire) { grant_addr := grant_addr + ( 1.U << bus_lsb ) }

  when(state_qout =/= evict & state_dnxt === evict) {
    evict_addr :=
      Mux1H(Seq(
        is_op_aqblk -> abandon_addr,//from cktag
        is_op_fence -> req_addr
      ))
  }
  .elsewhen(state_qout === evict & is_evict_bus_fire) { evict_addr := evict_addr + ( 1.U << bus_lsb ) }


  tlc_state_dnxt_in_cktag := {
    Mux1H(Seq(
      is_op_aqblk -> Mux1H( Seq(
                      ( cache_coherence(req_cl)(cb_sel) === Coher.NONE ) -> flash,
                      ( cache_coherence(req_cl)(cb_sel) === Coher.TTIP ) -> Mux(is_cb_hit.contains(true.B), grant, evict),
                      ( cache_coherence(req_cl)(cb_sel) === Coher.TRNK ) -> probe
                    )),
      is_op_fence -> Mux( ~is_cb_hit.contains(true.B), cfree, 
                      Mux1H(Seq(
                        ( cache_coherence(req_cl)(cb_sel) === Coher.NONE ) -> cfree,
                        ( cache_coherence(req_cl)(cb_sel) === Coher.TTIP ) -> evict,
                        ( cache_coherence(req_cl)(cb_sel) === Coher.TRNK ) -> probe
                      ))
                    )
    ))
  }



  tlc_state_dnxt_in_probe := Mux( is_probe_fire.asUInt.andR, rlese, probe)

  tlc_state_dnxt_in_evict := 
    Mux( ~is_evict_bus_end, evict, 
      Mux1H(Seq(
        is_op_aqblk -> cktag,
        is_op_fence -> cfree
      ))
    )

  tlc_state_dnxt_in_flash := Mux( is_flash_bus_end, cktag, flash )
  tlc_state_dnxt_in_grant := Mux( is_grant_bus_end, cfree, grant )
  tlc_state_dnxt_in_rlese := 
    Mux1H(Seq(
      (is_op_aqblk | is_op_fence) -> Mux( is_probe_rtn.forall( (x:Bool) => (x === true.B) ),
                                      Mux( is_probe_process_end, cktag, probe ),
                                      rlese ),
      is_op_wbblk -> Mux( is_release_bus_end, cfree, rlese ),
    ))

  for ( i <- 0 until cl; j <- 0 until cb ) yield {

    when( is_op_aqblk & i.U === req_cl & j.U === cb_sel ) {
      cache_coherence(i)(j) :=
        Mux1H(Seq(
          ( state_qout === flash & state_dnxt =/= flash ) -> Coher.TTIP,
          ( state_qout === rlese & state_dnxt === cktag ) -> Coher.TTIP,
          ( state_qout === grant & state_dnxt =/= grant ) -> Coher.TRNK,
          ( state_qout === evict & state_dnxt =/= evict ) -> Coher.NONE
        ))
    }

    when( is_op_fence & i.U === req_cl & j.U === cb_sel ) {
      cache_coherence(i)(j) := 
        Mux1H(Seq(
          ( state_qout === rlese & state_dnxt === cktag ) -> Coher.TTIP,
          ( state_qout === evict & state_dnxt =/= evict ) -> Coher.NONE
        ))
    }

    when( i.U === req_cl & j.U === cb_sel ) {
       cache_dirty(i)(j) := 
        Mux1H(Seq(
          (state_qout === rlese & (state_dnxt === cktag | state_dnxt === cfree) & is_release_with_block) -> true.B,
          (state_qout === evict & state_dnxt =/= evict) -> false.B
        ))
    }

  }

  cache_dat.dat_addr_w :=
    Mux1H( Seq(
      (state_qout === flash) -> flash_addr,
      (state_qout === rlese) -> release_addr
    ))

  cache_dat.dat_en_w :=
    ( state_qout === flash & is_flash_bus_fire ) |
    ( state_qout === rlese & is_release_bus_fire & is_release_with_block)

  cache_dat.dat_info_wstrb := Fill(16, 1.U)

  cache_dat.dat_info_w :=
    Mux1H( Seq(
      ( state_qout === flash ) -> flash_data,
      ( state_qout === rlese ) -> release_data,
    ))

  cache_dat.dat_en_r :=
    ( state_qout === evict) |
    ( state_qout === grant)

  cache_dat.dat_addr_r :=
    Mux1H(Seq(
      ( state_qout === evict) -> evict_addr,
      ( state_qout === grant) -> grant_addr
    ))

  cache_tag.tag_addr_r := req_addr

  cache_tag.tag_addr_w := req_addr
      // is_op_fence -> (req_addr & Fill(addr_lsb + line_w, 1.U)) //force tag == 0.U to implit invalid in L2


  cache_tag.tag_en_w := ( state_qout === cktag & state_dnxt === flash)
      


    
  cache_tag.tag_en_r := state_dnxt === cktag

}

