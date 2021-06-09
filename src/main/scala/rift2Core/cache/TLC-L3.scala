
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



abstract class TLC_fsm extends Module{

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
  val is_release_bus_valid: Bool
  val is_release_bus_fire: Bool
  //release_valid = l2c_slv.io.c.valid
  val release_bus_addr: UInt
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
  val is_probe_rtn:Vec[Bool]
  val is_L3cache: Boolean
  val is_release_with_block: Bool
  val flash_data: UInt
  // flash_data = mem_mst_r.io.r.bits.data
  val release_data: UInt
  // release_data = l2c_slv.io.c.bits.data

  val abandon_addr = Cat(cache_tag.tag_info_r(cb_sel), req_cl, 0.U(addr_lsb))
  val is_evict_bus_end = 
    Mux(cache_dirty(req_cl)(cb_sel), is_evict_bus_fire & evict_addr(addr_lsb-1, bus_lsb).andR, is_evict_bus_fire)
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

  when( state_qout === rlese ) {
    when( is_release_bus_valid & release_addr === 0.U ) { release_addr := release_bus_addr }
    .elsewhen( is_release_bus_end ) { release_addr := 0.U }
    .elsewhen( is_release_bus_fire ) { release_addr := release_addr + ( 1.U << bus_lsb ) }
  }


  assert( ~(is_release_bus_valid & release_bus_addr(mst_lsb-1,0) =/= 0.U), "Assert Failed at tlc, the address of chn_c is misalign" )

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

  tlc_state_dnxt_in_probe := rlese

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
          ( state_qout === evict & state_dnxt =/= evict ) -> {
                                                              if(is_L3cache) {Coher.TTIP} 
                                                              else {Coher.NONE}
                                                            }
        ))
    }

    when( i.U === req_cl & j.U === cb_sel ) {
       cache_dirty(i)(j) := 
        Mux1H(Seq(
          (state_qout === rlese & (state_dnxt === cktag | state_dnxt === cfree) & is_release_with_block) -> true.B,
          (state_qout === evict & state_dnxt === cktag) -> false.B
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

  cache_tag.tag_addr_w :=
    Mux1H(Seq(
      is_op_aqblk -> req_addr,
      is_op_fence -> (req_addr & Fill(addr_lsb + line_w, 1.U)) //force tag == 0.U to implit invalid in L2
    ))

  cache_tag.tag_en_w := {
    if( is_L3cache ) {
      is_op_aqblk & (
        ( state_qout === cktag & state_dnxt === flash) |
        ( state_qout === evict & state_dnxt === cfree)          
      )
    }
    else {
      ( state_qout === cktag & state_dnxt === flash) |
      ( state_qout === evict & state_dnxt === cfree)          
    }
  }

    
  cache_tag.tag_en_r := state_dnxt === cktag

}


abstract class TLC_slv_port( dw:Int = 1024, bk:Int = 4, cb:Int = 4, cl:Int = 25, mst_num:Int = 3, mst_size:Int )  extends TLC_ram( dw, bk, cb, cl, mst_size ){

  val slv_chn_a = Vec(mst_num, Flipped(new DecoupledIO(new TLchannel_a(128, 32))))
  val slv_chn_b = Vec(mst_num, new DecoupledIO(new TLchannel_b(128, 32)))
  val slv_chn_c = Vec(mst_num, Flipped(new DecoupledIO(new TLchannel_c(128, 32))))
  val slv_chn_d = Vec(mst_num, new DecoupledIO( new TLchannel_d(128)))
  val slv_chn_e = Vec(mst_num, Flipped(new DecoupledIO( new TLchannel_e)))

 val aqblk_agent_no = {
    if ( mst_num == 1 ) {
      0.U(1.W)
    }
    else {
      val value = RegInit(0.U((log2Ceil(mst_num)).W))

      val req = for ( i <- 0 until mst_num ) yield { slv_chn_a(i).valid }
      val no = for ( i <- 0 until mst_num ) yield { i.U }

      when( state_qout === cfree & state_dnxt === cktag ) {
        value := PriorityMux( req zip no )
      }
      value
    }
  }
  
  val wbblk_agent_no = {
    if ( mst_num == 1 ) {
      0.U(1.W)
    }
    else {
      val value = RegInit(0.U((log2Ceil(mst_num)).W))

      val req = for ( i <- 0 until mst_num ) yield { slv_chn_c(i).valid }
      val no = for ( i <- 0 until mst_num ) yield { i.U }

      when( state_qout === rlese & release_addr === 0.U & slv_chn_c.exists(x => x.valid === true.B)  ) {
        value := PriorityMux( req zip no )
      }
      value
    }
  }

  assert( mst_num > 0, "Invalid Setting at TLC-L3, mst number must larger than 0" )

  override val is_aqblk_req = (state_qout === cfree) & slv_chn_a.exists((x:DecoupledIO[TLchannel_a]) => (x.valid === true.B) )
  override val is_wbblk_req = (state_qout === cfree) & slv_chn_c.exists((x:DecoupledIO[TLchannel_c]) => (x.valid === true.B) )

  assert( ~((is_op_aqblk | is_op_wbblk | is_op_fence) & (state_qout === cfree)), "Assert Failed at TLC_slv_port, when cfree, no op can be valid" )




  override val is_release_bus_valid = slv_chn_c.exists((x:DecoupledIO[TLchannel_c]) => (x.valid === true.B) )
  override val is_release_bus_fire = slv_chn_c(wbblk_agent_no).fire

  override val release_bus_addr = slv_chn_c(wbblk_agent_no).bits.address


  override val is_grant_bus_fire = slv_chn_d(aqblk_agent_no).fire



  override val is_probe_rtn = RegInit(VecInit(Seq.fill(mst_num)(false.B)))
  when( state_qout === probe & state_dnxt === rlese ) { is_probe_rtn := 0.U.asBools }
  .elsewhen( state_qout === rlese ) {
      when(
        is_release_bus_end & (
          slv_chn_c(wbblk_agent_no).bits.opcode === Opcode.ProbeAckData |
          slv_chn_c(wbblk_agent_no).bits.opcode === Opcode.ProbeAck
        )
      ) { is_probe_rtn(wbblk_agent_no) := true.B }

  }

  override val is_release_with_block = 
    is_release_bus_fire & (
      slv_chn_c(wbblk_agent_no).bits.opcode === Opcode.ReleaseData |
      slv_chn_c(wbblk_agent_no).bits.opcode === Opcode.ProbeAckData
    )

  override val release_data = slv_chn_c(wbblk_agent_no).bits.data







}


trait TLC_mst {

}

trait AXI_mst {

}




class TLC_L3 ( dw:Int = 1024, bk:Int = 4, cb:Int = 4, cl:Int = 256, mst_num:Int = 3, mst_size:Int = 1024 ) extends TLC_bus( dw, bk, cb, cl, mst_size ) {
  val io = IO(new Bundle{

    val slv_chn_a = Vec(mst_num, Flipped(new DecoupledIO(new TLchannel_a(128, 32))))
    val slv_chn_b = Vec(mst_num, new DecoupledIO(new TLchannel_b(128, 32)))
    val slv_chn_c = Vec(mst_num, Flipped(new DecoupledIO(new TLchannel_c(128, 32))))
    val slv_chn_d = Vec(mst_num, new DecoupledIO( new TLchannel_d(128)))
    val slv_chn_e = Vec(mst_num, Flipped(new DecoupledIO( new TLchannel_e)))
    
    val mem_chn_ar = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    val mem_chn_r = Flipped( new DecoupledIO(new AXI_chn_r( 128, 1, 1)) )

    val mem_chn_aw = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    val mem_chn_w = new DecoupledIO(new AXI_chn_w( 128, 1 )) 
    val mem_chn_b = Flipped( new DecoupledIO(new AXI_chn_b( 1, 1 )))

    val l3c_fence = Flipped(DecoupledIO(Bool()))


  })


  val tl_slv = {
    for ( i <- 0 until mst_num ) yield {
      val mdl = Module( new TileLink_slv_heavy(128, 32) )
      mdl
    }
  }
  
  // val aqblk_agent_no: UInt
  // val wbblk_agent_no: UInt


 

  val mem_mst_r = Module( new AXI_mst_r( 32, 128, 1, 1, 63 ))
  val mem_mst_w = Module( new AXI_mst_w( 32, 128, 1, 1, 63 ))

  override val is_L3cache = true

  override val is_fence_req = ~is_op_aqblk & ~is_op_wbblk & ~is_op_fence & l3c_fence.valid

  override val is_flash_bus_fire = io.mem_chn_r.fire
  override val is_evict_bus_fire = io.mem_chn_w.fire
 

  override val flash_data = io.mem_chn_r.bits.data

}

