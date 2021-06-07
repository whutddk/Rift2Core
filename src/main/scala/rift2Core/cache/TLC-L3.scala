
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
import chisel3.experimental.ChiselEnum

import tilelink._
import axi._
import base._



class TLC_L3 ( dw:Int = 1024, bk:Int = 4, cl:Int = 256 ) extends Module {
  val io = IO(new Bundle{

    val l2c_chn_a = Vec(1, Flipped(new DecoupledIO(new TLchannel_a(128, 32))))
    val l2c_chn_b = Vec(1, new DecoupledIO(new TLchannel_b(128, 32)))
    val l2c_chn_c = Vec(1, Flipped(new DecoupledIO(new TLchannel_c(128, 32))))
    val l2c_chn_d = Vec(1, new DecoupledIO( new TLchannel_d(128)))
    val l2c_chn_e = Vec(1, Flipped(new DecoupledIO( new TLchannel_e)))
    
    val mem_chn_ar = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    val mem_chn_r = Flipped( new DecoupledIO(new AXI_chn_r( 128, 1, 1)) )

    val mem_chn_aw = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    val mem_chn_w = new DecoupledIO(new AXI_chn_w( 128, 1 )) 
    val mem_chn_b = Flipped( new DecoupledIO(new AXI_chn_b( 1, 1 )))

    val l3c_fence_req = Input(Bool())
  })

  def addr_lsb = log2Ceil(dw*bk/8)
  def line_w   = log2Ceil(cl)
  def tag_w    = 32 - addr_lsb - line_w
  def cb = 1

  val l2c_slv   = Module( new TileLink_slv_heavy(128, 32) )
  val mem_mst_r = Module( new AXI_mst_r( 32, 128, 1, 1, 63 ))
  val mem_mst_w = Module( new AXI_mst_w( 32, 128, 1, 1, 63 ))
  val cache_dat = new Cache_dat( dw, 32, bk, 1, cl )
  val cache_tag = new Cache_tag( dw, 32, bk, 1, cl )


object Coher {
  def NONE = 0.U
  def TRNK = 1.U
  def TTIP = 2.U
}

object Oper {
  val is_aqblk = RegInit(false.B)
  val is_wbblk = RegInit(false.B)
  val is_fence = RegInit(false.B)

    assert(PopCount(Cat(is_op_aqblk, is_op_wbblk, is_op_fence) <= 1.U, "Assert Failed at TLC, cannot op in 2 stage at a time." ))
}



  is_release_req
  is_release_end
  is_probe_rtn
  req_no

  val fsm = new Bundle {

    val cfree = 0.U
    val cktag = 1.U
    val flash = 2.U
    val evict = 3.U
    val probe = 4.U
    val fence = 5.U
    val grant = 6.U
    val rlese = 7.U
  

    val state_dnxt = Wire( UInt(3.W) )
    val state_qout = RegNext( state_dnxt, cfree )
  }

  val bram = new Bundle {

    val cache_coherence = RegInit(VecInit(Seq.fill(cl)(VecInit(Seq.fill(cb)(0.U(3.W))))))
    val cache_dirty = RegInit(VecInit(Seq.fill(cl)(VecInit(Seq.fill(cb)(false.B)))))

    val mem_dat = Wire( UInt(128.W) )
    val tag_addr = Wire( UInt(32.W) )
    val tag_info = tag_addr(31, 32-tag_w)

    val is_cb_hit = Wire(Vec(cb, Bool()))
    def cl_sel(addr: UInt): UInt = addr(addr_lsb+line_w-1, addr_lsb)

  }



// FFFFFFFFFFFFFFFFFFFFFF   SSSSSSSSSSSSSSS MMMMMMMM               MMMMMMMM
// F::::::::::::::::::::F SS:::::::::::::::SM:::::::M             M:::::::M
// F::::::::::::::::::::FS:::::SSSSSS::::::SM::::::::M           M::::::::M
// FF::::::FFFFFFFFF::::FS:::::S     SSSSSSSM:::::::::M         M:::::::::M
//   F:::::F       FFFFFFS:::::S            M::::::::::M       M::::::::::M
//   F:::::F             S:::::S            M:::::::::::M     M:::::::::::M
//   F::::::FFFFFFFFFF    S::::SSSS         M:::::::M::::M   M::::M:::::::M
//   F:::::::::::::::F     SS::::::SSSSS    M::::::M M::::M M::::M M::::::M
//   F:::::::::::::::F       SSS::::::::SS  M::::::M  M::::M::::M  M::::::M
//   F::::::FFFFFFFFFF          SSSSSS::::S M::::::M   M:::::::M   M::::::M
//   F:::::F                         S:::::SM::::::M    M:::::M    M::::::M
//   F:::::F                         S:::::SM::::::M     MMMMM     M::::::M
// FF:::::::FF           SSSSSSS     S:::::SM::::::M               M::::::M
// F::::::::FF           S::::::SSSSSS:::::SM::::::M               M::::::M
// F::::::::FF           S:::::::::::::::SS M::::::M               M::::::M
// FFFFFFFFFFF            SSSSSSSSSSSSSSS   MMMMMMMM               MMMMMMMM




  fsm.state_dnxt := {
 
    /**
      * @note when fence, goto fence
      * @note when l2c require release, goto release_ack
      * @note when l2c aquire, goto cktag 
      */ 
    val l3c_state_dnxt_in_cfree = 
      Mux1H(Seq(
        is_op_fence -> fsm.fence,
        is_op_wbblk -> fsm.rlese,
        is_op_aqblk -> fsm.cktag
      ))


    /**
      * @note the only cache line is none? just aquire next level memory
      * @note the only cache line is t or b, no matter hit or no-hit, porbe perivious level cache
      */
    val l3c_state_dnxt_in_cktag = {
      val aqblk_req_cl = bram.cl_sel( aqblk_req_addr )

      Mux1H( Seq(
        bram.cache_coherence(aqblk_req_cl) === Coher.NONE -> fsm.flash,
        bram.cache_coherence(aqblk_req_cl) === Coher.TTIP -> Mux(bram.is_cb_hit, fsm.grant, fsm.evict),
        bram.cache_coherence(aqblk_req_cl) === Coher.TRUK -> fsm.probe
      ))


    }

    /**
      * @note when probe rtn in chn c, exit fsm.probe
      * @note when cache_miss goto fsm.evice, when cache_hit goto fsm.grant
      */
    val l3c_state_dnxt_in_probe = fsm.rlese

    /**
      * @note when axi_w rtn, exit evict, if entry from fence, goto fence, or goto cktag 
      */
    val l3c_state_dnxt_in_evict = 
      Mux( ~mem_mst_w.io.end, fsm.evict, 
        Mux1H(Seq(
          is_op_aqblk -> fsm.cktag,
          is_op_fence -> fsm.fence
        ))
      )

    /**
      * @note waitting for axi rtn and goto grant
      */
    val l3c_state_dnxt_in_flash = 
      Mux( mem_mst_r.io.end, fsm.cktag, fsm.flash )

    /**
      * @note when grant rtn, goto cfree
      */
    val l3c_state_dnxt_in_grant = 
      Mux( io.l2c_chn_c(req_no).e.fire, fsm.cfree, fsm.grant )

    /**
      * @note when no dirty, goto cfree or goto evict
      */
    val l3c_state_dnxt_in_fence = 
      Mux( bram.cache_coherence.contains(Coher.TRUK), fsm.probe, 
        Mux( bram.cache_coherence.forall(x:UInt => x === Coher.NONE), fsm.cfree, fsm.fence  )
      ) 
      
    val l3c_state_dnxt_in_rlese = 
      Mux1H(Seq(
        Oper.is_aqblk -> Mux( is_probe_rtn.forall( (x:Bool) => (x === true.B) ), fsm.cktag, fsm.rlese ),
        Oper.is_wbblk -> Mux( is_release_end, fsm.cfree, fsm.rlese ),
        Oper.is_fence -> Mux( is_probe_rtn.forall( (x:Bool) => (x === true.B) ), fsm.evict, fsm.rlese )
      ))

    Mux1H( Seq(
      (fsm.state_qout === fsm.cfree) -> l3c_state_dnxt_in_cfree,
      (fsm.state_qout === fsm.cktag) -> l3c_state_dnxt_in_cktag,
      (fsm.state_qout === fsm.flash) -> l3c_state_dnxt_in_flash,
      (fsm.state_qout === fsm.evict) -> l3c_state_dnxt_in_evict,
      (fsm.state_qout === fsm.probe) -> l3c_state_dnxt_in_probe,
      (fsm.state_qout === fsm.grant) -> l3c_state_dnxt_in_grant,
      (fsm.state_qout === fsm.fence) -> l3c_state_dnxt_in_fence,
      (fsm.state_qout === fsm.rlese) -> l3c_state_dnxt_in_rlese
    ))
  }



// BBBBBBBBBBBBBBBBB   RRRRRRRRRRRRRRRRR                  AAA               MMMMMMMM               MMMMMMMM
// B::::::::::::::::B  R::::::::::::::::R                A:::A              M:::::::M             M:::::::M
// B::::::BBBBBB:::::B R::::::RRRRRR:::::R              A:::::A             M::::::::M           M::::::::M
// BB:::::B     B:::::BRR:::::R     R:::::R            A:::::::A            M:::::::::M         M:::::::::M
//   B::::B     B:::::B  R::::R     R:::::R           A:::::::::A           M::::::::::M       M::::::::::M
//   B::::B     B:::::B  R::::R     R:::::R          A:::::A:::::A          M:::::::::::M     M:::::::::::M
//   B::::BBBBBB:::::B   R::::RRRRRR:::::R          A:::::A A:::::A         M:::::::M::::M   M::::M:::::::M
//   B:::::::::::::BB    R:::::::::::::RR          A:::::A   A:::::A        M::::::M M::::M M::::M M::::::M
//   B::::BBBBBB:::::B   R::::RRRRRR:::::R        A:::::A     A:::::A       M::::::M  M::::M::::M  M::::::M
//   B::::B     B:::::B  R::::R     R:::::R      A:::::AAAAAAAAA:::::A      M::::::M   M:::::::M   M::::::M
//   B::::B     B:::::B  R::::R     R:::::R     A:::::::::::::::::::::A     M::::::M    M:::::M    M::::::M
//   B::::B     B:::::B  R::::R     R:::::R    A:::::AAAAAAAAAAAAA:::::A    M::::::M     MMMMM     M::::::M
// BB:::::BBBBBB::::::BRR:::::R     R:::::R   A:::::A             A:::::A   M::::::M               M::::::M
// B:::::::::::::::::B R::::::R     R:::::R  A:::::A               A:::::A  M::::::M               M::::::M
// B::::::::::::::::B  R::::::R     R:::::R A:::::A                 A:::::A M::::::M               M::::::M
// BBBBBBBBBBBBBBBBB   RRRRRRRR     RRRRRRRAAAAAAA                   AAAAAAAMMMMMMMM               MMMMMMMM





  val flash_addr = RegEnable(
    Mux1H(
      (fsm.state_qout === fsm.cktag & fsm.state_dnxt === fsm.flash) ->
      (fsm.state_qout === fsm.flash & mem_mst_r.io.r.fire) -> flash_addr + 
    ),
    (fsm.state_qout === fsm.cktag & fsm.state_dnxt === fsm.flash) |
    (fsm.state_qout === fsm.flash & mem_mst_r.io.r.fire)
  )

  val release_addr = RegInit(0.U(64.W))
  when(l2c_slv.io.c.valid & release_addr === 0.U) {
    release_addr := l2c_slv.io.c.bits.address
  }
  .elsewhen( l2c_slv.io.release_end ) {
    release_addr := 0.U 
  }
  .elsewhen( l2c_slv.io.c.fire ) {
    release_addr := release_addr + 
  }
  assert( ~(l2c_slv.io.c.valid & l2c_slv.io.c.bits.address(,0) =/= 0.U), "Assert Failed at tlc, the address of chn_c is misalign" )

  val grant_addr = 
    RegEnable(
      Mux1H(
        (fsm.state_qout === fsm.cktag & fsm.state_dnxt === fsm.grant) ->
        (fsm.state_qout === fsm.grant & l2c_slv.io.d.fire) -> grant_addr + 
      ),
      (fsm.state_qout === fsm.cktag & fsm.state_dnxt === fsm.grant) |
      (fsm.state_qout === fsm.grant & l2c_slv.io.d.fire)
    )


  val evict_addr = 
    RegEnable(
      Mux1H(
        (fsm.state_dnxt === fsm.evict) ->
          Mux1H(Seq(
            is_op_aqblk -> ,//from cktag
            is_op_fence -> //from fence or release
          ))
        (fsm.state_qout === fsm.evict & mem_mst_w.io.w.fire) -> evict_addr + 
      ),
      (fsm.state_dnxt === fsm.evict) |
      (fsm.state_qout === fsm.evict & mem_mst_w.io.w.fire)
    )


  cache_dat.dat_addr_w :=
    Mux1H(
      (fsm.state_qout === fsm.flash) -> flash_addr,
      (fsm.state_qout === fsm.rlese) -> release_addr
    )


  cache_dat.dat_en_w :=
    ( fsm.state_qout === fsm.flash & mem_mst_r.io.r.fire ) |
    ( fsm.state_qout === fsm.rlese & l2c_slv.io.c.fire )

  cache_dat.dat_info_wstrb := Fill(16, 1.U)

  cache_dat.dat_info_w :=
    Mux1H( Seq(
      ( fsm.state_qout === fsm.flash ) -> mem_mst_r.io.r.bits.data,
      ( fsm.state_qout === fsm.rlese ) -> l2c_slv.io.c.bits.data,
    ))

  cache_dat.dat_en_r :=
    ( fsm.state_qout === fsm.evict) |
    ( fsm.state_qout === fsm.grant)




  cache_dat.dat_addr_r :=
    Mux1H(Seq(
      ( fsm.state_qout === fsm.evict) -> evict_addr,
      ( fsm.state_qout === fsm.grant) -> grant_addr
    ))

  bram.mem_dat := cache_mem.dat_info_r(0)

  val aqblk_req_addr = RegEnable()
  val fence_req_addr


  cache_tag.tag_addr_r := 
    Mux1H(Seq(
      is_op_aqblk -> aqblk_req_addr,
      is_op_fence -> fence_req_addr
    ))

  cache_tag.tag_addr_w := aqblk_req_addr

  cache_tag.tag_en_w := fsm.state_qout === fsm.cktag & fsm.state_dnxt === fsm.flash
  cache_tag.tag_en_r := fsm.state_dnxt === fsm.cktag


  bram.is_cb_hit := cache_tag.tag_info_r(0) === bram.tag_info


  

  
  val cache_dirty = RegInit(VecInit(Seq.fill(cl)(false.B)))


  for ( i <- 0 until cl ) yield {
    val cache_coherence_dnxt = Wire(Vec(cl, UInt(3.W)))


    when( (fsm.state_qout === fsm.fence) & (fsm.state_dnxt === fsm.cfree) ) {
      bram.cache_coherence(i) := bram.NONE
    }
    when
    bram.cache_coherence(i) :=

      Mux(
        (fsm.state_qout === fsm.fence) & (fsm.state_dnxt === fsm.cfree),
        bram.NONE,
        Mux(
          i.U =/= bram.cl_sel(),
          bram.cache_coherence(i),
          MuxCase( bram.cache_coherence(i), Array(
            (fsm.state_qout === fsm.evict & fsm.state_dnxt =/= fsm.evict) -> bram.NONE,
            (fsm.state_qout === fsm.flash & fsm.state_dnxt =/= fsm.flash) -> bram.TRUK
          ))
        )
      )

    bram.cache_dirty(i) := 
      Mux(
        i.U =/= bram.cl_sel,
        bram.cache_coherence(i),
        MuxCase( bram.cache_dirty(i), Array(
          (fsm.state_qout === fsm.rlese & ) -> ,
          (fsm.state_qout === fsm.probe & io.l2c_chn_c(req_no)) -> 
        ))
      )
  }






// FFFFFFFFFFFFFFFFFFFFFFEEEEEEEEEEEEEEEEEEEEEENNNNNNNN        NNNNNNNN        CCCCCCCCCCCCCEEEEEEEEEEEEEEEEEEEEEE
// F::::::::::::::::::::FE::::::::::::::::::::EN:::::::N       N::::::N     CCC::::::::::::CE::::::::::::::::::::E
// F::::::::::::::::::::FE::::::::::::::::::::EN::::::::N      N::::::N   CC:::::::::::::::CE::::::::::::::::::::E
// FF::::::FFFFFFFFF::::FEE::::::EEEEEEEEE::::EN:::::::::N     N::::::N  C:::::CCCCCCCC::::CEE::::::EEEEEEEEE::::E
//   F:::::F       FFFFFF  E:::::E       EEEEEEN::::::::::N    N::::::N C:::::C       CCCCCC  E:::::E       EEEEEE
//   F:::::F               E:::::E             N:::::::::::N   N::::::NC:::::C                E:::::E             
//   F::::::FFFFFFFFFF     E::::::EEEEEEEEEE   N:::::::N::::N  N::::::NC:::::C                E::::::EEEEEEEEEE   
//   F:::::::::::::::F     E:::::::::::::::E   N::::::N N::::N N::::::NC:::::C                E:::::::::::::::E   
//   F:::::::::::::::F     E:::::::::::::::E   N::::::N  N::::N:::::::NC:::::C                E:::::::::::::::E   
//   F::::::FFFFFFFFFF     E::::::EEEEEEEEEE   N::::::N   N:::::::::::NC:::::C                E::::::EEEEEEEEEE   
//   F:::::F               E:::::E             N::::::N    N::::::::::NC:::::C                E:::::E             
//   F:::::F               E:::::E       EEEEEEN::::::N     N:::::::::N C:::::C       CCCCCC  E:::::E       EEEEEE
// FF:::::::FF           EE::::::EEEEEEEE:::::EN::::::N      N::::::::N  C:::::CCCCCCCC::::CEE::::::EEEEEEEE:::::E
// F::::::::FF           E::::::::::::::::::::EN::::::N       N:::::::N   CC:::::::::::::::CE::::::::::::::::::::E
// F::::::::FF           E::::::::::::::::::::EN::::::N        N::::::N     CCC::::::::::::CE::::::::::::::::::::E
// FFFFFFFFFFF           EEEEEEEEEEEEEEEEEEEEEENNNNNNNN         NNNNNNN        CCCCCCCCCCCCCEEEEEEEEEEEEEEEEEEEEEE

  when( io.l3c_fence_req & ~db.is_fence ) { db.is_fence := true.B }
  .elsewhen( db.is_empty &  db.is_fence ) { db.is_fence := false.B }



// BBBBBBBBBBBBBBBBB   UUUUUUUU     UUUUUUUU   SSSSSSSSSSSSSSS 
// B::::::::::::::::B  U::::::U     U::::::U SS:::::::::::::::S
// B::::::BBBBBB:::::B U::::::U     U::::::US:::::SSSSSS::::::S
// BB:::::B     B:::::BUU:::::U     U:::::UUS:::::S     SSSSSSS
//   B::::B     B:::::B U:::::U     U:::::U S:::::S            
//   B::::B     B:::::B U:::::D     D:::::U S:::::S            
//   B::::BBBBBB:::::B  U:::::D     D:::::U  S::::SSSS         
//   B:::::::::::::BB   U:::::D     D:::::U   SS::::::SSSSS    
//   B::::BBBBBB:::::B  U:::::D     D:::::U     SSS::::::::SS  
//   B::::B     B:::::B U:::::D     D:::::U        SSSSSS::::S 
//   B::::B     B:::::B U:::::D     D:::::U             S:::::S
//   B::::B     B:::::B U::::::U   U::::::U             S:::::S
// BB:::::BBBBBB::::::B U:::::::UUU:::::::U SSSSSSS     S:::::S
// B:::::::::::::::::B   UU:::::::::::::UU  S::::::SSSSSS:::::S
// B::::::::::::::::B      UU:::::::::UU    S:::::::::::::::SS 
// BBBBBBBBBBBBBBBBB         UUUUUUUUU       SSSSSSSSSSSSSSS   



  io.l2c_chn_a <> l2c_slv.io.a
  io.l2c_chn_d <> l2c_slv.io.d
  io.mem_chn_ar <> mem_mst_r.io.ar
  io.mem_chn_r  <> mem_mst_r.io.r
  io.mem_chn_aw <> mem_mst_w.io.aw
  io.mem_chn_w  <> mem_mst_w.io.w
  io.mem_chn_b  <> mem_mst_w.io.b



  l2c_slv.io.is_rsp   := fsm.state_qout === L3C_state.cktag & fsm.state_dnxt === L3C_state.rspl2
  mem_mst_r.io.ar_req := fsm.state_qout === L3C_state.cktag & fsm.state_dnxt === L3C_state.flash
  mem_mst_w.io.aw_req := fsm.state_qout =/= L3C_state.evict & fsm.state_dnxt === L3C_state.evict

  l2c_slv.io.rsp_data := bram.mem_dat

  mem_mst_r.io.ar_info.addr :=
    RegEnable( bram.cache_addr_dnxt & ~("h1ff".U(32.W)), 0.U, mem_mst_r.io.ar_req )


  mem_mst_r.io.ar_info.burst := "b01".U
  mem_mst_r.io.ar_info.id := 0.U
  mem_mst_r.io.ar_info.len := 31.U
  mem_mst_r.io.ar_info.size := 4.U
  mem_mst_r.io.ar_info.user := 0.U
  mem_mst_r.io.ar_info.cache := 0.U
  mem_mst_r.io.ar_info.lock := 0.U
  mem_mst_r.io.ar_info.port := 0.U
  mem_mst_r.io.ar_info.qos := 0.U


  mem_mst_w.io.aw_info.addr :=
    RegEnable( bram.cache_addr_dnxt & ~("h1ff".U(32.W)), 0.U, mem_mst_w.io.aw_req )


  mem_mst_w.io.aw_info.burst := "b01".U
  mem_mst_w.io.aw_info.id := 0.U
  mem_mst_w.io.aw_info.len := 31.U
  mem_mst_w.io.aw_info.size := 4.U
  mem_mst_w.io.aw_info.user := 0.U
  mem_mst_w.io.aw_info.cache := 0.U
  mem_mst_w.io.aw_info.lock := 0.U
  mem_mst_w.io.aw_info.port := 0.U
  mem_mst_w.io.aw_info.qos := 0.U

  mem_mst_w.io.w_info.data := bram.mem_dat
  mem_mst_w.io.w_info.strb := Fill(16, 1.U)
  mem_mst_w.io.w_info.user := 0.U
  mem_mst_w.io.w_info.last := DontCare



}

