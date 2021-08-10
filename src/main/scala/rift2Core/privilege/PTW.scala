
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
import rift2Core.L1Cache._


import rift._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._


/** 
  * page table walker
  */ 
class PTW(edge: TLEdgeOut)(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val vaddr = Flipped(ValidIO(UInt(64.W)))
    val ptw_o = ValidIO(new Info_pte_sv39)
    val is_ptw_fail = Output(Bool())
    val satp_ppn = Input(UInt(44.W))

    val ptw_get    = new DecoupledIO(new TLBundleA(edge.bundle))
    val ptw_access = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))

    // val ptw_chn_a = new DecoupledIO(new TLchannel_a(64, 32))
    // val ptw_chn_d = Flipped(new DecoupledIO(new TLchannel_d(64)))
  })

  object state {
    val free = 0.U
    val lvl2 = 1.U
    val lvl1 = 2.U
    val lvl0 = 3.U
  }

  // val ptw_mst = Module( new TileLink_mst_heavy(64, 32, 2))



  val fsm = new Bundle {
    val state_dnxt = Wire(UInt(2.W))
    val state_qout = RegNext( state_dnxt, state.free )

  }

  val walk = new Bundle {
    val a     = Wire(UInt(44.W))
    // val level = RegInit(0.U(2.W))
    val is_ptw_end = Wire(Bool())
    val is_ptw_fail = Wire(Bool())
    val pte_value = RegInit(0.U(256.W))
    val pte  = pte_value.asTypeOf(new Info_pte_sv39)
    val addr_dnxt = Wire(UInt(56.W))
    val addr_qout = RegNext(addr_dnxt, 0.U(56.W))
    
    a :=
      Mux1H(Seq(
        ( fsm.state_dnxt === state.lvl2 ) -> io.satp_ppn,
        ( fsm.state_dnxt === state.lvl1 ) -> Cat(pte.ppn(2), pte.ppn(1), pte.ppn(0)),
        ( fsm.state_dnxt === state.lvl0 ) -> Cat(pte.ppn(2), pte.ppn(1), pte.ppn(0)),
      ))

    addr_dnxt :=
      Cat(a, Mux1H(Seq(
        (fsm.state_dnxt === state.lvl0) -> Cat(io.vaddr.bits(20,12), 0.U(3.W)),
        (fsm.state_dnxt === state.lvl1) -> Cat(io.vaddr.bits(29,21), 0.U(3.W)),
        (fsm.state_dnxt === state.lvl2) -> Cat(io.vaddr.bits(38,30), 0.U(3.W)),
        ))
      )

    def bk_sel_dnxt = addr_dnxt(4,3)
    def bk_sel_qout = addr_qout(4,3)
    // def cl_sel = addr(11,5)
    def tag_sel = addr_qout(55,12)

    is_ptw_end := 
      pte.R === true.B & 
      pte.X === true.B


    is_ptw_fail :=
      ( ~is_ptw_end & 
      Mux1H(Seq(
        (fsm.state_qout === state.free) -> false.B,
        (fsm.state_qout === state.lvl2) -> (pte.ppn(0) =/= 0.U | pte.ppn(1) =/= 0.U),
        (fsm.state_qout === state.lvl1) -> (pte.ppn(0) =/= 0.U),
        (fsm.state_qout === state.lvl0) -> false.B,
      ))
      ) |
      ( pte.V === false.B | (pte.R === false.B & pte.W === true.B))

            
    pte.is_4K_page   := is_ptw_end & fsm.state_qout === state.lvl0
    pte.is_mega_page := is_ptw_end & fsm.state_qout === state.lvl1       
    pte.is_giga_page := is_ptw_end & fsm.state_qout === state.lvl2

  }

  val is_valid = RegInit(VecInit( Seq.fill(128)(false.B) ))

  val cache_dat = new Cache_dat( 64, 56, 4, 1, 128 )
  val cache_tag = new Cache_tag( 64, 56, 4, 1, 128 ){ require ( tag_w == 44 ) } 

  val (_, _, is_trans_done, transCnt) = edge.count(io.ptw_access)

  val ptw_get_valid = RegInit(false.B)
  val ptw_access_ready = RegInit(false.B)
  val ptw_access_data = RegInit( 0.U(256.W) )

  cache_dat.dat_addr_r := walk.addr_dnxt
  cache_tag.tag_addr_r := walk.addr_dnxt

  cache_dat.dat_addr_w := walk.addr_qout
  cache_tag.tag_addr_w := walk.addr_qout


  for( i <- 0 until 1 ) yield {
    val rd_enable = 
      (fsm.state_qout === state.free & fsm.state_dnxt === state.lvl2) |
      (fsm.state_qout === state.lvl2 & fsm.state_dnxt === state.lvl1) |
      (fsm.state_qout === state.lvl1 & fsm.state_dnxt === state.lvl0)

    val wr_enable = 
      (fsm.state_qout === state.lvl2 & io.ptw_access.fire & is_trans_done & ~walk.is_ptw_fail) |
      (fsm.state_qout === state.lvl1 & io.ptw_access.fire & is_trans_done & ~walk.is_ptw_fail) |
      (fsm.state_qout === state.lvl0 & io.ptw_access.fire & is_trans_done & ~walk.is_ptw_fail)

    cache_tag.tag_en_w(i) := wr_enable
    cache_tag.tag_en_r(i) := rd_enable

    for ( j <- 0 until 4 ) yield {
      cache_dat.dat_en_r(i)(j) := rd_enable & (j.U === walk.bk_sel_dnxt)
      cache_dat.dat_en_w(i)(j) := wr_enable
    }
  }

  for ( j <- 0 until 4 ) yield {
    cache_dat.dat_info_w(j) := ptw_access_data(64*j+63, 64*j)
    cache_dat.dat_info_wstrb(j) := "hFF".U
  }

  val is_hit = (walk.tag_sel === cache_tag.tag_info_r(0))



  when(is_hit) {
    walk.pte_value := cache_dat.dat_info_r(0)(walk.bk_sel_qout)
  }
  .elsewhen( io.ptw_access.fire & is_trans_done ) {
    val data =
      VecInit(
        for ( j <- 0 until 4) yield { ptw_access_data(64*j+63, 64*j) }
      )
    walk.pte_value := data(walk.bk_sel_qout)
  }



//    SSSSSSSSSSSSSSS TTTTTTTTTTTTTTTTTTTTTTT         AAA         TTTTTTTTTTTTTTTTTTTTTTTEEEEEEEEEEEEEEEEEEEEEE
//  SS:::::::::::::::ST:::::::::::::::::::::T        A:::A        T:::::::::::::::::::::TE::::::::::::::::::::E
// S:::::SSSSSS::::::ST:::::::::::::::::::::T       A:::::A       T:::::::::::::::::::::TE::::::::::::::::::::E
// S:::::S     SSSSSSST:::::TT:::::::TT:::::T      A:::::::A      T:::::TT:::::::TT:::::TEE::::::EEEEEEEEE::::E
// S:::::S            TTTTTT  T:::::T  TTTTTT     A:::::::::A     TTTTTT  T:::::T  TTTTTT  E:::::E       EEEEEE
// S:::::S                    T:::::T            A:::::A:::::A            T:::::T          E:::::E             
//  S::::SSSS                 T:::::T           A:::::A A:::::A           T:::::T          E::::::EEEEEEEEEE   
//   SS::::::SSSSS            T:::::T          A:::::A   A:::::A          T:::::T          E:::::::::::::::E   
//     SSS::::::::SS          T:::::T         A:::::A     A:::::A         T:::::T          E:::::::::::::::E   
//        SSSSSS::::S         T:::::T        A:::::AAAAAAAAA:::::A        T:::::T          E::::::EEEEEEEEEE   
//             S:::::S        T:::::T       A:::::::::::::::::::::A       T:::::T          E:::::E             
//             S:::::S        T:::::T      A:::::AAAAAAAAAAAAA:::::A      T:::::T          E:::::E       EEEEEE
// SSSSSSS     S:::::S      TT:::::::TT   A:::::A             A:::::A   TT:::::::TT      EE::::::EEEEEEEE:::::E
// S::::::SSSSSS:::::S      T:::::::::T  A:::::A               A:::::A  T:::::::::T      E::::::::::::::::::::E
// S:::::::::::::::SS       T:::::::::T A:::::A                 A:::::A T:::::::::T      E::::::::::::::::::::E
//  SSSSSSSSSSSSSSS         TTTTTTTTTTTAAAAAAA                   AAAAAAATTTTTTTTTTT      EEEEEEEEEEEEEEEEEEEEEE



  val ptw_state_dnxt_in_free = Mux( io.vaddr.valid, state.lvl2, state.free )
  val ptw_state_dnxt_in_lvl2 = 
    Mux(
      (io.ptw_access.fire & is_trans_done) | is_hit,
      Mux( walk.is_ptw_end | io.is_ptw_fail, state.free, state.lvl1 ),
      state.lvl2
    )
  val ptw_state_dnxt_in_lvl1 = 
    Mux(
      (io.ptw_access.fire & is_trans_done) | is_hit,
      Mux( walk.is_ptw_end | io.is_ptw_fail, state.free, state.lvl0 ),
      state.lvl1
    )
  val ptw_state_dnxt_in_lvl0 = 
    Mux(
      (io.ptw_access.fire & is_trans_done) | is_hit,
      Mux( walk.is_ptw_end | io.is_ptw_fail, state.free, state.free ),
      state.lvl0
    )


  fsm.state_dnxt := Mux1H( Seq(
    (fsm.state_qout === state.free) -> ptw_state_dnxt_in_free,
    (fsm.state_qout === state.lvl2) -> ptw_state_dnxt_in_lvl2,
    (fsm.state_qout === state.lvl1) -> ptw_state_dnxt_in_lvl1,
    (fsm.state_qout === state.lvl0) -> ptw_state_dnxt_in_lvl0,
  ))

  assert( 
    PopCount( Seq(
      fsm.state_qout === state.free,
      fsm.state_qout === state.lvl2,
      fsm.state_qout === state.lvl1,
      fsm.state_qout === state.lvl0,
    )) === 1.U, "Assert Faild at ptw FSM!"
  )




  io.is_ptw_fail := walk.is_ptw_fail

  io.ptw_o.bits  := walk.pte
  io.ptw_o.valid := walk.is_ptw_end

















// TTTTTTTTTTTTTTTTTTTTTTT  iiii  lllllll                   LLLLLLLLLLL               iiii                   kkkkkkkk           
// T:::::::::::::::::::::T i::::i l:::::l                   L:::::::::L              i::::i                  k::::::k           
// T:::::::::::::::::::::T  iiii  l:::::l                   L:::::::::L               iiii                   k::::::k           
// T:::::TT:::::::TT:::::T        l:::::l                   LL:::::::LL                                      k::::::k           
// TTTTTT  T:::::T  TTTTTTiiiiiii  l::::l     eeeeeeeeeeee    L:::::L               iiiiiiinnnn  nnnnnnnn     k:::::k    kkkkkkk
//         T:::::T        i:::::i  l::::l   ee::::::::::::ee  L:::::L               i:::::in:::nn::::::::nn   k:::::k   k:::::k 
//         T:::::T         i::::i  l::::l  e::::::eeeee:::::eeL:::::L                i::::in::::::::::::::nn  k:::::k  k:::::k  
//         T:::::T         i::::i  l::::l e::::::e     e:::::eL:::::L                i::::inn:::::::::::::::n k:::::k k:::::k   
//         T:::::T         i::::i  l::::l e:::::::eeeee::::::eL:::::L                i::::i  n:::::nnnn:::::n k::::::k:::::k    
//         T:::::T         i::::i  l::::l e:::::::::::::::::e L:::::L                i::::i  n::::n    n::::n k:::::::::::k     
//         T:::::T         i::::i  l::::l e::::::eeeeeeeeeee  L:::::L                i::::i  n::::n    n::::n k:::::::::::k     
//         T:::::T         i::::i  l::::l e:::::::e           L:::::L         LLLLLL i::::i  n::::n    n::::n k::::::k:::::k    
//       TT:::::::TT      i::::::il::::::le::::::::e        LL:::::::LLLLLLLLL:::::Li::::::i n::::n    n::::nk::::::k k:::::k   
//       T:::::::::T      i::::::il::::::l e::::::::eeeeeeeeL::::::::::::::::::::::Li::::::i n::::n    n::::nk::::::k  k:::::k  
//       T:::::::::T      i::::::il::::::l  ee:::::::::::::eL::::::::::::::::::::::Li::::::i n::::n    n::::nk::::::k   k:::::k 
//       TTTTTTTTTTT      iiiiiiiillllllll    eeeeeeeeeeeeeeLLLLLLLLLLLLLLLLLLLLLLLLiiiiiiii nnnnnn    nnnnnnkkkkkkkk    kkkkkkk



  io.ptw_get.valid := ptw_get_valid
  io.ptw_access.ready := ptw_access_ready

  val is_get_reqed = RegInit(false.B)
  when( io.ptw_get.fire ) { is_get_reqed := true.B }
  .elsewhen( fsm.state_dnxt =/= fsm.state_qout ) { is_get_reqed := false.B }


  when( (fsm.state_qout === state.lvl2 | fsm.state_qout === state.lvl1 | fsm.state_qout === state.lvl0) & ~is_hit & ~io.ptw_get.valid ) {
    ptw_get_valid := true.B
  } .elsewhen( io.ptw_get.fire ) {
    ptw_get_valid := false.B
  }

  when( io.ptw_access.valid & ~io.ptw_access.ready) {
    ptw_access_ready := true.B
  } .elsewhen( io.ptw_access.fire ) {
    ptw_access_ready := false.B
  }

  io.ptw_get.bits := edge.Get(fromSource = 0.U, toAddress = walk.addr_qout, lgSize = log2Ceil(256/8).U)._2

  when( io.ptw_access.fire ) {
    ptw_access_data := 
      Mux( is_trans_done,
        Cat( io.ptw_access.bits.data, ptw_access_data(127,0)  ),
        Cat( ptw_access_data(255,128), io.ptw_access.bits.data )
      )
  }




}

