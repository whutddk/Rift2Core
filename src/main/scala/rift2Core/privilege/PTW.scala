
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

import tilelink._




/** 
  * page table walker
  */ 
class PTW extends Module {
  val io = IO(new Bundle{
    val vaddr = Flipped(ValidIO(UInt(64.W)))
    val ptw_o = ValidIO(new Info_pte_sv39)
    val is_ptw_fail = Output(Bool())
    val satp_ppn = Input(UInt(44.W))

    val ptw_chn_a = new DecoupledIO(new TLchannel_a(64, 32))
    val ptw_chn_d = Flipped(new DecoupledIO(new TLchannel_d(64)))
  })

  object state {
    val free = 0.U
    val read = 1.U
    val walk = 2.U
  }

  val ptw_mst = Module( new TileLink_mst_heavy(64, 32, 2))



  val fsm = new Bundle {
    val state_dnxt = Wire(UInt(2.W))
    val state_qout = RegNext( state_dnxt, state.free )

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


  val ptw_state_dnxt_in_free = Mux( io.vaddr.valid, state.read, state.free )
  val ptw_state_dnxt_in_read = Mux( io.ptw_chn_d.fire, state.walk, state.read )
  val ptw_state_dnxt_in_walk = Mux( walk.is_ptw_end | io.is_ptw_fail , state.free, state.read)

  fsm.state_dnxt := Mux1H( Seq(
    (fsm.state_qout === state.free) -> ptw_state_dnxt_in_free,
    (fsm.state_qout === state.read) -> ptw_state_dnxt_in_read,
    (fsm.state_qout === state.walk) -> ptw_state_dnxt_in_walk,
  ))

  assert( 
    PopCount( Seq(
      fsm.state_qout === state.free,
      fsm.state_qout === state.read,
      fsm.state_qout === state.walk
    )) === 1.U, "Assert Faild at ptw FSM!"
  )




// WWWWWWWW                           WWWWWWWW   AAA               LLLLLLLLLLL             KKKKKKKKK    KKKKKKK
// W::::::W                           W::::::W  A:::A              L:::::::::L             K:::::::K    K:::::K
// W::::::W                           W::::::W A:::::A             L:::::::::L             K:::::::K    K:::::K
// W::::::W                           W::::::WA:::::::A            LL:::::::LL             K:::::::K   K::::::K
//  W:::::W           WWWWW           W:::::WA:::::::::A             L:::::L               KK::::::K  K:::::KKK
//   W:::::W         W:::::W         W:::::WA:::::A:::::A            L:::::L                 K:::::K K:::::K   
//    W:::::W       W:::::::W       W:::::WA:::::A A:::::A           L:::::L                 K::::::K:::::K    
//     W:::::W     W:::::::::W     W:::::WA:::::A   A:::::A          L:::::L                 K:::::::::::K     
//      W:::::W   W:::::W:::::W   W:::::WA:::::A     A:::::A         L:::::L                 K:::::::::::K     
//       W:::::W W:::::W W:::::W W:::::WA:::::AAAAAAAAA:::::A        L:::::L                 K::::::K:::::K    
//        W:::::W:::::W   W:::::W:::::WA:::::::::::::::::::::A       L:::::L                 K:::::K K:::::K   
//         W:::::::::W     W:::::::::WA:::::AAAAAAAAAAAAA:::::A      L:::::L         LLLLLLKK::::::K  K:::::KKK
//          W:::::::W       W:::::::WA:::::A             A:::::A   LL:::::::LLLLLLLLL:::::LK:::::::K   K::::::K
//           W:::::W         W:::::WA:::::A               A:::::A  L::::::::::::::::::::::LK:::::::K    K:::::K
//            W:::W           W:::WA:::::A                 A:::::A L::::::::::::::::::::::LK:::::::K    K:::::K
//             WWW             WWWAAAAAAA                   AAAAAAALLLLLLLLLLLLLLLLLLLLLLLLKKKKKKKKK    KKKKKKK

  val walk = new Bundle {
    val a     = RegInit(0.U(44.W))
    val level = RegInit(0.U(2.W))
    val is_ptw_end = Wire(Bool())
    val pte_value = RegEnable( ptw_mst.io.d.bits.data, ptw_mst.io.d.fire )
    val pte  = Wire(new Info_pte_sv39)
    

    val addr  = Cat(a, Mux1H(Seq(
              (level === 0.U) -> Cat(io.vaddr.bits(20,12), 0.U(3.W)),
              (level === 1.U) -> Cat(io.vaddr.bits(29,21), 0.U(3.W)),
              (level === 2.U) -> Cat(io.vaddr.bits(38,30), 0.U(3.W)),
              ))
          )
  }

  walk.pte.value := walk.pte_value

  walk.is_ptw_end := 
    fsm.state_qout === state.walk & 
    walk.pte.R === true.B & 
    walk.pte.X === true.B

  io.is_ptw_fail :=
    fsm.state_qout === state.walk & (
      ( ~walk.is_ptw_end & MuxCase( false.B, Seq( (walk.level === 0.U) -> false.B, (walk.level === 1.U) -> (walk.pte.ppn(0) =/= 0.U), (walk.level === 2.U) -> (walk.pte.ppn(0) =/= 0.U | walk.pte.ppn(1) =/= 0.U)) )) |
      ( walk.pte.V === false.B | (walk.pte.R === false.B & walk.pte.W === true.B))
    )
            
  walk.pte.is_4K_page   := walk.is_ptw_end & walk.level === 0.U
  walk.pte.is_mega_page := walk.is_ptw_end & walk.level === 1.U       
  walk.pte.is_giga_page := value & walk.level === 2.U

  io.ptw_o.bits  := walk.pte
  io.ptw_o.valid := walk.is_ptw_end

  when( fsm.state_qout === state.free & fsm.state_dnxt === state.read ) {
    walk.a     := io.satp_ppn
    walk.level := 2.U
  }
  .elsewhen( fsm.state_qout === state.walk & fsm.state_dnxt === state.read ) {
    walk.a     := Cat(walk.pte.ppn(2), walk.pte.ppn(1), walk.pte.ppn(0))
    walk.level := walk.level - 1.U
  }





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

  io.ptw_chn_a <> ptw_mst.io.a
  io.ptw_chn_d <> ptw_mst.io.d

  ptw_mst.io.a_info.address := walk.addr
  ptw_mst.io.a_info.corrupt := false.B
  ptw_mst.io.a_info.data := DontCare
  ptw_mst.io.a_info.mask := DontCare
  ptw_mst.io.a_info.opcode := ptw_mst.Get
  ptw_mst.io.a_info.param := DontCare
  ptw_mst.io.a_info.size := 3.U
  ptw_mst.io.a_info.source := 2.U

  ptw_mst.io.is_req := (fsm.state_dnxt === state.read & fsm.state_qout =/= state.read)




}

