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

import tilelink._

class Info_pte_sv39 extends Bundle {
	val V = Bool()
	val R = Bool()
	val W = Bool()
	val X = Bool()
	val U = Bool()
	val G = Bool()
	val A = Bool()
	val D = Bool()
	val rsw = UInt(2.W)

	val ppn = MixedVec( Seq( UInt(9.W), UInt(9.W), UInt(26.W) ) )

}

class Info_tlb_tag extends Bundle {
	val is_valid = Bool()
	val asid = UInt( 16.W )
	val vpn = Vec( 3, UInt(9.W) )
	val is_4K_page = Bool()
	val is_mega_page = Bool()
	val is_giga_page = Bool()

}



class Info_ptw_tlb extends Bundle {
	val pte = new Info_pte_sv39
	val is_4K_page = Bool()
	val is_giga_page = Bool()
	val is_mega_page = Bool()
}




object Page {

	def apply( info: UInt ) = {
		val pte = new Info_pte_sv39

		pte.V      := info(0).asBool
		pte.R      := info(1).asBool
		pte.W      := info(2).asBool
		pte.X      := info(3).asBool
		pte.U      := info(4).asBool
		pte.G      := info(5).asBool
		pte.A      := info(6).asBool
		pte.D      := info(7).asBool
		pte.rsw    := info(9,8)
		pte.ppn(0) := info(18,10)
		pte.ppn(1) := info(27,19)
		pte.ppn(2) := info(53,28)

		pte
	}


}


/** translation lookaside buffer for instance both itlb and dtlb
  * @param entry the entries number of fully associative tlb
  * 
  * @param is_tlb
  */ 
class TLB( entry: Int = 32 ) extends Module {
	val io = IO(new Bundle{

		val pte_o  = Output( new Info_pte_sv39 )
		val vaddr  = Input( UInt(64.W) )
		val asid_i = Input(UInt(16.W))

		val ptw_tlb = Flipped(ValidIO(new Info_ptw_tlb))
		val is_tlb_hit = Output(Bool())




		val flush = Input(Bool())
	})

	val tag = RegInit( VecInit( Seq.fill(entry)(0.U.asTypeOf( new Info_tlb_tag  ))))
	val pte = RegInit( VecInit( Seq.fill(entry)(0.U.asTypeOf( new Info_pte_sv39 ))))

	when( io.flush ) {
		for( i <- 0 until entry ) yield tag(i) := 0.U.asTypeOf( new Info_tlb_tag )
	}
	.elsewhen(io.ptw_tlb.valid){
		tag(tlb_update_idx).is_valid := true.B
		tag(tlb_update_idx).asid := io.asid_i
		tag(tlb_update_idx).is_4K_page   := io.ptw_tlb.bits.is_4K_page
		tag(tlb_update_idx).is_giga_page := io.ptw_tlb.bits.is_giga_page
		tag(tlb_update_idx).is_mega_page := io.ptw_tlb.bits.is_mega_page
		tag(tlb_update_idx).vpn(0) := io.vaddr(20,12)
		tag(tlb_update_idx).vpn(1) := io.vaddr(29,21)
		tag(tlb_update_idx).vpn(2) := io.vaddr(38,30)


		pte(tlb_update_idx) := io.ptw_tlb.bits.pte
	}






	/** tlb replace in random 
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
	io.is_tlb_hit := tlb_hit.contains(true.B)



}




/** page table walker
  * 
  */ 
class PTW extends Module {
	val io = IO(new Bundle{
		val vaddr = Flipped(ValidIO(UInt(64.W)))
		val ptw_tlb = ValidIO(new Info_ptw_tlb)

		val ptw_chn_a = new DecoupledIO(new TLchannel_a(64, 32))
		val ptw_chn_d = Flipped(new DecoupledIO(new TLchannel_d(64)))
	})

	object state {
		val free = 0.U
		val read = 1.U
		val walk = 2.U
	}

	val ptw_mst = Module( new TileLink_mst_heavy(64, 32, 2))
	val is_ptw_end = Wire(Bool())


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


	val ptw_state_dnxt_in_free = Mux( io.vaddr.valid, state.walk, state.free )
	val ptw_state_dnxt_in_read = Mux( io.ptw_chn_d.fire, state.walk, state.read )
	val ptw_state_dnxt_in_walk = Mux( is_ptw_end , state.free, state.read)

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

	ptw_mst.io.a_info.address := 
	ptw_mst.io.a_info.corrupt := 
	ptw_mst.io.a_info.data := 
	ptw_mst.io.a_info.mask := 
	ptw_mst.io.a_info.opcode := 
	ptw_mst.io.a_info.param := 
	ptw_mst.io.a_info.size := 
	ptw_mst.io.a_info.source := 
	ptw_mst.io.is_req := 





}




class PMP extends Module {
	val io = IO(new Bundle{

	})





}


class MMU extends Module {
	val io = IO(new Bundle{
		
	})
}
