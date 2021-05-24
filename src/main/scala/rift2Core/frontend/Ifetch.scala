/*
* @Author: Ruige Lee
* @Date:   2021-04-08 14:32:49
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-15 16:18:45
*/

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


package rift2Core.frontend

import chisel3._
import chisel3.util._
import chisel3.util.Enum._
import chisel3.util.random._

import chisel3.experimental.ChiselEnum
import rift2Core.define._
import rift2Core.cache._
import rift2Core.frontend._
import tilelink._






class Ifetch() extends Module with IBuf{
	val io = IO(new Bundle{
		val pc_if = Flipped(new DecoupledIO( UInt(64.W) ))

		val if_iq = Vec(4, new DecoupledIO(UInt(16.W)) )

		val il1_chn_a = new DecoupledIO(new TLchannel_a(128, 32))
		val il1_chn_d = Flipped(new DecoupledIO( new TLchannel_d(128) ))

		val is_il1_fence_req = Input(Bool())

		val flush = Input(Bool())
	})

	val cfree :: cktag :: cmiss :: fence :: Nil = Enum(4)

	val dw = 128
	val bk = 4
	val cb = 2
	val cl = 256

	val addr_lsb = log2Ceil(dw*bk/8)
	val line_w = log2Ceil(cl)
	val tag_w = 32 - addr_lsb - line_w

	val if_addr = Wire(UInt(64.W))
	val if_addr_reg = RegInit( "h80000000".U(64.W) )



	def addr_align_128 = if_addr & ~("b1111".U(64.W))
	def addr_align_256 = if_addr & ~("b11111".U(64.W))
	def addr_align_512 = if_addr & ~("b111111".U(64.W))
	def addr_tag = if_addr(31, 32-tag_w)
	def cl_sel = if_addr(addr_lsb+line_w-1, addr_lsb)




	val il1_mst = Module(new TileLink_mst_heavy(128, 32, 0))
	val mem = new Cache_mem( dw, 32, bk, cb, cl )
	val addr_il1_req = RegInit( "h80000000".U(32.W) )
	val cache_valid = Reg( Vec( cl, Vec(cb, Bool()) ) )
	val random_res = LFSR(log2Ceil(4), true.B )(0)
	val is_cb_vhit = Wire(Vec(cb, Bool()))
	val trans_kill = RegInit(false.B)
	val is_il1_fence_req = RegInit(false.B)

	val fsm = new Bundle{
		val state_dnxt = Wire(UInt(2.W))
		val state_qout = RegNext(state_dnxt, cfree)
	}

	if_addr_reg := Mux( (fsm.state_qout === cfree), io.pc_if.bits, if_addr_reg)
	if_addr     := Mux( (fsm.state_qout === cfree), io.pc_if.bits, if_addr_reg)

	io.if_iq <> ibuf.io.deq
	ibuf.io.flush := io.flush | io.is_il1_fence_req


	ia.pc := if_addr
	ia.instr := {
		val mem_dat = {
			val cb_num = for ( i <- 0 until cb ) yield { is_cb_vhit(i) === true.B }
			val dat_sel = for ( i <- 0 until cb ) yield { mem.dat_info_r(i) }
			MuxCase( DontCare, cb_num zip dat_sel )	
		}

		Mux( 
			( (fsm.state_qout === cktag) & is_cb_vhit.contains(true.B) & ~io.flush),
			mem_dat,
			Mux(
				( fsm.state_qout === cmiss & (addr_align_128 === addr_il1_req) ),
				il1_mst.io.d.bits.data,
				DontCare
			)
		)
	}
	


	ibuf_valid_i := 
				( (fsm.state_qout === cktag) & is_cb_vhit.contains(true.B) ) |
				( fsm.state_qout === cmiss & il1_mst.io.d.fire & (addr_align_128 === addr_il1_req) & ~trans_kill)



	io.pc_if.ready := ~trans_kill & (fsm.state_qout === cktag | fsm.state_qout === cmiss) & (fsm.state_dnxt === cfree)





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


	




	fsm.state_dnxt := {
		val il1_state_dnxt_in_cfree = 
			Mux( is_il1_fence_req, fence,
				Mux( ( (ibuf_ready_i) & ~io.flush), cktag, cfree) ) // no valid or valid is ack then next
		val il1_state_dnxt_in_cktag = 
			Mux( is_cb_vhit.contains(true.B), cfree, cmiss )
		val il1_state_dnxt_in_cmiss = 
			Mux( il1_mst.io.mode === 7.U, cfree, cmiss )
		val il1_state_dnxt_in_fence = 
			Mux( ~is_il1_fence_req, cfree, fence )

		MuxCase( cfree, Array(
			( fsm.state_qout === cfree ) -> il1_state_dnxt_in_cfree,
			( fsm.state_qout === cktag ) -> il1_state_dnxt_in_cktag,
			( fsm.state_qout === cmiss ) -> il1_state_dnxt_in_cmiss,
			( fsm.state_qout === fence ) -> il1_state_dnxt_in_fence
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

	when ( true.B ) {
		addr_il1_req := Mux1H( Seq(
						(fsm.state_qout === cfree) -> addr_align_512,
						(fsm.state_qout === cktag) -> addr_align_512,
						(fsm.state_qout === cmiss) -> Mux( il1_mst.io.d.fire, addr_il1_req + ( 1.U << (log2Ceil(dw/8)).U) , addr_il1_req),
						(fsm.state_qout === fence) -> addr_align_512
						))
	}


	mem.cache_addr := Mux1H( Seq(
							(fsm.state_qout === cfree) -> addr_align_128,
							(fsm.state_qout === cktag) -> addr_align_128,
							(fsm.state_qout === cmiss) -> addr_il1_req,
							(fsm.state_qout === fence) -> addr_align_128
							))

	for ( i <- 0 until cb ) yield {
		mem.dat_en_w(i) := (fsm.state_qout === cmiss) & (il1_mst.io.d.fire & is_cb_vhit(i))
		mem.dat_en_r(i) := (fsm.state_dnxt === cktag)
		mem.tag_en_w(i) := (fsm.state_qout === cktag) & ((fsm.state_dnxt === cmiss) & is_block_replace(i.U))
		mem.tag_en_r(i) :=
				((fsm.state_qout === cfree) & (fsm.state_dnxt === cktag)) |
				((fsm.state_qout === cmiss) & (il1_mst.io.a.fire))
	}

	mem.dat_info_wstrb := "hFFFF".U
	mem.dat_info_w := il1_mst.io.d.bits.data




	when( reset.asBool ) {
		for ( i <- 0 until cl; j <- 0 until cb ) yield cache_valid(i)(j) := false.B
	}
	.elsewhen( fsm.state_qout === cktag & fsm.state_dnxt === cmiss ) {
		cache_valid(cl_sel)(replace_sel) := true.B
	}
	.elsewhen( fsm.state_qout === fence & fsm.state_dnxt === cfree ) {
		for ( i <- 0 until cl; j <- 0 until cb ) yield cache_valid(i)(j) := false.B
	}

	

	for ( i <- 0 until cb ) yield { is_cb_vhit(i) := ( cache_valid(cl_sel)(i) & (mem.tag_info_r(i) === addr_tag)) }


	def replace_sel = 
		Mux(
			cache_valid(cl_sel).contains(false.B),
			cache_valid(cl_sel).indexWhere((a: Bool) => (a === false.B)),
			random_res
		)

	def is_block_replace(i:UInt) = UIntToOH(replace_sel)(i).asBool




	
	


// IIIIIIIIIILLLLLLLLLLL               1111111   
// I::::::::IL:::::::::L              1::::::1   
// I::::::::IL:::::::::L             1:::::::1   
// II::::::IILL:::::::LL             111:::::1   
//   I::::I    L:::::L                  1::::1   
//   I::::I    L:::::L                  1::::1   
//   I::::I    L:::::L                  1::::1   
//   I::::I    L:::::L                  1::::l   
//   I::::I    L:::::L                  1::::l   
//   I::::I    L:::::L                  1::::l   
//   I::::I    L:::::L                  1::::l   
//   I::::I    L:::::L         LLLLLL   1::::l   
// II::::::IILL:::::::LLLLLLLLL:::::L111::::::111
// I::::::::IL::::::::::::::::::::::L1::::::::::1
// I::::::::IL::::::::::::::::::::::L1::::::::::1
// IIIIIIIIIILLLLLLLLLLLLLLLLLLLLLLLL111111111111

	io.il1_chn_a <> il1_mst.io.a
	io.il1_chn_d <> il1_mst.io.d


	il1_mst.io.a_info.opcode  := il1_mst.Get
	il1_mst.io.a_info.param   := DontCare
	il1_mst.io.a_info.size    := addr_lsb.U
	il1_mst.io.a_info.source  := 0.U
	il1_mst.io.a_info.address := addr_align_512
	il1_mst.io.a_info.mask    := DontCare
	il1_mst.io.a_info.data    := DontCare
	il1_mst.io.a_info.corrupt := false.B

	il1_mst.io.is_req := fsm.state_qout === cktag & fsm.state_dnxt === cmiss









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




	when ( io.flush & fsm.state_dnxt === cmiss ) {
		trans_kill := true.B
	}
	.elsewhen( fsm.state_dnxt =/= cmiss ) {
		trans_kill := false.B
	}


	when ( io.is_il1_fence_req & ~is_il1_fence_req ) {
		is_il1_fence_req := true.B
	}
	.elsewhen( ~io.is_il1_fence_req & is_il1_fence_req ) {
		is_il1_fence_req := false.B
	}







}








