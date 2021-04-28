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


package rift2Core

import chisel3._
import chisel3.util._
import chisel3.util.random._

import chisel3.experimental.ChiselEnum
import rift2Core.define._
import rift2Core.cache._
import rift2Core.frontend._
import tilelink._




object Il1_state extends ChiselEnum {
	val cfree, cktag, cmiss, fence = Value
}


class Ifetch() extends Module with IBuf{
	val io = IO(new Bundle{
		val pc_if = Flipped(new DecoupledIO( new Info_pc_if ))

		val if_iq = Vec(4, new DecoupledIO(UInt(16.W)) )

		val il1_chn_a = new DecoupledIO(new TLchannel_a(128, 32))
		val il1_chn_d = Flipped(new DecoupledIO( new TLchannel_d(128) ))

		val is_il1_fence_req = Input(Bool())

		val flush = Input(Bool())
	})

	val dw = 128
	val bk = 2
	val cb = 4
	val cl = 64

	val addr_lsb = log2Ceil(dw*bk/8)
	val line_w = log2Ceil(cl)
	val tag_w = 32 - addr_lsb - line_w

	val if_addr = Wire(UInt(64.W))
	val if_addr_reg = RegInit( "h80000000".U(64.W) )



	def addr_align_128 = if_addr & ~("b1111".U(64.W))
	def addr_align_256 = if_addr & ~("b11111".U(64.W))
	def addr_tag = if_addr(31, 32-tag_w)
	def cl_sel = if_addr(addr_lsb+line_w-1, addr_lsb)




	val il1_mst = new TileLink_mst(128, 32, 0)
	val mem = new Cache_mem( dw, 32, bk, cb, cl )
	val stateReg = RegInit(Il1_state.cfree)
	val addr_il1_req = RegInit( "h80000000".U(32.W) )
	val cache_valid = Reg( Vec( cl, Vec(cb, Bool()) ) )
	val random_res = LFSR(log2Ceil(cb), true.B )
	val is_cb_vhit = Wire(Vec(cb, Bool()))
	val trans_kill = RegInit(false.B)
	val is_il1_fence_req = RegInit(false.B)

	if_addr_reg := Mux( (stateReg === Il1_state.cfree), io.pc_if.bits.addr, if_addr_reg)
	if_addr     := Mux( (stateReg === Il1_state.cfree), io.pc_if.bits.addr, if_addr_reg)

	io.if_iq <> ibuf.io.deq
	ibuf.io.flush := io.flush

	def is_pc_if_ack = (io.pc_if.valid & io.pc_if.ready)

	ia.pc := if_addr
	ia.instr := Mux( 
		( (stateReg === Il1_state.cktag) & is_cb_vhit.contains(true.B) & ~io.flush),
		mem_dat,
		Mux(
			( stateReg === Il1_state.cmiss & (addr_align_128 === addr_il1_req) ),
			il1_mst.data_ack,
			DontCare
		)
	 )

	ibuf_valid_i := 
				( (stateReg === Il1_state.cktag) & is_cb_vhit.contains(true.B) ) |
				( stateReg === Il1_state.cmiss & il1_mst.is_chn_d_ack & (addr_align_128 === addr_il1_req) & ~trans_kill)



	io.pc_if.ready := ~trans_kill & (stateReg =/= Il1_state.cfree) & (stateDnxt === Il1_state.cfree)

	io.il1_chn_a.bits := il1_mst.a
	io.il1_chn_a.valid := il1_mst.a_valid
	il1_mst.a_ready    := io.il1_chn_a.ready

	io.il1_chn_d.ready := il1_mst.d_ready
	il1_mst.d          := io.il1_chn_d.bits
	il1_mst.d_valid    := io.il1_chn_d.valid



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


	stateReg := stateDnxt

	def il1_state_dnxt_in_cfree = 
		Mux( is_il1_fence_req, Il1_state.fence,
			Mux( ( (ibuf_ready_i) & ~io.flush), Il1_state.cktag, Il1_state.cfree) ) // no valid or valid is ack then next
	def il1_state_dnxt_in_cktag = 
		Mux( is_cb_vhit.contains(true.B), Il1_state.cfree, Il1_state.cmiss )
	def il1_state_dnxt_in_cmiss = 
		Mux( il1_mst.is_last_d_trans, Il1_state.cfree, Il1_state.cmiss )
	def il1_state_dnxt_in_fence = 
		Mux( ~is_il1_fence_req, Il1_state.cfree, Il1_state.fence )



	def stateDnxt =
		MuxCase( Il1_state.cfree, Array(
			( stateReg === Il1_state.cfree ) -> il1_state_dnxt_in_cfree,
			( stateReg === Il1_state.cktag ) -> il1_state_dnxt_in_cktag,
			( stateReg === Il1_state.cmiss ) -> il1_state_dnxt_in_cmiss,
			( stateReg === Il1_state.fence ) -> il1_state_dnxt_in_fence
		))











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
						(stateReg === Il1_state.cfree) -> addr_align_256,
						(stateReg === Il1_state.cktag) -> addr_align_256,
						(stateReg === Il1_state.cmiss) -> Mux( il1_mst.is_accessAckData, addr_il1_req + "b10000".U , addr_il1_req),
						(stateReg === Il1_state.fence) -> addr_align_256
						))
	}




	mem.cache_addr := Mux1H( Seq(
							(stateReg === Il1_state.cfree) -> addr_align_128,
							(stateReg === Il1_state.cktag) -> addr_align_128,
							(stateReg === Il1_state.cmiss) -> addr_il1_req,
							(stateReg === Il1_state.fence) -> addr_align_128
							))

	for ( i <- 0 until cb ) yield {
		mem.dat_en_w(i) := (stateReg === Il1_state.cmiss) & (il1_mst.is_accessAckData & is_cb_vhit(i))

		mem.dat_en_r(i) := (stateDnxt === Il1_state.cktag)

		mem.tag_en_w(i) := (stateReg === Il1_state.cktag) & ((stateDnxt === Il1_state.cmiss) & is_block_replace(i.U))

		mem.tag_en_r(i) :=
				((stateReg === Il1_state.cfree) & (stateDnxt === Il1_state.cktag)) |
				((stateReg === Il1_state.cmiss) & (il1_mst.is_chn_a_ack))


	}

	mem.dat_info_wstrb := "b1111111111111111".U
	mem.dat_info_w := il1_mst.data_ack




	when( reset.asBool ) {
		for ( i <- 0 until cl; j <- 0 until cb ) yield cache_valid(i)(j) := false.B
	}
	.elsewhen( stateReg === Il1_state.cktag & stateDnxt === Il1_state.cmiss ) {
		cache_valid(cl_sel)(replace_sel) := true.B
	}
	.elsewhen( stateReg === Il1_state.fence & stateDnxt === Il1_state.cfree ) {
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


	def mem_dat = {
		val cb_num = for ( i <- 0 until cb ) yield { is_cb_vhit(i) === true.B }
		val dat_sel = for ( i <- 0 until cb ) yield { mem.dat_info_r(i) }
		MuxCase( DontCare, cb_num zip dat_sel )	
	}

	
	


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


	il1_mst.d_ready := true.B

	when ( stateReg === Il1_state.cktag & stateDnxt === Il1_state.cmiss ) {
		il1_mst.op_getData(addr_align_256, 5.U)
		il1_mst.a_valid_set()
	}
	.elsewhen( stateReg === Il1_state.cmiss & il1_mst.is_chn_a_ack === true.B){
		il1_mst.a_valid_rst()
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




	when ( io.flush & stateDnxt === Il1_state.cmiss ) {
		trans_kill := true.B
	}
	.elsewhen( stateDnxt =/= Il1_state.cmiss ) {
		trans_kill := false.B
	}


	when ( io.is_il1_fence_req & ~is_il1_fence_req ) {
		is_il1_fence_req := true.B
	}
	.elsewhen( ~io.is_il1_fence_req & is_il1_fence_req ) {
		is_il1_fence_req := false.B
	}







}








