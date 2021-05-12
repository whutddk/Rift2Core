/*
* @Author: Ruige Lee
* @Date:   2021-04-30 10:39:01
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-30 10:39:11
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

package rift2Core.cache

import chisel3._
import chisel3.util._
import chisel3.util.random._
import chisel3.experimental.ChiselEnum

import tilelink._
import axi._
import base._


class L3Cache ( dw:Int = 1024, bk:Int = 4, cl:Int = 256 ) extends Module {
	val io = IO(new Bundle{

		val l2c_chn_a = Flipped(new DecoupledIO(new TLchannel_a(128, 32)))
		val l2c_chn_d = new DecoupledIO( new TLchannel_d(128))
		
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
	val cache_mem = new Cache_mem( dw, 32, bk, 1, cl )

	object L3C_state extends ChiselEnum {
		val cfree, cktag, evict, flash, rspl2, fence = Value
	}

	val fsm = new Bundle {
		val state_qout = RegInit( L3C_state.cfree )
		val state_dnxt = WireDefault( L3C_state.cfree )		
	}

	val bram = new Bundle {
		val cache_addr_dnxt = Wire(UInt(32.W))
		val cache_addr_qout = RegInit(0.U(32.W))
		val cache_valid = RegInit(VecInit(Seq.fill(cl)(false.B)))
		val mem_dat = Wire( UInt(128.W) )

		val tag_addr = Wire( UInt(32.W) )
		val tag_info = tag_addr(31, 32-tag_w)
		val is_cb_hit = Wire(Bool())

		val cl_sel = Wire( UInt( line_w.W ) )
	}

	val db = new Bundle {
		val buf = RegInit( VecInit(Seq.fill(16)(0.U(tag_w.W))) )
		val valid = RegInit( VecInit(Seq.fill(16)(false.B)) )

		val is_full  = Wire(Bool())
		val is_empty = Wire(Bool())

		val idx_enq = Wire( UInt(4.W) )
		val idx_deq = Wire( UInt(4.W) )

		val is_hazard = Wire(Bool())

		val addr_o = Wire(UInt(tag_w.W))

		val is_fence = RegInit(false.B)
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




	val l3c_state_dnxt_in_cfree = 
		Mux( db.is_fence, L3C_state.fence, Mux( l2c_slv.io.a.valid, L3C_state.cktag, L3C_state.cfree ) )

	val l3c_state_dnxt_in_cktag = 
		Mux( ~bram.cache_valid(bram.cl_sel), L3C_state.flash,
			Mux( ~bram.is_cb_hit, L3C_state.evict,
				Mux( (l2c_slv.io.a.bits.opcode =/= l2c_slv.Get & db.is_full), L3C_state.evict, L3C_state.rspl2  )))

	val l3c_state_dnxt_in_evict = 
		Mux( ~mem_mst_w.io.end, L3C_state.evict,
			Mux( db.is_fence, L3C_state.fence, L3C_state.cktag ))

	val l3c_state_dnxt_in_flash = 
		Mux( mem_mst_r.io.end, L3C_state.cktag, L3C_state.flash )

	val l3c_state_dnxt_in_rspl2 = 
		Mux( l2c_slv.io.mode === 7.U, L3C_state.cfree, L3C_state.rspl2 )

	val l3c_state_dnxt_in_fence = 
		Mux( db.is_empty, L3C_state.cfree, L3C_state.evict )

	fsm.state_dnxt := MuxCase(L3C_state.cfree, Array(
		(fsm.state_qout === L3C_state.cfree) -> l3c_state_dnxt_in_cfree,
		(fsm.state_qout === L3C_state.cktag) -> l3c_state_dnxt_in_cktag,
		(fsm.state_qout === L3C_state.flash) -> l3c_state_dnxt_in_flash,
		(fsm.state_qout === L3C_state.evict) -> l3c_state_dnxt_in_evict,
		(fsm.state_qout === L3C_state.rspl2) -> l3c_state_dnxt_in_rspl2,
		(fsm.state_qout === L3C_state.fence) -> l3c_state_dnxt_in_fence
	))

	fsm.state_qout := fsm.state_dnxt


//                AAA               MMMMMMMM               MMMMMMMM     OOOOOOOOO     
//               A:::A              M:::::::M             M:::::::M   OO:::::::::OO   
//              A:::::A             M::::::::M           M::::::::M OO:::::::::::::OO 
//             A:::::::A            M:::::::::M         M:::::::::MO:::::::OOO:::::::O
//            A:::::::::A           M::::::::::M       M::::::::::MO::::::O   O::::::O
//           A:::::A:::::A          M:::::::::::M     M:::::::::::MO:::::O     O:::::O
//          A:::::A A:::::A         M:::::::M::::M   M::::M:::::::MO:::::O     O:::::O
//         A:::::A   A:::::A        M::::::M M::::M M::::M M::::::MO:::::O     O:::::O
//        A:::::A     A:::::A       M::::::M  M::::M::::M  M::::::MO:::::O     O:::::O
//       A:::::AAAAAAAAA:::::A      M::::::M   M:::::::M   M::::::MO:::::O     O:::::O
//      A:::::::::::::::::::::A     M::::::M    M:::::M    M::::::MO:::::O     O:::::O
//     A:::::AAAAAAAAAAAAA:::::A    M::::::M     MMMMM     M::::::MO::::::O   O::::::O
//    A:::::A             A:::::A   M::::::M               M::::::MO:::::::OOO:::::::O
//   A:::::A               A:::::A  M::::::M               M::::::M OO:::::::::::::OO 
//  A:::::A                 A:::::A M::::::M               M::::::M   OO:::::::::OO   
// AAAAAAA                   AAAAAAAMMMMMMMM               MMMMMMMM     OOOOOOOOO    

	val amo = new Bundle {
		val res = Wire(UInt(128.W))

		val op32_1 = Wire( UInt(32.W) )
		val op32_2 = Wire( UInt(32.W) )
		val res32 = Wire( UInt(32.W) )

		val op64_1 = Wire( UInt(64.W) )
		val op64_2 = Wire( UInt(64.W) )
		val res64 = Wire( UInt(64.W) )	

		val min_res  = Mux( l2c_slv.io.a.bits.data.asSInt > bram.mem_dat.asSInt, bram.mem_dat, l2c_slv.io.a.bits.data )
		val max_res  = Mux( l2c_slv.io.a.bits.data.asSInt > bram.mem_dat.asSInt, l2c_slv.io.a.bits.data, bram.mem_dat )
		val minu_rex = Mux( l2c_slv.io.a.bits.data > bram.mem_dat, bram.mem_dat, l2c_slv.io.a.bits.data )
		val maxu_res = Mux( l2c_slv.io.a.bits.data > bram.mem_dat, l2c_slv.io.a.bits.data, bram.mem_dat )
		val add_res  = l2c_slv.io.a.bits.data + bram.mem_dat
		val xor_res  = bram.mem_dat ^ l2c_slv.io.a.bits.data
		val or_res   = bram.mem_dat | l2c_slv.io.a.bits.data
		val and_res  = bram.mem_dat & l2c_slv.io.a.bits.data
		val swap_res = l2c_slv.io.a.bits.data
	}




	amo.op32_1 := Mux1H(Seq(
		( l2c_slv.io.a.bits.mask === "h000f".U ) -> l2c_slv.io.a.bits.data(31,0),
		( l2c_slv.io.a.bits.mask === "h00f0".U ) -> l2c_slv.io.a.bits.data(63,32),
		( l2c_slv.io.a.bits.mask === "h0f00".U ) -> l2c_slv.io.a.bits.data(95,64),
		( l2c_slv.io.a.bits.mask === "hf000".U ) -> l2c_slv.io.a.bits.data(127,96),

	))

	amo.op32_2 := Mux1H(Seq(
		( l2c_slv.io.a.bits.mask === "h000f".U ) -> bram.mem_dat(31,0),
		( l2c_slv.io.a.bits.mask === "h00f0".U ) -> bram.mem_dat(63,32),
		( l2c_slv.io.a.bits.mask === "h0f00".U ) -> bram.mem_dat(95,64),
		( l2c_slv.io.a.bits.mask === "hf000".U ) -> bram.mem_dat(127,96),
	))

	amo.op64_1 := Mux1H(Seq(
		( l2c_slv.io.a.bits.mask === "h00ff".U ) -> l2c_slv.io.a.bits.data(63,0),
		( l2c_slv.io.a.bits.mask === "hff00".U ) -> l2c_slv.io.a.bits.data(127,64)
	))

	amo.op64_2 := Mux1H(Seq(
		( l2c_slv.io.a.bits.mask === "h00ff".U ) -> bram.mem_dat(63,0),
		( l2c_slv.io.a.bits.mask === "hff00".U ) -> bram.mem_dat(127,64)
	))

	amo.res32 :=
		Mux1H( Seq(
			(l2c_slv.io.a.bits.opcode === l2c_slv.ArithmeticData) -> 
				Mux1H(Seq(
					(l2c_slv.io.a.bits.param === l2c_slv.MIN)  -> Mux( amo.op32_1.asSInt > amo.op32_2.asSInt, amo.op32_2, amo.op32_1 ),
					(l2c_slv.io.a.bits.param === l2c_slv.MAX)  -> Mux( amo.op32_1.asSInt > amo.op32_2.asSInt, amo.op32_1, amo.op32_2 ),
					(l2c_slv.io.a.bits.param === l2c_slv.MINU) -> Mux( amo.op32_1        > amo.op32_2,        amo.op32_2, amo.op32_1 ),
					(l2c_slv.io.a.bits.param === l2c_slv.MAXU) -> Mux( amo.op32_1        > amo.op32_2,        amo.op32_1, amo.op32_2 ),
					(l2c_slv.io.a.bits.param === l2c_slv.ADD)  -> (amo.op32_1 + amo.op32_2)
				)),
			(l2c_slv.io.a.bits.opcode === l2c_slv.LogicalData) ->
				Mux1H(Seq(
					(l2c_slv.io.a.bits.param === l2c_slv.XOR)  -> ( amo.op32_1 ^ amo.op32_2 ),
					(l2c_slv.io.a.bits.param === l2c_slv.OR)   -> ( amo.op32_1 | amo.op32_2 ),
					(l2c_slv.io.a.bits.param === l2c_slv.AND)  -> ( amo.op32_1 & amo.op32_2 ),
					(l2c_slv.io.a.bits.param === l2c_slv.SWAP) -> ( amo.op32_1 ),							
				))
		))

	amo.res64 :=
		Mux1H( Seq(
			(l2c_slv.io.a.bits.opcode === l2c_slv.ArithmeticData) -> 
				Mux1H(Seq(
					(l2c_slv.io.a.bits.param === l2c_slv.MIN)  -> Mux( amo.op64_1.asSInt > amo.op64_2.asSInt, amo.op64_2, amo.op64_1 ),
					(l2c_slv.io.a.bits.param === l2c_slv.MAX)  -> Mux( amo.op64_1.asSInt > amo.op64_2.asSInt, amo.op64_1, amo.op64_2 ),
					(l2c_slv.io.a.bits.param === l2c_slv.MINU) -> Mux( amo.op64_1        > amo.op64_2,        amo.op64_2, amo.op64_1 ),
					(l2c_slv.io.a.bits.param === l2c_slv.MAXU) -> Mux( amo.op64_1        > amo.op64_2,        amo.op64_1, amo.op64_2 ),
					(l2c_slv.io.a.bits.param === l2c_slv.ADD)  -> (amo.op64_1 + amo.op64_2)
				)),
			(l2c_slv.io.a.bits.opcode === l2c_slv.LogicalData) ->
				Mux1H(Seq(
					(l2c_slv.io.a.bits.param === l2c_slv.XOR)  -> ( amo.op64_1 ^ amo.op64_2 ),
					(l2c_slv.io.a.bits.param === l2c_slv.OR)   -> ( amo.op64_1 | amo.op64_2 ),
					(l2c_slv.io.a.bits.param === l2c_slv.AND)  -> ( amo.op64_1 & amo.op64_2 ),
					(l2c_slv.io.a.bits.param === l2c_slv.SWAP) -> ( amo.op64_1 ),							
				))
		))

	amo.res :=
		Mux1H(Seq(
			( l2c_slv.io.a.bits.mask === "h000f".U ) -> Cat( 0.U, amo.res32),
			( l2c_slv.io.a.bits.mask === "h00f0".U ) -> Cat( amo.res32, 0.U(32.W) ),
			( l2c_slv.io.a.bits.mask === "h0f00".U ) -> Cat( amo.res32, 0.U(64.W) ),
			( l2c_slv.io.a.bits.mask === "hf000".U ) -> Cat( amo.res32, 0.U(96.W) ),
			( l2c_slv.io.a.bits.mask === "h00ff".U ) -> Cat( 0.U, amo.res64),
			( l2c_slv.io.a.bits.mask === "hff00".U ) -> Cat( amo.res64, 0.U(64.W) )
	))


	assert( ~(
				l2c_slv.io.a.fire &
				( l2c_slv.io.a.bits.opcode === l2c_slv.ArithmeticData | l2c_slv.io.a.bits.opcode === l2c_slv.LogicalData ) & (
					l2c_slv.io.a.bits.mask =/= "h000f".U |
					l2c_slv.io.a.bits.mask =/= "h00f0".U |
					l2c_slv.io.a.bits.mask =/= "h0f00".U |
					l2c_slv.io.a.bits.mask =/= "hf000".U |
					l2c_slv.io.a.bits.mask =/= "h00ff".U |
					l2c_slv.io.a.bits.mask =/= "hff00".U
				)
			), "Assert Fail at L3 cache, invalid amo mask"	
	)

	val min_res  = Mux( l2c_slv.io.a.bits.data.asSInt > bram.mem_dat.asSInt, bram.mem_dat, l2c_slv.io.a.bits.data )
	val max_res  = Mux( l2c_slv.io.a.bits.data.asSInt > bram.mem_dat.asSInt, l2c_slv.io.a.bits.data, bram.mem_dat )
	val minu_rex = Mux( l2c_slv.io.a.bits.data > bram.mem_dat, bram.mem_dat, l2c_slv.io.a.bits.data )
	val maxu_res = Mux( l2c_slv.io.a.bits.data > bram.mem_dat, l2c_slv.io.a.bits.data, bram.mem_dat )
	val add_res  = l2c_slv.io.a.bits.data + bram.mem_dat
	val xor_res  = bram.mem_dat ^ l2c_slv.io.a.bits.data
	val or_res   = bram.mem_dat | l2c_slv.io.a.bits.data
	val and_res  = bram.mem_dat & l2c_slv.io.a.bits.data
	val swap_res = l2c_slv.io.a.bits.data











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

	bram.mem_dat := cache_mem.dat_info_r(0)
	bram.cache_addr_qout := bram.cache_addr_dnxt

	bram.cache_addr_dnxt := Mux1H(Seq(
		(fsm.state_qout === L3C_state.cfree) -> l2c_slv.io.a.bits.address,
		(fsm.state_qout === L3C_state.cktag) -> 
			Mux1H( Seq(
				( fsm.state_dnxt === L3C_state.evict ) -> Cat(db.addr_o , 0.U(addr_lsb.W)),
				( fsm.state_dnxt === L3C_state.flash ) -> (l2c_slv.io.a.bits.address & Cat( Fill(32-addr_lsb, 1.U), 0.U(addr_lsb.W) )),
				( fsm.state_dnxt === L3C_state.rspl2 ) -> l2c_slv.io.a.bits.address
			)),



			// Mux( ~bram.cache_valid(bram.cl_sel), ,
			// 	Mux( ~bram.is_cb_hit, 
			// 		Mux( (l2c_slv.io.a.bits.opcode =/= l2c_slv.Get & db.is_full), Cat(db.addr_o , 0.U(addr_lsb.W)),   ))),
		(fsm.state_qout === L3C_state.evict) -> Mux( mem_mst_w.io.w.fire, bram.cache_addr_qout + "b10000".U, bram.cache_addr_qout ),
		(fsm.state_qout === L3C_state.flash) -> Mux( mem_mst_r.io.r.fire, bram.cache_addr_qout + "b10000".U, bram.cache_addr_qout ),
		(fsm.state_qout === L3C_state.rspl2) -> Mux( l2c_slv.io.d.fire,bram.cache_addr_qout + "b10000".U, bram.cache_addr_qout ),
		(fsm.state_qout === L3C_state.fence) -> Cat(db.addr_o , 0.U(addr_lsb.W))

	))


	cache_mem.cache_addr := bram.cache_addr_qout

	cache_mem.dat_en_w(0) :=
		( fsm.state_qout === L3C_state.flash & mem_mst_r.io.r.fire ) |
		( fsm.state_qout === L3C_state.rspl2 & l2c_slv.io.a.fire &
			(l2c_slv.io.a.bits.opcode === l2c_slv.PutFullData |
			l2c_slv.io.a.bits.opcode === l2c_slv.ArithmeticData |
			l2c_slv.io.a.bits.opcode === l2c_slv.LogicalData
			)
		)

	cache_mem.dat_en_r(0) :=
		(fsm.state_dnxt === L3C_state.evict) |
		(fsm.state_dnxt === L3C_state.rspl2 & l2c_slv.io.a.bits.opcode =/= l2c_slv.PutFullData)


	cache_mem.dat_info_wstrb :=
		MuxCase( DontCare, Array(
			( fsm.state_qout === L3C_state.flash ) -> Fill(16, 1.U),
			( fsm.state_qout === L3C_state.rspl2 ) -> l2c_slv.io.a.bits.mask
		))




	cache_mem.dat_info_w :=
		MuxCase( DontCare, Array(
			( fsm.state_qout === L3C_state.flash ) -> mem_mst_r.io.r.bits.data,
			( fsm.state_qout === L3C_state.rspl2 ) -> 
				PMux( Seq(
					(l2c_slv.io.a.bits.opcode === l2c_slv.PutFullData) -> l2c_slv.io.a.bits.data,
					(l2c_slv.io.a.bits.opcode === l2c_slv.ArithmeticData) -> amo.res,
					(l2c_slv.io.a.bits.opcode === l2c_slv.LogicalData) -> amo.res
				))
			
		))

	cache_mem.tag_en_w(0) :=
		(fsm.state_qout === L3C_state.cktag) & (fsm.state_dnxt === L3C_state.flash)

	cache_mem.tag_en_r(0) :=
		(fsm.state_dnxt === L3C_state.cktag)



	bram.tag_addr := l2c_slv.io.a.bits.address
	bram.is_cb_hit := cache_mem.tag_info_r(0) === bram.tag_info
	bram.cl_sel := Mux( fsm.state_qout === L3C_state.evict, db.addr_o(line_w-1,0), l2c_slv.io.a.bits.address(addr_lsb+line_w-1, addr_lsb) )
	
	// bram.cache_addr_qout(addr_lsb+line_w-1, addr_lsb)

	for ( i <- 0 until cl ) yield {
		bram.cache_valid(i) :=
			Mux(
				(fsm.state_qout === L3C_state.fence) & (fsm.state_dnxt === L3C_state.cfree),
				false.B,
				Mux(
					i.U =/= bram.cl_sel,
					bram.cache_valid(i),
					MuxCase( bram.cache_valid(i), Array(
						(fsm.state_qout === L3C_state.evict) -> false.B,
						(fsm.state_qout === L3C_state.flash) -> true.B
					))
				)
		)
	}






// DDDDDDDDDDDDD          iiii                              tttt                                    BBBBBBBBBBBBBBBBB   lllllll                                      kkkkkkkk           
// D::::::::::::DDD      i::::i                          ttt:::t                                    B::::::::::::::::B  l:::::l                                      k::::::k           
// D:::::::::::::::DD     iiii                           t:::::t                                    B::::::BBBBBB:::::B l:::::l                                      k::::::k           
// DDD:::::DDDDD:::::D                                   t:::::t                                    BB:::::B     B:::::Bl:::::l                                      k::::::k           
//   D:::::D    D:::::D iiiiiiirrrrr   rrrrrrrrr   ttttttt:::::tttttttyyyyyyy           yyyyyyy       B::::B     B:::::B l::::l    ooooooooooo       cccccccccccccccc k:::::k    kkkkkkk
//   D:::::D     D:::::Di:::::ir::::rrr:::::::::r  t:::::::::::::::::t y:::::y         y:::::y        B::::B     B:::::B l::::l  oo:::::::::::oo   cc:::::::::::::::c k:::::k   k:::::k 
//   D:::::D     D:::::D i::::ir:::::::::::::::::r t:::::::::::::::::t  y:::::y       y:::::y         B::::BBBBBB:::::B  l::::l o:::::::::::::::o c:::::::::::::::::c k:::::k  k:::::k  
//   D:::::D     D:::::D i::::irr::::::rrrrr::::::rtttttt:::::::tttttt   y:::::y     y:::::y          B:::::::::::::BB   l::::l o:::::ooooo:::::oc:::::::cccccc:::::c k:::::k k:::::k   
//   D:::::D     D:::::D i::::i r:::::r     r:::::r      t:::::t          y:::::y   y:::::y           B::::BBBBBB:::::B  l::::l o::::o     o::::oc::::::c     ccccccc k::::::k:::::k    
//   D:::::D     D:::::D i::::i r:::::r     rrrrrrr      t:::::t           y:::::y y:::::y            B::::B     B:::::B l::::l o::::o     o::::oc:::::c              k:::::::::::k     
//   D:::::D     D:::::D i::::i r:::::r                  t:::::t            y:::::y:::::y             B::::B     B:::::B l::::l o::::o     o::::oc:::::c              k:::::::::::k     
//   D:::::D    D:::::D  i::::i r:::::r                  t:::::t    tttttt   y:::::::::y              B::::B     B:::::B l::::l o::::o     o::::oc::::::c     ccccccc k::::::k:::::k    
// DDD:::::DDDDD:::::D  i::::::ir:::::r                  t::::::tttt:::::t    y:::::::y             BB:::::BBBBBB::::::Bl::::::lo:::::ooooo:::::oc:::::::cccccc:::::ck::::::k k:::::k   
// D:::::::::::::::DD   i::::::ir:::::r                  tt::::::::::::::t     y:::::y              B:::::::::::::::::B l::::::lo:::::::::::::::o c:::::::::::::::::ck::::::k  k:::::k  
// D::::::::::::DDD     i::::::ir:::::r                    tt:::::::::::tt    y:::::y               B::::::::::::::::B  l::::::l oo:::::::::::oo   cc:::::::::::::::ck::::::k   k:::::k 
// DDDDDDDDDDDDD        iiiiiiiirrrrrrr                      ttttttttttt     y:::::y                BBBBBBBBBBBBBBBBB   llllllll   ooooooooooo       cccccccccccccccckkkkkkkk    kkkkkkk
//                                                                          y:::::y                                                                                                     
//                                                                         y:::::y                                                                                                      
//                                                                        y:::::y                                                                                                       
//                                                                       y:::::y                                                                                                        
//                                                                      yyyyyyy 
	

 


	db.is_full  := ( db.valid.forall((x:Bool) => (x === true.B)) )
	db.is_empty := ( db.valid.forall((x:Bool) => (x === false.B)) )

	db.idx_enq := ( db.valid.indexWhere((x:Bool) => (x === false.B)) )
	db.idx_deq := ( db.valid.lastIndexWhere((x:Bool) => (x === true.B)) )

	db.is_hazard := db.buf.contains(bram.cache_addr_dnxt(31,32-tag_w))
	
	when(
		~db.is_hazard & 
		fsm.state_qout === L3C_state.cktag & fsm.state_dnxt === L3C_state.rspl2 &
		(l2c_slv.io.a.bits.opcode === l2c_slv.PutFullData | l2c_slv.io.a.bits.opcode === l2c_slv.ArithmeticData | l2c_slv.io.a.bits.opcode === l2c_slv.LogicalData)
	){
		db.buf(db.idx_enq)   := bram.cache_addr_dnxt(31,32-tag_w)
		db.valid(db.idx_enq) := true.B
	}
	.elsewhen( fsm.state_qout === L3C_state.evict & fsm.state_dnxt =/= L3C_state.evict ){
		db.valid(db.idx_deq) := false.B
	}

	db.addr_o := db.buf(db.idx_deq)




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

