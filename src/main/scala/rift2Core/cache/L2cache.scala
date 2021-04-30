

/*
* @Author: Ruige Lee
* @Date:   2021-04-27 17:08:56
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-30 11:12:27
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
import base._


class L2Cache( dw:Int = 256, bk:Int = 4, cb:Int = 4, cl:Int = 32 ) extends Module {
	val io = IO( new Bundle{
		val il1_chn_a = Flipped( new DecoupledIO(new TLchannel_a(128, 32)) )
		val il1_chn_d = new DecoupledIO( new TLchannel_d(128) )

		val dl1_chn_a = Flipped(new DecoupledIO(new TLchannel_a(128, 32)))
		val dl1_chn_d = new DecoupledIO( new TLchannel_d(128) )

		val l2c_chn_a = new DecoupledIO(new TLchannel_a(128, 32))
		val l2c_chn_d = Flipped(new DecoupledIO( new TLchannel_d(128) )	)	


		val l2c_fence_req = Input(Bool())
	})

	def addr_lsb = log2Ceil(dw*bk/8)
	def line_w   = log2Ceil(cl)
	def tag_w    = 32 - addr_lsb - line_w

	val il1_slv = Module(new TileLink_slv_lite(128, 32))
	val dl1_slv = Module(new TileLink_slv_lite(128, 32))
	val l2c_mst = Module(new TileLink_mst_heavy(128, 32, 3))

	val cache_mem = new Cache_mem( dw, 32, bk, cb, cl )

	val cache_addr_dnxt = Wire( UInt(32.W) )
	val cache_addr_qout = RegInit(0.U(32.W))
	cache_addr_qout := cache_addr_dnxt

	val tag_addr_reg = RegInit(0.U(tag_w.W))


	val req_no = RegInit(0.U(3.W))

	// val tag_addr_sel = PMux( Seq(
	// 					(req_no === 1.U) -> il1_slv.io.a.bits.address,
	// 					(req_no === 2.U) -> dl1_slv.io.a.bits.address,
	// 					(req_no === 3.U) -> dl1_slv.io.a.bits.address,
	// 					(req_no === 4.U) -> dl1_slv.io.a.bits.address
	// 				))

	// tag_addr_reg := Mux( (fsm.state_qout === L2C_state.cfree) & (fsm.state_dnxt === L2C_state.cktag),
	// 					tag_addr_sel, tag_addr_reg
	// 				)

	val tag_addr = cache_addr_dnxt(31, 32-tag_w)


	cache_addr_dnxt := Mux1H( Seq(
		(fsm.state_qout === L2C_state.cfree) -> PMux( Seq(
														(req_no === 1.U) -> il1_slv.io.a.bits.address,
														(req_no === 2.U) -> dl1_slv.io.a.bits.address,
														(req_no === 3.U) -> dl1_slv.io.a.bits.address,
														(req_no === 4.U) -> dl1_slv.io.a.bits.address
													)),
		(fsm.state_qout === L2C_state.cktag) -> (	cache_addr_qout &
													Mux(
														fsm.state_dnxt === L2C_state.flash,
														Cat( Fill(32-addr_lsb, 1.U), Fill(addr_lsb, 0.U) ), Fill(32, 1.U)
													)
												),
		(fsm.state_qout === L2C_state.flash) -> Mux( l2c_mst.io.d.fire, cache_addr_qout + "b10000".U, cache_addr_qout ),
		(fsm.state_qout === L2C_state.rspir) -> Mux( l2c_mst.io.d.fire, cache_addr_qout + "b10000".U, cache_addr_qout ),
		(fsm.state_qout === L2C_state.rspdr) -> Mux( l2c_mst.io.d.fire, cache_addr_qout + "b10000".U, cache_addr_qout ),
		(fsm.state_qout === L2C_state.rspdw) -> cache_addr_qout,
		(fsm.state_qout === L2C_state.rspda) -> cache_addr_qout,
		(fsm.state_qout === L2C_state.fence) -> cache_addr_qout
	))


	object bram {
		val random_res = LFSR(log2Ceil(cb), true.B )

		val is_cb_vhit = Wire( Vec(cb, Bool()) )
		def is_block_replace(i: Int) = UIntToOH(replace_sel)(i).asBool

		val cache_valid = RegInit( VecInit( Seq.fill(cl)( VecInit( Seq.fill(cb)(false.B ) ) ) ))
		val cl_sel = cache_addr_dnxt(addr_lsb+line_w-1, addr_lsb)
		val replace_sel = 
			Mux(
				cache_valid(cl_sel).contains(false.B),
				cache_valid(cl_sel).indexWhere((a: Bool) => (a === false.B)),
				random_res
			)



		for ( i <- 0 until cb ) yield {
			cache_mem.dat_en_w(i) := 
				is_cb_vhit(i) & l2c_mst.io.d.fire & ( fsm.state_qout === L2C_state.flash | fsm.state_qout === L2C_state.rspdw )

			cache_mem.dat_en_r(i) := 
				is_cb_vhit(i) & ( fsm.state_dnxt === L2C_state.rspir | fsm.state_dnxt === L2C_state.rspdr )
		}

		cache_mem.dat_info_wstrb := 
			PMux( Seq(
				(fsm.state_qout === L2C_state.flash) -> "hffff".U,
				(fsm.state_qout === L2C_state.rspdw) -> dl1_slv.io.a.bits.mask
			))

		cache_mem.dat_info_w :=
			PMux( Seq(
				(fsm.state_qout === L2C_state.flash) -> l2c_mst.io.d.bits.data,
				(fsm.state_qout === L2C_state.rspdw) -> dl1_slv.io.a.bits.data
			))



		for ( i <- 0 until cb ) yield {
			cache_mem.tag_en_w(i) := 
				is_block_replace(i) & (fsm.state_qout === L2C_state.cktag) & ( fsm.state_dnxt === L2C_state.flash ) 
		
			cache_mem.tag_en_r(i) :=
				( fsm.state_dnxt === L2C_state.cktag ) |
				l2c_mst.io.a.fire

		}


		cache_mem.cache_addr := cache_addr_dnxt

		when( fsm.state_qout === L2C_state.cktag & fsm.state_dnxt === L2C_state.flash ) {
			cache_valid(cl_sel)(replace_sel) := true.B
		}
		.elsewhen( fsm.state_qout === L2C_state.fence & fsm.state_dnxt === L2C_state.cfree ) {
			for ( i <- 0 until cl; j <- 0 until cb ) yield cache_valid(i)(j) := false.B
		}
		.elsewhen( fsm.state_qout === L2C_state.rspda & fsm.state_dnxt === L2C_state.cfree ) {
			for ( i <- 0 until cb ) yield {
				cache_valid(cl_sel)(i) := Mux( is_cb_vhit(i), false.B, cache_valid(cl_sel)(i) )
			}
		}
		




		for ( i <- 0 until cb ) yield {
			is_cb_vhit(i) := cache_valid(cl_sel)(i) & ( cache_mem.tag_info_r(i) === tag_addr  )
		}

		val mem_dat = PMux( is_cb_vhit zip cache_mem.dat_info_r )


	}


	object L2C_state extends ChiselEnum {
		val cfree, cktag, flash, rspir, rspdr, rspdw, rspda, fence = Value
	}

	object fsm {
		val state_qout = RegInit( L2C_state.cfree )

		val l2c_state_dnxt_in_cfree = 
			Mux( io.l2c_fence_req, L2C_state.fence,
				Mux( (il1_slv.io.a.valid | dl1_slv.io.a.valid), L2C_state.cktag, L2C_state.cfree )
			)


		val l2c_state_dnxt_in_cktag = Mux1H( Seq(
			(req_no === 1.U) -> ( Mux( bram.is_cb_vhit.contains(true.B), L2C_state.rspir, L2C_state.flash )  ),
			(req_no === 2.U) -> ( Mux( bram.is_cb_vhit.contains(true.B), L2C_state.rspdr, L2C_state.flash )  ),
			(req_no === 3.U) -> ( L2C_state.rspdw ),
			(req_no === 4.U) -> ( L2C_state.rspda )
		) )

		val l2c_state_dnxt_in_flash = Mux( l2c_mst.io.mode === 7.U, L2C_state.cktag, L2C_state.flash )
		val l2c_state_dnxt_in_rspir = Mux( il1_slv.io.mode === 7.U, L2C_state.cfree, L2C_state.rspir )
		val l2c_state_dnxt_in_rspdr = Mux( dl1_slv.io.mode === 7.U, L2C_state.cfree, L2C_state.rspdr )
		val l2c_state_dnxt_in_rspda = Mux( dl1_slv.io.mode === 7.U, L2C_state.cfree, L2C_state.rspda )
		val l2c_state_dnxt_in_rspdw = Mux( dl1_slv.io.mode === 7.U, L2C_state.cfree, L2C_state.rspdw )
		val l2c_state_dnxt_in_fence = L2C_state.cfree

		val state_dnxt = Mux1H( Seq(
			(state_qout === L2C_state.cfree) -> l2c_state_dnxt_in_cfree,
			(state_qout === L2C_state.cktag) -> l2c_state_dnxt_in_cktag,
			(state_qout === L2C_state.flash) -> l2c_state_dnxt_in_flash,
			(state_qout === L2C_state.rspir) -> l2c_state_dnxt_in_rspir,
			(state_qout === L2C_state.rspdr) -> l2c_state_dnxt_in_rspdr,
			(state_qout === L2C_state.rspdw) -> l2c_state_dnxt_in_rspdw,
			(state_qout === L2C_state.rspda) -> l2c_state_dnxt_in_rspda,
			(state_qout === L2C_state.fence) -> l2c_state_dnxt_in_fence
		))

		
		state_qout := state_dnxt
	}

	object bus {
		req_no :=
			Mux( fsm.state_qout === L2C_state.cfree & fsm.state_dnxt === L2C_state.cktag,
				MuxCase( 0.U, Array(
					(il1_slv.io.a.valid)                                            -> 1.U,
					(dl1_slv.io.a.valid &  dl1_slv.io.a.bits.opcode === dl1_slv.Get)         -> 2.U,
					(dl1_slv.io.a.valid &  dl1_slv.io.a.bits.opcode === dl1_slv.PutFullData) -> 3.U,
					(dl1_slv.io.a.valid & (dl1_slv.io.a.bits.opcode === dl1_slv.ArithmeticData |
										   dl1_slv.io.a.bits.opcode === dl1_slv.LogicalData) ) -> 4.U,
				)),
				req_no
			)

		il1_slv.io.is_rsp :=
						( fsm.state_qout === L2C_state.cktag & fsm.state_dnxt === L2C_state.rspir)

		dl1_slv.io.is_rsp :=
						( fsm.state_qout === L2C_state.cktag & fsm.state_dnxt === L2C_state.rspdr ) |
						( fsm.state_qout === L2C_state.rspdw | fsm.state_qout === L2C_state.rspda )	

		l2c_mst.io.is_req := 	
							( fsm.state_qout === L2C_state.cktag ) &
								(
									fsm.state_dnxt === L2C_state.flash |
									fsm.state_dnxt === L2C_state.rspdw |
									fsm.state_dnxt === L2C_state.rspda
								)

		il1_slv.io.d.bits.data := bram.mem_dat


		dl1_slv.io.d.bits.data := MuxCase( DontCare, Array(
			(fsm.state_qout === L2C_state.rspdw) -> DontCare,
			(fsm.state_qout === L2C_state.rspdr) -> bram.mem_dat,
			(fsm.state_qout === L2C_state.rspda) -> l2c_mst.io.d.bits.data
		))


		l2c_mst.io.a.bits.opcode :=
								RegEnable( PMux( Seq(
											( fsm.state_dnxt === L2C_state.flash ) -> l2c_mst.Get,
											( fsm.state_dnxt === L2C_state.rspdw ) -> dl1_slv.io.a.bits.opcode,
											( fsm.state_dnxt === L2C_state.rspda ) -> dl1_slv.io.a.bits.opcode
										)),
										0.U(3.W),
										( fsm.state_qout === L2C_state.cktag )
								)

		l2c_mst.io.a.bits.param   := 
								RegEnable( PMux( Seq(
											( fsm.state_dnxt === L2C_state.flash ) -> 0.U,
											( fsm.state_dnxt === L2C_state.rspdw ) -> dl1_slv.io.a.bits.param,
											( fsm.state_dnxt === L2C_state.rspda ) -> dl1_slv.io.a.bits.param
										)),
										0.U(2.W),
										( fsm.state_qout === L2C_state.cktag )
								)

		l2c_mst.io.a.bits.size    := 
								RegEnable( PMux( Seq(
											( fsm.state_dnxt === L2C_state.flash ) -> 7.U, //(1024bits)
											( fsm.state_dnxt === L2C_state.rspdw ) -> dl1_slv.io.a.bits.size,
											( fsm.state_dnxt === L2C_state.rspda ) -> dl1_slv.io.a.bits.size
										)),
										0.U(8.W),
										( fsm.state_qout === L2C_state.cktag )
								)

		l2c_mst.io.a.bits.source  := 
								RegEnable( PMux( Seq(
											( fsm.state_dnxt === L2C_state.flash ) -> 3.U,
											( fsm.state_dnxt === L2C_state.rspdw ) -> 1.U,
											( fsm.state_dnxt === L2C_state.rspda ) -> 1.U
										)),
										0.U(8.W),
										( fsm.state_qout === L2C_state.cktag )
								)

		l2c_mst.io.a.bits.address := 
								RegEnable( PMux( Seq(
											( fsm.state_dnxt === L2C_state.flash ) -> (cache_addr_dnxt & ~("b11111111".U(32.W))),
											( fsm.state_dnxt === L2C_state.rspdw ) -> dl1_slv.io.a.bits.address,
											( fsm.state_dnxt === L2C_state.rspda ) -> dl1_slv.io.a.bits.address
										)),
										0.U(8.W),
										( fsm.state_qout === L2C_state.cktag )
								)

		l2c_mst.io.a.bits.mask    := 
								RegEnable( PMux( Seq(
											( fsm.state_dnxt === L2C_state.flash ) -> 0.U,
											( fsm.state_dnxt === L2C_state.rspdw ) -> dl1_slv.io.a.bits.mask,
											( fsm.state_dnxt === L2C_state.rspda ) -> dl1_slv.io.a.bits.mask
										)),
										0.U(8.W),
										( fsm.state_qout === L2C_state.cktag )
								)

		l2c_mst.io.a.bits.data    := 
								RegEnable( PMux( Seq(
											( fsm.state_dnxt === L2C_state.flash ) -> 0.U,
											( fsm.state_dnxt === L2C_state.rspdw ) -> dl1_slv.io.a.bits.data,
											( fsm.state_dnxt === L2C_state.rspda ) -> dl1_slv.io.a.bits.data
										)),
										0.U(8.W),
										( fsm.state_qout === L2C_state.cktag )
								)

	}


}

