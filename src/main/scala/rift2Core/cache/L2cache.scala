

/*
* @Author: Ruige Lee
* @Date:   2021-04-27 17:08:56
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-27 19:49:44
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

import tilelink._
import base._


class L2cache( dw:Int = 256, bk:Int = 4, cb:Int = 4, cl:Int = 32 ) extends Module {
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

	val il1_slv = new TileLink_slv(128, 32)
	val dl1_slv = new TileLink_slv(128, 32)
	val l2c_mst = new TileLink_mst(128, 32, 3)

	val cache_mem = new Cache_mem( dw, 32, bk, cb, cl )

	val cache_addr = RegInit(0.U(32.W))


	val tag_addr_reg = RegInit(0.U(tag_w.W))


	val req_no = RegInit()

	val tag_addr_sel = PMux( Seq(
						(req_no === 1.U) -> il1_slv.a.address,
						(req_no === 2.U) -> dl1_slv.a.address,
						(req_no === 3.U) -> dl1_slv.a.address,
						(req_no === 4.U) -> dl1_slv.a.address
					))

	tag_addr_reg := Mux( (fsm.state === L2C_state.cfree) & (fsm.stateDnxt === L2C_state.cktag),
						tag_addr_sel, tag_addr_reg
					)

	val tag_addr = Mux( (fsm.state === L2C_state.cfree), tag_addr_sel(31, 32-tag_w), tag_addr_reg(31, 32-tag_w) )

	cache_addr := Mux1H( Seq(
		(fsm.state === L2C_state.cfree) -> cache_addr,
		(fsm.state === L2C_state.cktag) -> (
			Mux(fsm.stateDnxt === L2C_state.flash, Cat( Fill(32-addr_lsb, 1.U), Fill(addr_lsb, 0.U) ), Fill(32, 1.U)) & 
			PMux( Seq(
						(req_no === 1.U) -> il1_slv.a.address,
						(req_no === 2.U) -> dl1_slv.a.address,
						(req_no === 3.U) -> dl1_slv.a.address,
						(req_no === 4.U) -> dl1_slv.a.address
					))),
		(fsm.state === L2C_state.flash) -> Mux( l2c_mst.is_chn_d_ack, cache_addr + "b10000".U, cache_addr ),
		(fsm.state === L2C_state.rspir) -> Mux( l2c_mst.is_chn_d_ack, cache_addr + "b10000".U, cache_addr ),
		(fsm.state === L2C_state.rspdr) -> Mux( l2c_mst.is_chn_d_ack, cache_addr + "b10000".U, cache_addr ),
		(fsm.state === L2C_state.rspdw) -> cache_addr,
		(fsm.state === L2C_state.rspda) -> cache_addr,
		(fsm.state === L2C_state.fence) -> cache_addr
	))


	object bram {
		val random_res = LFSR(log2Ceil(cb), true.B )

		val is_cb_vhit = Wire( Vec(cb, Bool()) )
		val cache_valid = RegInit( VecInit( Seq.fill(cl)( VecInit( cb, false.B ) ) ) )
		val cl_sel = l2c_addr(addr_lsb+line_w-1, addr_lsb)
		val replace_sel = 
			Mux(
				cache_valid(cl_sel).contains(false.B),
				cache_valid(cl_sel).indexWhere((a: Bool) => (a === false.B)),
				random_res
			)



		for ( i <- 0 until cb ) yield {
			cache_mem.dat_en_w(i) := 
				cb_vhit(i) & l2c_mst.is_chn_d_ack & ( fsm.state === L2C_state.flash | fsm.state === L2C_state.rspdw )

			cache_mem.dat_en_r(i) := 
				cb_vhit(i) & ( fsm.stateDnxt === L2C_state.rspir | fsm.stateDnxt === L2C_state.rspdr )
		}

		cache_mem.dat_info_wstrb := 
			PMux( Seq(
				(fsm.state === L2C_state.flash) -> "hffff".U,
				(fsm.state === L2C_state.rspdw) -> dl1_slv.a.mask
			))

		cache_mem.dat_info_w :=
			PMux( Seq(
				(fsm.state === L2C_state.flash) -> l2c_mst.d.data,
				(fsm.state === L2C_state.rspdw) -> dl1_slv.a.data
			))



		for ( i <- o until cb ) yield {
			cache_mem.tag_en_w(i) := 
				is_block_replace(i.U) & (fsm.state === L2C_state.cktag) & ( fsm.stateDnxt === L2C_state.flash ) 
		
			cache_mem.tag_en_r(i) :=
				( fsm.stateDnxt === L2C_state.cktag ) |
				l2c_mst.is_chn_a_ack

		}


		cache_mem.cache_addr := cache_addr

		when( fsm.state === L2C_state.cktag & fsm.stateDnxt === L2C_state.flash ) {
			cache_valid(cl_sel)(replace_sel) := true.B
		}
		.elsewhen( fsm.state === L2C_state.fence & fsm.stateDnxt === L2C_state.cfree ) {
			for ( i <- 0 until cl; j <- 0 until cb ) yield cache_valid(i)(j) := false.B
		}
		.elsewhen( fsm.state === L2C_state.rspda & fsm.stateDnxt === L2C_state.cfree ) {
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
		val state = RegInit( L2C_state.cfree )

		val l2c_state_dnxt_in_cfree = 
			Mux( io.l2c_fence_req, L2C_state.fence,
				Mux( (il1_chn_a.valid | dl1_chn_a.valid), L2C_state.cktag, L2C_state.cfree )
			)


		val l2c_state_dnxt_in_cktag = Mux1H( Seq(
			(req_no === 1.U) -> ( Mux( cb_vhit.contains(true.B), L2C_state.rspir, L2C_state.flash )  ),
			(req_no === 2.U) -> ( Mux( cb_vhit.contains(true.B), L2C_state.rspdr, L2C_state.flash )  ),
			(req_no === 3.U) -> ( L2C_state.rspdw ),
			(req_no === 4.U) -> ( L2C_state.rspda )
		) )

		val l2c_state_dnxt_in_flash = Mux( l2c_mst.is_free, L2C_state.cktag, L2C_state.flash )
		val l2c_state_dnxt_in_rspir = Mux( il1_slv.is_free, L2C_state.cfree, L2C_state.rspir )
		val l2c_state_dnxt_in_rspdr = Mux( dl1_slv.is_free, L2C_state.cfree, L2C_state.rspdr )
		val l2c_state_dnxt_in_rspda = Mux( dl1_slv.is_free, L2C_state.cfree, L2C_state.rspda )
		val l2c_state_dnxt_in_rspdw = Mux( dl1_slv.is_free, L2C_state.cfree, L2C_state.rspdw )
		val l2c_state_dnxt_in_fence = L2C_state.cfree

		val stateDnxt = Mux1H( Seq(
			(state === L2C_state.cfree) -> l2c_state_dnxt_in_cfree,
			(state === L2C_state.cktag) -> l2c_state_dnxt_in_cktag,
			(state === L2C_state.flash) -> l2c_state_dnxt_in_flash,
			(state === L2C_state.rspir) -> l2c_state_dnxt_in_rspir,
			(state === L2C_state.rspdr) -> l2c_state_dnxt_in_rspdr,
			(state === L2C_state.rspdw) -> l2c_state_dnxt_in_rspdw,
			(state === L2C_state.rspda) -> l2c_state_dnxt_in_rspda,
			(state === L2C_state.fence) -> l2c_state_dnxt_in_fence
		))

		state := stateDnxt
	}


}

