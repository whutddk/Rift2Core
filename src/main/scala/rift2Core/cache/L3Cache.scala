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
		val l3c_fence_end = Output(Bool())
	})

	def addr_lsb = log2Ceil(dw*bk/8)
	def line_w   = log2Ceil(cl)
	def tag_w    = 32 - addr_lsb - line_w
	def cb = 1

	val l2c_slv   = Module( new TileLink_slv_lite(128, 32) )
	val mem_mst_r = Module( new AXI_mst_r( 32, 128, 1, 1, 63 ))
	val mem_mst_w = Module( new AXI_mst_w( 32, 128, 1, 1, 63 ))
	val cache_mem = new Cache_mem( dw, 32, bk, 1, cl )

	object L3C_state extends ChiselEnum {
		val cfree, cktag, evict, flash, rspl2, fence = Value
	}

	object fsm {
		val state_qout = RegInit(L3C_state.cfree )

		val l3c_state_dnxt_in_cfree = 
			Mux( io.l3c_fence_req, L3C_state.fence, Mux( l2c_slv.io.a.valid, L3C_state.cktag, L3C_state.cfree ) )

		val l3c_state_dnxt_in_cktag = 
			Mux( ~bram.cache_valid(bram.cl_sel), L3C_state.flash,
				Mux( ~bram.is_cb_hit, L3C_state.evict,
					Mux( (l2c_slv.io.a.bits.opcode =/= l2c_slv.Get & db.is_full), L3C_state.evict, L3C_state.rspl2  )))

		val l3c_state_dnxt_in_evict = 
			Mux( ~mem_mst_w.io.end, L3C_state.evict,
				Mux( io.l3c_fence_req, L3C_state.fence, L3C_state.cktag ))

		val l3c_state_dnxt_in_flash = 
			Mux( mem_mst_r.io.end, L3C_state.evict, L3C_state.flash )

		val l3c_state_dnxt_in_rspl2 = 
			Mux( l2c_slv.io.mode === 7.U, L3C_state.cfree, L3C_state.rspl2 )

		val l3c_state_dnxt_in_fence = 
			Mux( db.is_empty, L3C_state.cfree, L3C_state.evict )

		val state_dnxt = Mux1H( Seq(
			(state_qout === L3C_state.cfree) -> l3c_state_dnxt_in_cfree,
			(state_qout === L3C_state.cktag) -> l3c_state_dnxt_in_cktag,
			(state_qout === L3C_state.flash) -> l3c_state_dnxt_in_flash,
			(state_qout === L3C_state.evict) -> l3c_state_dnxt_in_evict,
			(state_qout === L3C_state.rspl2) -> l3c_state_dnxt_in_rspl2,
			(state_qout === L3C_state.fence) -> l3c_state_dnxt_in_fence
		))

		
		state_qout := state_dnxt
	}

	object bram {
		val cache_addr_dnxt = Wire(UInt(32.W))
		val cache_addr_qout = RegInit(0.U(32.W))
		val cache_valid = RegInit(VecInit(Seq.fill(cl)(false.B)))
		val mem_dat = cache_mem.dat_info_r(0)

		cache_addr_qout := cache_addr_dnxt

		cache_addr_dnxt := Mux1H(Seq(
			(fsm.state_qout === L3C_state.cfree) -> l2c_slv.io.a.bits.address,
			(fsm.state_qout === L3C_state.cktag) -> Mux(db.is_full, db.addr_o, l2c_slv.io.a.bits.address),
			(fsm.state_qout === L3C_state.evict) -> Mux( mem_mst_w.io.end, cache_addr_qout + "b10000".U, cache_addr_qout ),
			(fsm.state_qout === L3C_state.flash) -> Mux( mem_mst_r.io.end, cache_addr_qout + "b10000".U, cache_addr_qout ),
			(fsm.state_qout === L3C_state.rspl2) -> Mux( l2c_slv.io.d.fire,cache_addr_qout + "b10000".U, cache_addr_qout ),
			(fsm.state_qout === L3C_state.fence) -> db.addr_o

		))


		cache_mem.cache_addr := cache_addr_dnxt

		cache_mem.dat_en_w(0) :=
			(fsm.state_qout === L3C_state.flash & mem_mst_r.io.r.fire) |
			(fsm.state_qout === L3C_state.rspl2 & (l2c_slv.io.a.bits.opcode === l2c_slv.PutFullData) & l2c_slv.io.a.fire)

		cache_mem.dat_en_r(0) :=
			(fsm.state_dnxt === L3C_state.flash) |
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
						(l2c_slv.io.a.bits.opcode === l2c_slv.ArithmeticData) -> 
							PMux(Seq(
								(l2c_slv.io.a.bits.param === l2c_slv.MIN)  -> Mux( l2c_slv.io.a.bits.data.asSInt > mem_dat.asSInt, mem_dat, l2c_slv.io.a.bits.data ),
								(l2c_slv.io.a.bits.param === l2c_slv.MAX)  -> Mux( l2c_slv.io.a.bits.data.asSInt > mem_dat.asSInt, l2c_slv.io.a.bits.data, mem_dat ),
								(l2c_slv.io.a.bits.param === l2c_slv.MINU) -> Mux( l2c_slv.io.a.bits.data > mem_dat, mem_dat, l2c_slv.io.a.bits.data ),
								(l2c_slv.io.a.bits.param === l2c_slv.MAXU) -> Mux( l2c_slv.io.a.bits.data > mem_dat, l2c_slv.io.a.bits.data, mem_dat ),
								(l2c_slv.io.a.bits.param === l2c_slv.ADD)  -> (l2c_slv.io.a.bits.data + mem_dat)
							)),
						(l2c_slv.io.a.bits.opcode === l2c_slv.LogicalData) ->
							PMux(Seq(
								(l2c_slv.io.a.bits.param === l2c_slv.XOR) -> ( mem_dat ^ l2c_slv.io.a.bits.data ),
								(l2c_slv.io.a.bits.param === l2c_slv.OR)  -> ( mem_dat | l2c_slv.io.a.bits.data ),
								(l2c_slv.io.a.bits.param === l2c_slv.AND) -> ( mem_dat & l2c_slv.io.a.bits.data ),
								(l2c_slv.io.a.bits.param === l2c_slv.AND) -> ( l2c_slv.io.a.bits.data ),							
							))
					))
				
			))

		cache_mem.tag_en_w :=
			(fsm.state_qout === L3C_state.cktag) & (fsm.state_dnxt === L3C_state.flash)

		cache_mem.tag_en_r :=
			(fsm.state_dnxt === L3C_state.cktag)


		val tag_addr = cache_addr_dnxt(31, 32-tag_w)
		val is_cb_hit = cache_mem.tag_info_r(0) === tag_addr


		val cl_sel = cache_addr_dnxt(addr_lsb+line_w-1, addr_lsb)

		for ( i <- 0 until cl ) yield {
			cache_valid(i) :=
				Mux(
					(fsm.state_qout === L3C_state.fence) & (fsm.state_dnxt === L3C_state.cfree),
					false.B,
					Mux(
						i.U =/= cl_sel,
						cache_valid(i),
						MuxCase( cache_valid(i), Array(
							(fsm.state_qout === L3C_state.evict) -> false.B,
							(fsm.state_qout === L3C_state.flash) -> true.B
						))
					)
			)
		}


	}

	object db {
		val buf = RegInit( VecInit(Seq.fill(16)(0.U(tag_w.W))) )
		val valid = RegInit( VecInit(Seq.fill(16)(false.B)) )

		val is_full  = valid.forall((x:Bool) => (x === true.B))
		val is_empty = valid.forall((x:Bool) => (x === false.B))

		val idx_enq = valid.indexWhere((x:Bool) => (x === false.B))
		val idx_deq = valid.lastIndexWhere((x:Bool) => (x === true.B))

		val is_hazard = buf.contains(bram.cache_addr_dnxt(31,32-tag_w))

		when(
			~is_hazard & 
			fsm.state_qout === L3C_state.cktag & fsm.state_dnxt === L3C_state.rspl2 &
			(l2c_slv.io.a.bits.opcode === l2c_slv.PutFullData | l2c_slv.io.a.bits.opcode === l2c_slv.ArithmeticData | l2c_slv.io.a.bits.opcode === l2c_slv.LogicalData)
		){
			buf(idx_enq)   := bram.cache_addr_dnxt(31,32-tag_w)
			valid(idx_enq) := true.B
		}
		.elsewhen( fsm.state_qout === L3C_state.evict & fsm.state_dnxt =/= L3C_state.evict ){
			valid(idx_deq) := false.B
		}

		val addr_o = buf(idx_deq)
	}

	object bus {
		l2c_slv.io.is_rsp   := fsm.state_qout === L3C_state.cktag & fsm.state_dnxt === L3C_state.rspl2
		mem_mst_r.io.ar_req := fsm.state_qout === L3C_state.cktag & fsm.state_dnxt === L3C_state.flash
		mem_mst_w.io.aw_req := fsm.state_qout =/= L3C_state.evict & fsm.state_dnxt === L3C_state.evict
	
		l2c_slv.io.d.bits.data := bram.mem_dat

		mem_mst_r.io.ar.bits.addr :=
			RegEnable( bram.cache_addr_dnxt & ~("b1111111111".U(32.W)), 0.U, mem_mst_r.io.ar_req )


		mem_mst_r.io.ar.bits.burst := "b01".U
		mem_mst_r.io.ar.bits.id := 0.U
		mem_mst_r.io.ar.bits.len := 63.U
		mem_mst_r.io.ar.bits.size := 4.U
		mem_mst_r.io.ar.bits.user := 0.U

		mem_mst_w.io.aw.bits.addr :=
			RegEnable( bram.cache_addr_dnxt & ~("b1111111111".U(32.W)), 0.U, mem_mst_w.io.aw_req )


		mem_mst_w.io.aw.bits.burst := "b01".U
		mem_mst_w.io.aw.bits.id := 0.U
		mem_mst_w.io.aw.bits.len := 63.U
		mem_mst_w.io.aw.bits.size := 4.U
		mem_mst_w.io.aw.bits.user := 0.U

		mem_mst_w.io.w.bits.data := bram.mem_dat
		mem_mst_w.io.w.bits.strb := Fill(16, 1.U)
		mem_mst_w.io.w.bits.user := 0.U
		



	}

}

