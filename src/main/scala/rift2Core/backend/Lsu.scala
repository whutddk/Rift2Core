/*
* @Author: Ruige Lee
* @Date:   2021-03-29 14:37:18
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-02 18:09:45
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

package rift2Core.backend


import chisel3._
import chisel3.util._
import chisel3.experimental.chiselName
import rift2Core.basicElement._
import rift2Core.cache._
import tilelink._

class System_bus() {

}

class DCache_bus() {

}


// @chiselName
class Lsu_fsm {
	


}



@chiselName
class Lsu extends Module {
	val io = IO(new Bundle{
		val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
		val lsu_exe_iwb = new DecoupledIO(new Exe_iwb_info)

		val flush = Input(Bool())
	})

	def dw = 64
	def bk = 4
	def cb = 4
	def cl = 64

	val addr_lsb = log2Ceil(dw*bk/8)
	val line_w   = log2Ceil(cl)
	val tag_w    = 32 - addr_lsb - line_w









	val iwb_valid = Reg(Bool())
	val iwb_res = Reg(UInt(64.W))
	val iwb_rd0 = Reg(UInt(7.W))

	def iss_ack = io.lsu_exe_iwb.valid & io.lsu_exe_iwb.ready
	def iwb_ack = io.lsu_exe_iwb.valid & io.lsu_exe_iwb.ready

	val dcache = new Dcache(128, 1, 4, 64)
	val wtb    = Module(new Wt_block(64+8+32, 3))

	def op1 = io.lsu_iss_exe.bits.param.op1
	def op1_tag = op1(31,32-tag_w)
	def op1_align64 = op1(31,0) & ("b111".U)

	def lsu_lb  = io.lsu_iss_exe.bits.fun.lb
	def lsu_lh  = io.lsu_iss_exe.bits.fun.lh
	def lsu_lw  = io.lsu_iss_exe.bits.fun.lw
	def lsu_ld  = io.lsu_iss_exe.bits.fun.ld
	def lsu_lbu = io.lsu_iss_exe.bits.fun.lbu
	def lsu_lhu = io.lsu_iss_exe.bits.fun.lhu
	def lsu_lwu = io.lsu_iss_exe.bits.fun.lwu
	def lsu_sb  = io.lsu_iss_exe.bits.fun.sb
	def lsu_sh  = io.lsu_iss_exe.bits.fun.sh
	def lsu_sw  = io.lsu_iss_exe.bits.fun.sw
	def lsu_sd  = io.lsu_iss_exe.bits.fun.sd



	def is_accessFalut = io.lsu_iss_exe.valid & ( is_ren | is_wen) & (~is_memory & ~is_system)
	def is_misAlign    = io.lsu_iss_exe.valid & (
													  ( (lsu_lh | lsu_lhu | lsu_sh ) & ( op1(0) =/= 0.U ) )
													| ( (lsu_lw | lsu_lwu | lsu_sw ) & ( op1(1,0) =/= 0.U ) )
													| ( (lsu_ld | lsu_sd)            & ( op1(2,0) =/= 0.U ) )			
												)


	def is_ren = io.lsu_iss_exe.bits.fun.is_lu
	def is_wen = io.lsu_iss_exe.bits.fun.is_su
	def is_memory = (op1(63,32) === 0.U) & (op1(31) === 1.U)
	def is_system = (op1(63,32) === 0.U) & (op1(31,30) === 0.U)
	def is_hazard = wtb.is_hazard( op1_align64, Cat(Fill(tag_w, 1.U), Fill(32-tag_w, 0.U) ) )
	def is_wtfull = wtb.full


	val dl1_state_cfree :: dl1_state_cread :: dl1_state_mwait :: dl1_state_cmiss :: dl1_state_write :: dl1_state_fence :: dl1_state_pwait :: dl1_state_pread :: Nil = Enum(8)
	val stateReg = RegNext(stateDnxt)


	def dl1_state_dnxt_in_cfree = {
		Mux(
			( io.lsu_iss_exe.valid & (io.lsu_iss_exe.bits.fun.fence | io.lsu_iss_exe.bits.fun.fence_i) & ~io.flush ),
			dl1_state_fence,
			Mux(
				(io.lsu_iss_exe.valid & ~is_accessFalut & ~is_misAlign & ~io.flush),
				MuxCase(dl1_state_cfree, Array(
					(is_ren & is_memory) -> dl1_state_cread,
					(is_wen & ~is_wtfull) -> dl1_state_write,
					(is_ren & is_system &  is_hazard) -> dl1_state_pwait,
					(is_ren & is_system & ~is_hazard) -> dl1_state_pread			
				)),
				dl1_state_cfree
			)
		)
	}

	def dl1_state_dnxt_in_cread = {
		Mux( 
			cb_vhit.orR,
			dl1_state_cfree,
			Mux( is_hazard, dl1_state_mwait, dl1_state_cmiss )
		)
	}

	def dl1_state_dnxt_in_mwait = Mux( is_hazard, dl1_state_mwait, dl1_state_cmiss )
	def dl1_state_dnxt_in_cmiss = Mux( dl1_end_r, dl1_state_cfree, dl1_state_cmiss )
	def dl1_state_dnxt_in_write = dl1_state_cfree
	def dl1_state_dnxt_in_fence = Mux( fence_end, dl1_state_cfree, dl1_state_fence )
	def dl1_state_dnxt_in_pwait = Mux( is_hazard, dl1_state_pwait, dl1_state_pread )
	def dl1_state_dnxt_in_pread = Mux( sys_end_r, dl1_state_cfree, dl1_state_pread )

	def state_dnxt = MuxCase( dl1_state_cfree, Array(
						(stateReg === dl1_state_cfree) -> dl1_state_dnxt_in_cfree ,
						(stateReg === dl1_state_cread) -> dl1_state_dnxt_in_cread ,
						(stateReg === dl1_state_mwait) -> dl1_state_dnxt_in_mwait ,
						(stateReg === dl1_state_cmiss) -> dl1_state_dnxt_in_cmiss ,
						(stateReg === dl1_state_write) -> dl1_state_dnxt_in_write ,
						(stateReg === dl1_state_fence) -> dl1_state_dnxt_in_fence ,
						(stateReg === dl1_state_pwait) -> dl1_state_dnxt_in_pwait ,
						(stateReg === dl1_state_pread) -> dl1_state_dnxt_in_pread ,
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






	val mem = new Cache_mem( dw, 32, bk, cb, cl )
	println("the dcache has "+dw+"*"+bk+"bit,with "+cb+"cache block,and "+cl+"cache line")
	println("Toltal size is "+dw*bk*cb*cl/8/1024.0+"KB")


	switch(stateReg) {
		is (dl1_state_cfree) {
			mem.cache_addr := op1_align64
			for ( i <- 0 unitl cb ) yield {
				mem.dat_en_w(i) := false.B
				mem.dat_en_r(i) := (stateDnxt === dl1_state_cread) & is_memory
				mem.tag_en_w(i) := false.B
				mem.tag_en_r(i) := (stateDnxt === dl1_state_cread) | ( stateDnxt === dl1_state_write & is_memory )
			}

			mem.dat_info_wstrb := DontCare
			mem.dat_info_w = DontCare

			mem.dat_info_r = Wire( Vec(cb, UInt(dw.W)) )
			mem.tag_info_r = Wire( Vec(cb, UInt(tag_w.W)) )
		}
		is (dl1_state_cread) {
			mem.cache_addr = DontCare

			for ( i <- 0 unitl cb ) yield {
				mem.dat_en_w(i) := false.B
				mem.dat_en_r(i) := false.B
				mem.tag_en_w(i) := (stateDnxt === dl1_state_cmiss) & is_block_replace(i)
				mem.tag_en_r(i) := false.B			
			}

			mem.dat_info_wstrb := DontCare
			mem.dat_info_w = DontCare

		}
		is (dl1_state_mwait) {
			mem.cache_addr = DontCare

			for ( i <- 0 unitl cb ) yield {
				mem.dat_en_w(i) := false.B
				mem.dat_en_r(i) := false.B
				mem.tag_en_w(i) := (stateDnxt === dl1_state_cmiss) & is_block_replace(i)
				mem.tag_en_r(i) := false.B
			}
			mem.dat_info_wstrb := DontCare
			mem.dat_info_w = DontCare

		}
		is (dl1_state_cmiss) {
			mem.cache_addr = op1_bus_req

			for ( i <- 0 unitl cb ) yield {
				mem.dat_en_w(i) := is_cb_vhit(i) & dcache_bus.is_accessAckData()
				mem.dat_en_r(i) := false.B
				mem.tag_en_w(i) := false.B
				mem.tag_en_r(i) := true.B
			}
			mem.dat_info_wstrb := Fill((dw/8), 1.U)
			mem.dat_info_w := dcache_bus.data_ack

		}
		is (dl1_state_write) {
			mem.cache_addr = DontCare
			for ( i <- 0 unitl cb ) yield {
				mem.dat_en_w(i) := is_cb_vhit(i)
				mem.dat_en_r(i) := false.B
				mem.tag_en_w(i) := false.B
				mem.tag_en_r(i) := false.B			
			}
			mem.dat_info_wstrb = lsu_wstrb_align
			mem.dat_info_w = lsu_wdata_align
	
		}
		is (dl1_state_pwait) {
			mem.cache_addr = DontCare
			for ( i <- 0 unitl cb ) yield {
				mem.dat_en_w(i) := false.B
				mem.dat_en_r(i) := false.B
				mem.tag_en_w(i) := false.B
				mem.tag_en_r(i) := false.B
			}
			mem.dat_info_wstrb = DontCare
			mem.dat_info_w = DontCare

		}
		is (dl1_state_pread) {
			mem.cache_addr = DontCare
			for ( i <- 0 unitl cb ) yield {
				mem.dat_en_w(i) := false.B
				mem.dat_en_r(i) := false.B
				mem.tag_en_w(i) := false.B
				mem.tag_en_r(i) := false.B
			}
			mem.dat_info_wstrb = DontCare
			mem.dat_info_w = DontCare

		}
		is (dl1_state_fence) {
			mem.cache_addr = DontCare
			for ( i <- 0 unitl cb ) yield {
				mem.dat_en_w(i) := false.B
				mem.dat_en_r(i) := false.B
				mem.tag_en_w(i) := false.B
				mem.tag_en_r(i) := false.B
			}
			mem.dat_info_wstrb = DontCare
			mem.dat_info_w = DontCare

		}
	}


	val cache_valid = Reg( Vec(cb, Vec(cl, Bool() )) )

	when ( reset.asBool ) {
		for ( i <- 0 until cb; j <- 0 until cl ) yield cache_valid(i)(j) := false.B
	}
	.elsewhen(
			 ( stateReg === dl1_state_cread & stateDnxt === dl1_state_cmiss)
			|( stateReg === dl1_state_mwait & stateDnxt === dl1_state_cmiss )
		) {
		cache_valid(replace_sel)(cl_sel) := true.B
	}
	.elsewhen(stateReg === dl1_state_fence & stateDnxt === dl1_state_cfree) {
		for ( i <- 0 until cb; j <- 0 until cl ) yield cache_valid(i)(j) := false.B
	}


	def is_cb_vhit(i: Int): Bool = cache_valid(i)(cl_sel) & ( mem.tag_info_r(i) === op1_tag )



	def replace_sel: UInt = {
		var res = LFSR16()
		for ( i <- 0 unitl cb ) {
			if ( cache_valid(i)(cl_sel) == false.B ){
				res = i.U
			}
		}
		return res

	}

	def is_block_replace(i) = UIntToOH(replace_sel)









	def dl1_end_r = 
	def sys_end_r = 
	def fence_end = 
op1_bus_req
lsu_wstrb_align
lsu_wdata_align


cl_sel
}
