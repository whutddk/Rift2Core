/*
* @Author: Ruige Lee
* @Date:   2021-03-29 14:37:18
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-06 17:58:53
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
import chisel3.util.random._
import rift2Core.basicElement._
import rift2Core.cache._
import tilelink._
import chisel3.experimental.chiselName





@chiselName
class Lsu extends Module {
	val io = IO(new Bundle{
		val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
		val lsu_exe_iwb = new DecoupledIO(new Exe_iwb_info)

		val dl1_chn_a = new TLchannel_a(128,32)
		val dl1_chn_d = Flipped(new TLchannel_d(128))

		val sys_chn_a = new TLchannel_a(64,32)
		val sys_chn_d = Flipped(new TLchannel_d(64))

		val is_fence_commit = Input(Bool())
		val is_store_commit = Input(Bool())

		val l2c_fence_req = Output(Bool())
		val l3c_fence_req = Output(Bool())
		val l2c_fence_end = Input(Bool())
		val l3c_fence_end = Input(Bool())

		val flush = Input(Bool())
	})

	def dw = 128
	def bk = 2
	def cb = 4
	def cl = 64

	val addr_lsb = log2Ceil(dw*bk/8)
	val line_w   = log2Ceil(cl)
	val tag_w    = 32 - addr_lsb - line_w


	val sys_mst = new TileLink_mst(64, 32, 0)
	val dl1_mst = new TileLink_mst(128,32, 0)

	io.sys_chn_a := sys_mst.a
	io.dl1_chn_a := dl1_mst.a
	io.sys_chn_d := sys_mst.d
	io.dl1_chn_d := dl1_mst.d


	val iwb_valid = Reg(Bool())
	val iwb_res = Reg(UInt(64.W))
	val iwb_rd0 = Reg(UInt(7.W))

	def iss_ack = io.lsu_exe_iwb.valid & io.lsu_exe_iwb.ready
	def iwb_ack = io.lsu_exe_iwb.valid & io.lsu_exe_iwb.ready

	io.lsu_iss_exe.ready := iwb_ack


	val wtb    = Module(new Wt_block(3))

	def op1 = io.lsu_iss_exe.bits.param.op1
	def op2 = io.lsu_iss_exe.bits.param.op2
	def op1_tag = op1(31,32-tag_w)
	def op1_align64 = op1(31,0) & ~("b111".U)
	def op1_align128 = op1(31,0) & ~("b1111".U)
	def op1_align256 = op1(31,0) & ~("b11111".U)

	def cl_sel = op1(addr_lsb+line_w-1, addr_lsb)


	def lsu_wstrb_align = MuxCase( 0.U, Array(
						lsu_sb -> "b00000001".U,
						lsu_sh -> "b00000011".U,
						lsu_sw -> "b00001111".U,
						lsu_sd -> "b11111111".U
						)) << op1(2,0)

	def lsu_wdata_align = op2 << (op1(2,0) << 3)








	def lsu_lb      = io.lsu_iss_exe.bits.fun.lb
	def lsu_lh      = io.lsu_iss_exe.bits.fun.lh
	def lsu_lw      = io.lsu_iss_exe.bits.fun.lw
	def lsu_ld      = io.lsu_iss_exe.bits.fun.ld
	def lsu_lbu     = io.lsu_iss_exe.bits.fun.lbu
	def lsu_lhu     = io.lsu_iss_exe.bits.fun.lhu
	def lsu_lwu     = io.lsu_iss_exe.bits.fun.lwu
	def lsu_sb      = io.lsu_iss_exe.bits.fun.sb
	def lsu_sh      = io.lsu_iss_exe.bits.fun.sh
	def lsu_sw      = io.lsu_iss_exe.bits.fun.sw
	def lsu_sd      = io.lsu_iss_exe.bits.fun.sd
	def lsu_fence   = io.lsu_iss_exe.bits.fun.fence
	def lsu_fence_i = io.lsu_iss_exe.bits.fun.fence_i

	def is_usi = lsu_lbu | lsu_lhu | lsu_lwu

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
	def is_hazard = wtb.is_hazard( op1_align64, Cat(Fill(2, 1.U), Fill(30, 0.U) ) ) //only check whether is mem or sys
	def is_wtfull = wtb.full






	def lsu_rsp_data_reAlign8(rdata: UInt): UInt = MuxLookup(op1(2,0), 0.U, Array(
			"b000".U -> rdata(7,0),   "b001".U -> rdata(15,8),  "b010".U -> rdata(23,16), "b011".U -> rdata(31,24),
			"b100".U -> rdata(39,32), "b101".U -> rdata(47,40), "b110".U -> rdata(55,48), "b111".U -> rdata(63,56)))

	def lsu_rsp_data_reAlign16(rdata: UInt): UInt = MuxLookup(op1(2,1), 0.U, Array(
			"b00".U -> rdata(15,0), "b01".U -> rdata(31,16),
			"b10".U -> rdata(47,32), "b11".U -> rdata(63,48)))

	def lsu_rsp_data_reAlign32(rdata: UInt): UInt = MuxLookup(op1(2), 0.U, Array(
									0.U -> rdata(31,0), 1.U -> rdata(63,32)))


	def load_byte(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(56, Mux(is_usi, 0.U, rdata(7)) ),  rdata(7,0)  )
	def load_half(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(48, Mux(is_usi, 0.U, rdata(15)) ), rdata(15,0) )
	def load_word(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(32, Mux(is_usi, 0.U, rdata(31)) ), rdata(31,0) )



	val wb_valid = Reg(Bool())
	val res = Reg(UInt(64.W))


	io.lsu_exe_iwb.bits.rd0_raw := io.lsu_iss_exe.bits.param.rd0_raw
	io.lsu_exe_iwb.bits.rd0_idx := io.lsu_iss_exe.bits.param.rd0_idx
	io.lsu_exe_iwb.bits.res := res
	io.lsu_exe_iwb.valid := wb_valid


	when (reset.asBool | io.flush) {
		wb_valid := false.B
		res      := 0.U
	}
	.elsewhen ( stateReg === dl1_state_cread & stateDnxt === dl1_state_cfree ) {
		wb_valid := true.B
		res      := res_dnxt
	}
	.elsewhen ( stateReg === dl1_state_cmiss ) {
		if ( (op1_dl1_req == op1_align128) && (dl1_mst.is_accessAckData == true.B) && (trans_kill == false.B) ) {
			wb_valid := true.B
			res      := res_dnxt
		}
	}
	.elsewhen ( stateReg === dl1_state_write & stateDnxt === dl1_state_cfree ) {
		wb_valid := true.B
		res      := 0.U
	}
	.elsewhen ( stateReg === dl1_state_fence ) {
		if ( (wtb.empty == true.B) & (trans_kill == false.B) ) {
			wb_valid := true.B
			res      := 0.U
		}
	}
	.elsewhen ( stateReg === dl1_state_pread & stateDnxt === dl1_state_cfree ) {
		if ( trans_kill == false.B ) {
			wb_valid := true.B
			res      := res_dnxt
		}
	}
	.otherwise {

		if ( iwb_ack == true.B ) {
			wb_valid := false.B
		}
	}


	def res_dnxt = MuxCase( DontCare, Array(
		io.lsu_iss_exe.bits.fun.lb        -> load_byte(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.lh        -> load_half(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.lw        -> load_word(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.ld        -> rsp_data,
		io.lsu_iss_exe.bits.fun.lbu       -> load_byte(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.lhu       -> load_half(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.lwu       -> load_word(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.sb        -> 0.U,
		io.lsu_iss_exe.bits.fun.sh        -> 0.U,
		io.lsu_iss_exe.bits.fun.sw        -> 0.U,
		io.lsu_iss_exe.bits.fun.sd        -> 0.U,
		io.lsu_iss_exe.bits.fun.fence     -> 0.U,
		io.lsu_iss_exe.bits.fun.fence_i   -> 0.U,

		io.lsu_iss_exe.bits.fun.lr_w      -> load_word(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.sc_w      -> 0.U,
		io.lsu_iss_exe.bits.fun.amoswap_w -> load_word(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.amoadd_w  -> load_word(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.amoxor_w  -> load_word(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.amoand_w  -> load_word(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.amoor_w   -> load_word(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.amomin_w  -> load_word(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.amomax_w  -> load_word(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.amominu_w -> load_word(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.amomaxu_w -> load_word(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.lr_d      -> rsp_data,
		io.lsu_iss_exe.bits.fun.sc_d      -> 0.U,
		io.lsu_iss_exe.bits.fun.amoswap_d -> rsp_data,
		io.lsu_iss_exe.bits.fun.amoadd_d  -> rsp_data,
		io.lsu_iss_exe.bits.fun.amoxor_d  -> rsp_data,
		io.lsu_iss_exe.bits.fun.amoand_d  -> rsp_data,
		io.lsu_iss_exe.bits.fun.amoor_d   -> rsp_data,
		io.lsu_iss_exe.bits.fun.amomin_d  -> rsp_data,
		io.lsu_iss_exe.bits.fun.amomax_d  -> rsp_data,
		io.lsu_iss_exe.bits.fun.amominu_d -> rsp_data,
		io.lsu_iss_exe.bits.fun.amomaxu_d -> rsp_data,

		io.lsu_iss_exe.bits.fun.flw       -> load_word(is_usi, rsp_data),
		io.lsu_iss_exe.bits.fun.fsw       -> 0.U,
		io.lsu_iss_exe.bits.fun.fld       -> rsp_data,
		io.lsu_iss_exe.bits.fun.fsd       -> 0.U,
	))


	def rsp_data = MuxLookup( stateReg, 0.U, Array(
		dl1_state_cfree -> 0.U,
		dl1_state_cread -> Mux(op1(3), mem_dat(127,64), mem_dat(63,0)),
		dl1_state_mwait -> 0.U,
		dl1_state_cmiss -> Mux(op1(3), dl1_mst.data_ack(127,64), dl1_mst.data_ack(63,0)),
		dl1_state_write -> 0.U,
		dl1_state_fence -> 0.U,
		dl1_state_pwait -> 0.U,
		dl1_state_pread -> sys_mst.data_ack
	))





//    SSSSSSSSSSSSSSS YYYYYYY       YYYYYYY   SSSSSSSSSSSSSSS 
//  SS:::::::::::::::SY:::::Y       Y:::::Y SS:::::::::::::::S
// S:::::SSSSSS::::::SY:::::Y       Y:::::YS:::::SSSSSS::::::S
// S:::::S     SSSSSSSY::::::Y     Y::::::YS:::::S     SSSSSSS
// S:::::S            YYY:::::Y   Y:::::YYYS:::::S            
// S:::::S               Y:::::Y Y:::::Y   S:::::S            
//  S::::SSSS             Y:::::Y:::::Y     S::::SSSS         
//   SS::::::SSSSS         Y:::::::::Y       SS::::::SSSSS    
//     SSS::::::::SS        Y:::::::Y          SSS::::::::SS  
//        SSSSSS::::S        Y:::::Y              SSSSSS::::S 
//             S:::::S       Y:::::Y                   S:::::S
//             S:::::S       Y:::::Y                   S:::::S
// SSSSSSS     S:::::S       Y:::::Y       SSSSSSS     S:::::S
// S::::::SSSSSS:::::S    YYYY:::::YYYY    S::::::SSSSSS:::::S
// S:::::::::::::::SS     Y:::::::::::Y    S:::::::::::::::SS 
//  SSSSSSSSSSSSSSS       YYYYYYYYYYYYY     SSSSSSSSSSSSSSS 
	


	// def sys_write = (wtb_addr(31,30) === 0.U)

	sys_mst.d.ready := true.B


	when ( (stateReg =/= dl1_state_pread) & (stateDnxt === dl1_state_pread) ) {
		sys_mst.op_getData(op1_align64, 3.U )
		sys_mst.a_valid_set()
	}
	.elsewhen(stateReg === dl1_state_pread) {
		if ( sys_mst.is_chn_a_ack == true.B) {  // sys chn a ack, release a valid
			sys_mst.a_valid_rst()
		}
		if ( sys_mst.is_accessAckData == true.B ) {	//sys chn d ack, leave PREAD
			rsp_data := sys_mst.data_ack
		}
	}
	.otherwise {
		if ( (~sys_mst.is_a_busy & ~sys_mst.is_d_busy & is_hazard ) == true.B) {
			sys_mst.op_putFullData(wtb_addr, 3.U, wtb_data, wtb_wstrb)
			sys_mst.a_valid_set()
		}
		if ( sys_mst.is_chn_a_ack == true.B ) {
			sys_mst.a_valid_rst()
		}

	}





// MMMMMMMM               MMMMMMMMEEEEEEEEEEEEEEEEEEEEEEMMMMMMMM               MMMMMMMM
// M:::::::M             M:::::::ME::::::::::::::::::::EM:::::::M             M:::::::M
// M::::::::M           M::::::::ME::::::::::::::::::::EM::::::::M           M::::::::M
// M:::::::::M         M:::::::::MEE::::::EEEEEEEEE::::EM:::::::::M         M:::::::::M
// M::::::::::M       M::::::::::M  E:::::E       EEEEEEM::::::::::M       M::::::::::M
// M:::::::::::M     M:::::::::::M  E:::::E             M:::::::::::M     M:::::::::::M
// M:::::::M::::M   M::::M:::::::M  E::::::EEEEEEEEEE   M:::::::M::::M   M::::M:::::::M
// M::::::M M::::M M::::M M::::::M  E:::::::::::::::E   M::::::M M::::M M::::M M::::::M
// M::::::M  M::::M::::M  M::::::M  E:::::::::::::::E   M::::::M  M::::M::::M  M::::::M
// M::::::M   M:::::::M   M::::::M  E::::::EEEEEEEEEE   M::::::M   M:::::::M   M::::::M
// M::::::M    M:::::M    M::::::M  E:::::E             M::::::M    M:::::M    M::::::M
// M::::::M     MMMMM     M::::::M  E:::::E       EEEEEEM::::::M     MMMMM     M::::::M
// M::::::M               M::::::MEE::::::EEEEEEEE:::::EM::::::M               M::::::M
// M::::::M               M::::::ME::::::::::::::::::::EM::::::M               M::::::M
// M::::::M               M::::::ME::::::::::::::::::::EM::::::M               M::::::M
// MMMMMMMM               MMMMMMMMEEEEEEEEEEEEEEEEEEEEEEMMMMMMMM               MMMMMMMM


	dl1_mst.d.ready := true.B

	when( stateReg === dl1_state_cread & stateDnxt === dl1_state_cmiss ) {
		dl1_mst.op_getData(op1_align64, 5.U )
		dl1_mst.a_valid_set()
	}
	.elsewhen( stateReg === dl1_state_mwait & stateDnxt === dl1_state_cmiss ) {
		dl1_mst.op_getData(op1_align64, 5.U )
		dl1_mst.a_valid_set()
	}
	.elsewhen( stateReg === dl1_state_cmiss ) {
		if ( dl1_mst.is_chn_a_ack == true.B) {  // dl1 chn a ack, release a valid
			dl1_mst.a_valid_rst()
		}
		if ( dl1_mst.is_accessAckData == true.B) {	//dl1 chn d ack, 
			// rsp_data := dl1_mst.data_ack
		}
	}
	.otherwise {
		if ( (~dl1_mst.is_a_busy & ~dl1_mst.is_d_busy & is_hazard ) == true.B) {

			def align_addr = wtb_addr & ~("b1111".U)
			def align_data = Mux( wtb_addr(3), Cat( wtb_data, 0.U(64.W) ), Cat(  0.U(64.W), wtb_data ) )
			def align_wstrb = Mux( wtb_addr(3), Cat( wtb_wstrb, 0.U(8.W) ), Cat(  0.U(8.W), wtb_wstrb ) )

			dl1_mst.op_putFullData(align_addr, 4.U, align_data, align_wstrb)    //write in 128 bit, and it need realign
			dl1_mst.a_valid_set()
		}
		if ( dl1_mst.is_chn_a_ack == true.B ) {
			dl1_mst.a_valid_rst()
		}
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



	val dl1_state_cfree :: dl1_state_cread :: dl1_state_mwait :: dl1_state_cmiss :: dl1_state_write :: dl1_state_fence :: dl1_state_pwait :: dl1_state_pread :: Nil = Enum(8)
	val stateDnxt = WireDefault(dl1_state_cfree)
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
					(is_ren & is_system &  is_hazard ) -> dl1_state_pwait,
					(is_ren & is_system & ~is_hazard ) -> dl1_state_pread			
				)),
				dl1_state_cfree
			)
		)
	}

	def dl1_state_dnxt_in_cread = {
		Mux( 
			{ var res = false.B; for ( i <- 0 until cb ) {  res = res | is_cb_vhit(i) }; res},
			dl1_state_cfree,
			Mux( is_hazard, dl1_state_mwait, dl1_state_cmiss )
		)
	}

	def dl1_state_dnxt_in_mwait = Mux( is_hazard, dl1_state_mwait, dl1_state_cmiss )
	def dl1_state_dnxt_in_cmiss = Mux( dl1_mst.is_free, dl1_state_cfree, dl1_state_cmiss )
	def dl1_state_dnxt_in_write = dl1_state_cfree
	def dl1_state_dnxt_in_fence = Mux( is_fence_end, dl1_state_cfree, dl1_state_fence )
	def dl1_state_dnxt_in_pwait = Mux( is_hazard, dl1_state_pwait, dl1_state_pread )
	def dl1_state_dnxt_in_pread = Mux( sys_mst.is_accessAckData, dl1_state_cfree, dl1_state_pread )

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

	val op1_dl1_req = RegInit(0.U(32.W))

	when( stateReg =/= dl1_state_cmiss & stateDnxt === dl1_state_cmiss ) {
		op1_dl1_req := op1_align256
	}
	.elsewhen( stateDnxt === dl1_state_cmiss ) {
		if ( dl1_mst.is_chn_a_ack == true.B ) {
			op1_dl1_req := op1_dl1_req + "b1000".U
		}
	}



	switch(stateReg) {
		is (dl1_state_cfree) {
			mem.cache_addr := op1_align128
			for ( i <- 0 until cb ) yield {
				mem.dat_en_w(i) := false.B
				mem.dat_en_r(i) := (stateDnxt === dl1_state_cread) & is_memory
				mem.tag_en_w(i) := false.B
				mem.tag_en_r(i) := (stateDnxt === dl1_state_cread) | ( stateDnxt === dl1_state_write & is_memory )
			}

			mem.dat_info_wstrb := DontCare
			mem.dat_info_w := DontCare


		}
		is (dl1_state_cread) {
			mem.cache_addr := op1_align128

			for ( i <- 0 until cb ) yield {
				mem.dat_en_w(i) := false.B
				mem.dat_en_r(i) := false.B
				mem.tag_en_w(i) := (stateDnxt === dl1_state_cmiss) & is_block_replace(i.U)
				mem.tag_en_r(i) := false.B			
			}

			mem.dat_info_wstrb := DontCare
			mem.dat_info_w := DontCare

		}
		is (dl1_state_mwait) {
			mem.cache_addr := op1_align128

			for ( i <- 0 until cb ) yield {
				mem.dat_en_w(i) := false.B
				mem.dat_en_r(i) := false.B
				mem.tag_en_w(i) := (stateDnxt === dl1_state_cmiss) & is_block_replace(i.U)
				mem.tag_en_r(i) := false.B
			}
			mem.dat_info_wstrb := DontCare
			mem.dat_info_w := DontCare

		}
		is (dl1_state_cmiss) {
			mem.cache_addr := op1_dl1_req

			for ( i <- 0 until cb ) yield {
				mem.dat_en_w(i) := is_cb_vhit(i) & dl1_mst.is_accessAckData
				mem.dat_en_r(i) := false.B
				mem.tag_en_w(i) := false.B
				mem.tag_en_r(i) := dl1_mst.is_chn_a_ack
			}
			mem.dat_info_wstrb := "b1111111111111111".U
			mem.dat_info_w := dl1_mst.data_ack

		}
		is (dl1_state_write) {
			mem.cache_addr := op1_align128
			for ( i <- 0 until cb ) yield {
				mem.dat_en_w(i) := is_cb_vhit(i)
				mem.dat_en_r(i) := false.B
				mem.tag_en_w(i) := false.B
				mem.tag_en_r(i) := false.B			
			}
			mem.dat_info_wstrb := lsu_wstrb_align
			mem.dat_info_w := lsu_wdata_align
	
		}
		is (dl1_state_pwait) {
			mem.cache_addr := DontCare
			for ( i <- 0 until cb ) yield {
				mem.dat_en_w(i) := false.B
				mem.dat_en_r(i) := false.B
				mem.tag_en_w(i) := false.B
				mem.tag_en_r(i) := false.B
			}
			mem.dat_info_wstrb := DontCare
			mem.dat_info_w := DontCare

		}
		is (dl1_state_pread) {
			mem.cache_addr := DontCare
			for ( i <- 0 until cb ) yield {
				mem.dat_en_w(i) := false.B
				mem.dat_en_r(i) := false.B
				mem.tag_en_w(i) := false.B
				mem.tag_en_r(i) := false.B
			}
			mem.dat_info_wstrb := DontCare
			mem.dat_info_w := DontCare

		}
		is (dl1_state_fence) {
			mem.cache_addr := DontCare
			for ( i <- 0 until cb ) yield {
				mem.dat_en_w(i) := false.B
				mem.dat_en_r(i) := false.B
				mem.tag_en_w(i) := false.B
				mem.tag_en_r(i) := false.B
			}
			mem.dat_info_wstrb := DontCare
			mem.dat_info_w := DontCare

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
		var res = LFSR(log2Ceil(cb), true.B )
		for ( i <- 0 until cb ) {
			if ( cache_valid(i)(cl_sel) == false.B ){
				res = i.U
			}
		}
		return res

	}

	def is_block_replace(i:UInt) = UIntToOH(replace_sel)(i).asBool



	def mem_dat: UInt = {
		var res = 0.U
		for ( i <- 0 until cb ) {
			if ( is_cb_vhit(i) == true.B ) { res = mem.dat_info_r(i) }
		}
		return res
	} 









// TTTTTTTTTTTTTTTTTTTTTTTXXXXXXX       XXXXXXX
// T:::::::::::::::::::::TX:::::X       X:::::X
// T:::::::::::::::::::::TX:::::X       X:::::X
// T:::::TT:::::::TT:::::TX::::::X     X::::::X
// TTTTTT  T:::::T  TTTTTTXXX:::::X   X:::::XXX
//         T:::::T           X:::::X X:::::X   
//         T:::::T            X:::::X:::::X    
//         T:::::T             X:::::::::X     
//         T:::::T             X:::::::::X     
//         T:::::T            X:::::X:::::X    
//         T:::::T           X:::::X X:::::X   
//         T:::::T        XXX:::::X   X:::::XXX
//       TT:::::::TT      X::::::X     X::::::X
//       T:::::::::T      X:::::X       X:::::X
//       T:::::::::T      X:::::X       X:::::X
//       TTTTTTTTTTT      XXXXXXX       XXXXXXX





	def wtb_wstrb = wtb.io.data_o.wstrb
	def wtb_data  = wtb.io.data_o.data
	def wtb_addr  = wtb.io.data_o.addr



	wtb.io.data_i.data  := lsu_wdata_align
	wtb.io.data_i.wstrb := lsu_wstrb_align
	wtb.io.data_i.addr  := op1_align64

	
	wtb.io.pop := (sys_mst.is_accessAck | dl1_mst.is_accessAck)
	wtb.io.push := stateDnxt === dl1_state_write
	wtb.io.commit := io.is_store_commit





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

	val is_fence = RegInit(false.B)
	val is_fence_end = RegInit(false.B)
	val is_l2c_fence_req = RegInit(false.B)
	val is_l3c_fence_req = RegInit(false.B)
	val is_l2c_fence_end = RegInit(false.B)
	val is_l3c_fence_end = RegInit(false.B)

	def is_dl1_fence_end = wtb.empty


	if ( is_fence == false.B && io.is_fence_commit == true.B ) {
		is_fence := true.B
	}

	if ( is_dl1_fence_end == true.B && is_fence == true.B ) {
		if ( io.l2c_fence_end == false.B && is_l2c_fence_req == false.B ) {
			is_l2c_fence_req := true.B
		}
		if ( io.l3c_fence_end == false.B && is_l3c_fence_req == false.B ) {
			is_l3c_fence_req := true.B
		}
	}

	if ( io.l2c_fence_end == true.B ) {
		is_l2c_fence_req := false.B
		is_l2c_fence_end := true.B
	}

	if ( io.l3c_fence_end == true.B ) {
		is_l3c_fence_req := false.B
		is_l3c_fence_end := true.B
	}

	if ( stateDnxt == dl1_state_cfree ) {
		is_fence := false.B
		is_l2c_fence_end := false.B
		is_l3c_fence_end := false.B
		is_fence_end := false.B
	}

	if ( (is_dl1_fence_end == true.B && lsu_fence_i == true.B) || (is_l3c_fence_end == true.B) || ( io.flush == true.B && is_fence == false.B) ) {
		is_fence_end := true.B
	}


	val trans_kill = RegInit(false.B)

	when( stateDnxt === dl1_state_cfree & stateReg === dl1_state_cfree ) {
		if ( io.flush == false.B )
			trans_kill := false.B
	}
	.otherwise{
		trans_kill := io.flush
	}


}
