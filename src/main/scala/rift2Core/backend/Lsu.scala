/*
* @Author: Ruige Lee
* @Date:   2021-03-29 14:37:18
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-08 16:22:42
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
import rift2Core.define._
import rift2Core.cache._
import tilelink._
import axi._
import chisel3.experimental.ChiselEnum







class Lsu extends Module {
	val io = IO( new Bundle{
		val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
		val lsu_exe_iwb = new DecoupledIO(new Exe_iwb_info)

		val dl1_chn_a = new DecoupledIO(new TLchannel_a(128, 32))
		val dl1_chn_d = Flipped(new DecoupledIO( new TLchannel_d(128) ))

		val sys_chn_ar = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
		val sys_chn_r = Flipped( new DecoupledIO(new AXI_chn_r( 64, 1, 1)) )

		val sys_chn_aw = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
		val sys_chn_w = new DecoupledIO(new AXI_chn_w( 64, 1 )) 
		val sys_chn_b = Flipped( new DecoupledIO(new AXI_chn_b( 1, 1 )))


		val cmm_lsu = Input(new Info_cmm_lsu)
		val lsu_cmm = Output( new Info_lsu_cmm )

		val il1_fence_req = Output(Bool())
		val l2c_fence_req = Output(Bool())
		val l3c_fence_req = Output(Bool())


		val flush = Input(Bool())
	})

	val lsu_exe_iwb_fifo = Module( new Queue( new Exe_iwb_info, 1, true, false ) )
	io.lsu_exe_iwb <> lsu_exe_iwb_fifo.io.deq
	lsu_exe_iwb_fifo.reset := reset.asBool | io.flush

	def dw = 128
	def bk = 2
	def cb = 4
	def cl = 64

	val addr_lsb = log2Ceil(dw*bk/8)
	val line_w   = log2Ceil(cl)
	val tag_w    = 32 - addr_lsb - line_w

	val stateReg = RegInit(Dl1_state.cfree)
	val cache_valid = Reg( Vec(cl, Vec(cb, Bool() )) )
	val mem = new Cache_mem( dw, 32, bk, cb, cl )

	val op1_dl1_req = RegInit(0.U(32.W))

	val random_res = LFSR(log2Ceil(cb), true.B )


	val is_fence = RegInit(false.B)
	val is_fence_i = RegInit(false.B)


	val trans_kill = RegInit(false.B)

	val sys_mst_r = Module(new AXI_mst_r(32, 64, 1, 1, 1 ))
	val sys_mst_w = Module(new AXI_mst_w(32, 64, 1, 1, 1 ))
	val dl1_mst = Module(new TileLink_mst_heavy(128,32, 2))




	val is_cb_vhit = Wire(Vec(cb, Bool()))


	
	// lsu_exe_iwb_fifo.io.enq.fire



	val lsu_addr_reg = RegInit(0.U(64.W))
	val lsu_addr = Wire(UInt(64.W))

	lsu_addr_reg := Mux( stateReg === Dl1_state.cfree, io.lsu_iss_exe.bits.param.op1, lsu_addr_reg )
	lsu_addr     := Mux( stateReg === Dl1_state.cfree, io.lsu_iss_exe.bits.param.op1, lsu_addr_reg )

	def op1 = lsu_addr
	def op2 = io.lsu_iss_exe.bits.param.op2
	def op1_tag = op1(31,32-tag_w)
	def op1_align64  = op1(31,0) & ~("b111".U(64.W))
	def op1_align128 = op1(31,0) & ~("b1111".U(64.W))
	def op1_align256 = op1(31,0) & ~("b11111".U(64.W))

	io.lsu_cmm.is_accessFault := is_accessFalut
	io.lsu_cmm.is_misAlign := is_misAlign
	io.lsu_cmm.trap_addr := op1

	val cl_sel = op1(addr_lsb+line_w-1, addr_lsb)


	val lsu_wstrb_align = (MuxCase( 0.U, Array(
						lsu_sb -> "b00000001".U,
						lsu_sh -> "b00000011".U,
						lsu_sw -> "b00001111".U,
						lsu_sd -> "b11111111".U
						)) << op1(2,0))

	val lsu_wdata_align = op2 << (op1(2,0) << 3)








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
	def is_misAlign    = io.lsu_iss_exe.valid & MuxCase( false.B, Array(
												(lsu_lh | lsu_lhu | lsu_sh ) -> (op1(0) =/= 0.U),
												(lsu_lw | lsu_lwu | lsu_sw ) -> (op1(1,0) =/= 0.U ),
												(lsu_ld | lsu_sd)            -> (op1(2,0) =/= 0.U)	
											))


	def is_ren = io.lsu_iss_exe.bits.fun.is_lu
	def is_wen = io.lsu_iss_exe.bits.fun.is_su
	def is_memory = (op1(63,32) === 0.U) & (op1(31) === 1.U)
	def is_system = (op1(63,32) === 0.U) & (op1(31,30) === 0.U)
	






	def reAlign8(rdata: UInt): UInt = MuxLookup(op1(2,0), 0.U, Array(
			"b000".U -> rdata(7,0),   "b001".U -> rdata(15,8),  "b010".U -> rdata(23,16), "b011".U -> rdata(31,24),
			"b100".U -> rdata(39,32), "b101".U -> rdata(47,40), "b110".U -> rdata(55,48), "b111".U -> rdata(63,56)))

	def reAlign16(rdata: UInt): UInt = MuxLookup(op1(2,1), 0.U, Array(
			"b00".U -> rdata(15,0), "b01".U -> rdata(31,16),
			"b10".U -> rdata(47,32), "b11".U -> rdata(63,48)))

	def reAlign32(rdata: UInt): UInt = MuxLookup(op1(2), 0.U, Array(
									0.U -> rdata(31,0), 1.U -> rdata(63,32)))


	def load_byte(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(56, Mux(is_usi, 0.U, rdata(7)) ),  rdata(7,0)  )
	def load_half(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(48, Mux(is_usi, 0.U, rdata(15)) ), rdata(15,0) )
	def load_word(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(32, Mux(is_usi, 0.U, rdata(31)) ), rdata(31,0) )


	lsu_exe_iwb_fifo.io.enq.bits.rd0_raw := io.lsu_iss_exe.bits.param.rd0_raw
	lsu_exe_iwb_fifo.io.enq.bits.rd0_idx := io.lsu_iss_exe.bits.param.rd0_idx


	io.lsu_iss_exe.ready := stateReg =/= Dl1_state.cfree & stateDnxt === Dl1_state.cfree & ~trans_kill
	lsu_exe_iwb_fifo.io.enq.valid := 
		( stateReg === Dl1_state.cread & stateDnxt === Dl1_state.cfree ) |
		( stateReg === Dl1_state.cmiss & op1_dl1_req === op1_align128 & dl1_mst.io.d.fire & ~trans_kill ) |
		( stateReg === Dl1_state.cfree & stateDnxt === Dl1_state.wwait ) |
		( stateReg === Dl1_state.cfree & stateDnxt === Dl1_state.fence & lsu_fence ) |
		( stateReg === Dl1_state.fence & stateDnxt === Dl1_state.cfree & lsu_fence_i ) |
		( stateReg === Dl1_state.pread & stateDnxt === Dl1_state.cfree & trans_kill === false.B)



	lsu_exe_iwb_fifo.io.enq.bits.res := res

	def res = MuxCase( DontCare, Array(
		io.lsu_iss_exe.bits.fun.lb        -> load_byte(false.B, reAlign8 (rsp_data)),
		io.lsu_iss_exe.bits.fun.lh        -> load_half(false.B, reAlign16(rsp_data)),
		io.lsu_iss_exe.bits.fun.lw        -> load_word(false.B, reAlign32(rsp_data)),
		io.lsu_iss_exe.bits.fun.ld        -> rsp_data,
		io.lsu_iss_exe.bits.fun.lbu       -> load_byte(true.B, reAlign8 (rsp_data)),
		io.lsu_iss_exe.bits.fun.lhu       -> load_half(true.B, reAlign16(rsp_data)),
		io.lsu_iss_exe.bits.fun.lwu       -> load_word(true.B, reAlign32(rsp_data)),
		io.lsu_iss_exe.bits.fun.sb        -> 0.U,
		io.lsu_iss_exe.bits.fun.sh        -> 0.U,
		io.lsu_iss_exe.bits.fun.sw        -> 0.U,
		io.lsu_iss_exe.bits.fun.sd        -> 0.U,
		io.lsu_iss_exe.bits.fun.fence     -> 0.U,
		io.lsu_iss_exe.bits.fun.fence_i   -> 0.U,

		io.lsu_iss_exe.bits.fun.lr_w      -> load_word(is_usi, reAlign32(rsp_data)),
		io.lsu_iss_exe.bits.fun.sc_w      -> 0.U,
		io.lsu_iss_exe.bits.fun.amoswap_w -> load_word(is_usi, reAlign32(rsp_data)),
		io.lsu_iss_exe.bits.fun.amoadd_w  -> load_word(is_usi, reAlign32(rsp_data)),
		io.lsu_iss_exe.bits.fun.amoxor_w  -> load_word(is_usi, reAlign32(rsp_data)),
		io.lsu_iss_exe.bits.fun.amoand_w  -> load_word(is_usi, reAlign32(rsp_data)),
		io.lsu_iss_exe.bits.fun.amoor_w   -> load_word(is_usi, reAlign32(rsp_data)),
		io.lsu_iss_exe.bits.fun.amomin_w  -> load_word(is_usi, reAlign32(rsp_data)),
		io.lsu_iss_exe.bits.fun.amomax_w  -> load_word(is_usi, reAlign32(rsp_data)),
		io.lsu_iss_exe.bits.fun.amominu_w -> load_word(is_usi, reAlign32(rsp_data)),
		io.lsu_iss_exe.bits.fun.amomaxu_w -> load_word(is_usi, reAlign32(rsp_data)),
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

		io.lsu_iss_exe.bits.fun.flw       -> load_word(is_usi, reAlign32(rsp_data)),
		io.lsu_iss_exe.bits.fun.fsw       -> 0.U,
		io.lsu_iss_exe.bits.fun.fld       -> rsp_data,
		io.lsu_iss_exe.bits.fun.fsd       -> 0.U,
	))


	def rsp_data: UInt = MuxCase( 0.U, Array(
		( stateReg === Dl1_state.cfree ) -> 0.U,
		( stateReg === Dl1_state.cread ) -> Mux(op1(3), mem_dat(127,64), mem_dat(63,0)),
		( stateReg === Dl1_state.wwait ) -> 0.U,
		( stateReg === Dl1_state.cmiss ) -> Mux(op1(3), dl1_mst.io.d.bits.data(127,64), dl1_mst.io.d.bits.data(63,0)),
		( stateReg === Dl1_state.write ) -> 0.U,
		( stateReg === Dl1_state.fence ) -> 0.U,
		// ( stateReg === Dl1_state.pwait ) -> 0.U,
		( stateReg === Dl1_state.pread ) -> sys_mst_r.io.r.bits.data
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
	


	sys_mst_r.io.ar_info.id    := 1.U
	sys_mst_r.io.ar_info.addr  := op1_align64
	sys_mst_r.io.ar_info.len   := 1.U
	sys_mst_r.io.ar_info.size  := 3.U
	sys_mst_r.io.ar_info.burst := 0.U
	sys_mst_r.io.ar_info.lock  := 0.U
	sys_mst_r.io.ar_info.cache := 0.U
	sys_mst_r.io.ar_info.port  := 0.U
	sys_mst_r.io.ar_info.qos   := 0.U
	sys_mst_r.io.ar_info.user  := 0.U

	sys_mst_w.io.aw_info.id    := 1.U
	sys_mst_w.io.aw_info.addr  := op1_align64
	sys_mst_w.io.aw_info.len   := 1.U
	sys_mst_w.io.aw_info.size  := 3.U
	sys_mst_w.io.aw_info.burst := 0.U
	sys_mst_w.io.aw_info.lock  := 0.U
	sys_mst_w.io.aw_info.cache := 0.U
	sys_mst_w.io.aw_info.port  := 0.U
	sys_mst_w.io.aw_info.qos   := 0.U
	sys_mst_w.io.aw_info.user  := 0.U

	sys_mst_w.io.w_info.data := lsu_wdata_align
	sys_mst_w.io.w_info.strb := lsu_wstrb_align
	sys_mst_w.io.w_info.last := DontCare
	sys_mst_w.io.w_info.user := 0.U

	sys_mst_r.io.ar_req := (stateReg =/= Dl1_state.pread) & (stateDnxt === Dl1_state.pread)
	sys_mst_w.io.aw_req := (stateReg === Dl1_state.wwait & is_system & io.cmm_lsu.is_store_commit)





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



	dl1_mst.io.a_info.opcode  := Mux1H( Seq(
		(stateDnxt === Dl1_state.cmiss) -> dl1_mst.Get,
		(stateReg === Dl1_state.wwait | stateReg === Dl1_state.write) -> dl1_mst.PutFullData
	))
	

	dl1_mst.io.a_info.param   := Mux1H( Seq(
		(stateReg === Dl1_state.cmiss) -> 0.U,
		(stateReg === Dl1_state.wwait | stateReg === Dl1_state.write) -> 0.U
	))

	dl1_mst.io.a_info.size    := Mux1H( Seq(
		(stateDnxt === Dl1_state.cmiss) -> 5.U,
		(stateReg === Dl1_state.wwait | stateReg === Dl1_state.write ) -> 4.U
	))

	dl1_mst.io.a_info.source  := 1.U

	dl1_mst.io.a_info.address := Mux1H( Seq(
		(stateReg === Dl1_state.cmiss) -> op1_align256,
		(stateReg === Dl1_state.wwait | stateReg === Dl1_state.write) -> op1_align128
	))

	dl1_mst.io.a_info.mask    := Mux1H( Seq(
		(stateReg === Dl1_state.cmiss) -> 0.U,
		(stateReg === Dl1_state.wwait | stateReg === Dl1_state.write) -> Mux( op1_align64(3), Cat( lsu_wstrb_align, 0.U(8.W) ), Cat(  0.U(8.W), lsu_wstrb_align ) )
	))

	dl1_mst.io.a_info.data    := Mux1H( Seq(
		(stateReg === Dl1_state.cmiss) -> 0.U,
		(stateReg === Dl1_state.wwait | stateReg === Dl1_state.write) -> Mux( op1_align64(3), Cat( lsu_wdata_align, 0.U(64.W) ), Cat(  0.U(64.W), lsu_wdata_align ) )
	))

	dl1_mst.io.a_info.corrupt := false.B

	dl1_mst.io.is_req :=
		( stateReg === Dl1_state.cread & stateDnxt === Dl1_state.cmiss ) | 
		( stateReg === Dl1_state.wwait & is_memory & io.cmm_lsu.is_store_commit) 






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



	object Dl1_state extends ChiselEnum {
		val cfree, cread, cmiss, wwait, write, fence, pread = Value
	}




	// val dl1_state_cfree :: dl1_state_cread :: dl1_state_mwait :: dl1_state_cmiss :: dl1_state_write :: dl1_state_fence :: dl1_state_pwait :: dl1_state_pread :: Nil = Enum(8)



	stateReg := stateDnxt




	def dl1_state_dnxt_in_cfree = {
		Mux(
			( io.lsu_iss_exe.valid & (io.lsu_iss_exe.bits.fun.fence | io.lsu_iss_exe.bits.fun.fence_i) & ~io.flush ),
			Dl1_state.fence,
			Mux(
				(io.lsu_iss_exe.valid & ~is_accessFalut & ~is_misAlign & ~io.flush),
				MuxCase(Dl1_state.cfree, Array(
					(is_ren & is_memory) -> Dl1_state.cread,
					(is_wen) -> Dl1_state.wwait,
					// (is_ren & is_system &  is_sys_hazard ) -> Dl1_state.pwait,
					(is_ren & is_system ) -> Dl1_state.pread			
				)),
				Dl1_state.cfree
			)
		)
	}

	def dl1_state_dnxt_in_cread = Mux( is_cb_vhit.contains(true.B), Dl1_state.cfree, Dl1_state.cmiss )


	// def dl1_state_dnxt_in_mwait = Mux( is_mem_hazard | ~lsu_exe_iwb_fifo.io.enq.ready, Dl1_state.mwait, Dl1_state.cmiss )
	def dl1_state_dnxt_in_cmiss = Mux( dl1_mst.io.mode === 7.U, Dl1_state.cfree, Dl1_state.cmiss )
	def dl1_state_dnxt_in_write = Mux( dl1_mst.io.d.fire | sys_mst_w.io.b.fire , Dl1_state.cfree, Dl1_state.write)
	def dl1_state_dnxt_in_fence = Dl1_state.cfree
	def dl1_state_dnxt_in_wwait =
								MuxCase( Dl1_state.wwait, Array(
									io.cmm_lsu.is_store_commit -> Dl1_state.write,
									io.flush -> Dl1_state.cfree
									))
	def dl1_state_dnxt_in_pread = Mux( sys_mst_r.io.end, Dl1_state.cfree, Dl1_state.pread )

	def stateDnxt = MuxCase( Dl1_state.cfree, Array(
						(stateReg === Dl1_state.cfree) -> dl1_state_dnxt_in_cfree ,
						(stateReg === Dl1_state.cread) -> dl1_state_dnxt_in_cread ,
						// (stateReg === Dl1_state.mwait) -> dl1_state_dnxt_in_mwait ,
						(stateReg === Dl1_state.cmiss) -> dl1_state_dnxt_in_cmiss ,
						(stateReg === Dl1_state.write) -> dl1_state_dnxt_in_write ,
						(stateReg === Dl1_state.fence) -> dl1_state_dnxt_in_fence ,
						(stateReg === Dl1_state.wwait) -> dl1_state_dnxt_in_wwait ,
						(stateReg === Dl1_state.pread) -> dl1_state_dnxt_in_pread ,
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







	println("the dcache has "+dw+"*"+bk+"bit,with "+cb+"cache block,and "+cl+"cache line")
	println("Toltal size is "+dw*bk*cb*cl/8/1024.0+"KB")



	when( stateReg =/= Dl1_state.cmiss & stateDnxt === Dl1_state.cmiss ) {
		op1_dl1_req := op1_align256
	}
	.elsewhen( stateDnxt === Dl1_state.cmiss ) {
		when ( dl1_mst.io.d.fire === true.B ) {
			op1_dl1_req := op1_dl1_req + "b10000".U
		}
	}

	mem.cache_addr := Mux1H( Seq(
							(stateReg === Dl1_state.cfree) -> op1_align128,
							(stateReg === Dl1_state.cread) -> op1_align128,
							// (stateReg === Dl1_state.mwait) -> op1_align128,
							(stateReg === Dl1_state.cmiss) -> op1_dl1_req,
							(stateReg === Dl1_state.write) -> op1_align128,
							(stateReg === Dl1_state.wwait) -> op1_align128,
							(stateReg === Dl1_state.pread) -> 0.U,
							(stateReg === Dl1_state.fence) -> 0.U
							))

	mem.dat_info_w := Mux1H( Seq(
							(stateReg === Dl1_state.cfree) -> 0.U,
							(stateReg === Dl1_state.cread) -> 0.U,
							// (stateReg === Dl1_state.mwait) -> 0.U,
							(stateReg === Dl1_state.cmiss) -> dl1_mst.io.d.bits.data,
							(stateReg === Dl1_state.write) -> Mux( op1(3) === 0.U, Cat(0.U(64.W), lsu_wdata_align(63,0)), Cat(lsu_wdata_align(63,0), 0.U(64.W))),
							(stateReg === Dl1_state.wwait) -> Mux( op1(3) === 0.U, Cat(0.U(64.W), lsu_wdata_align(63,0)), Cat(lsu_wdata_align(63,0), 0.U(64.W))),
							(stateReg === Dl1_state.pread) -> 0.U,
							(stateReg === Dl1_state.fence) -> 0.U
							))

	mem.dat_info_wstrb := Mux1H( Seq(
							(stateReg === Dl1_state.cfree) -> 0.U,
							(stateReg === Dl1_state.cread) -> 0.U,
							// (stateReg === Dl1_state.mwait) -> 0.U,
							(stateReg === Dl1_state.cmiss) -> "b1111111111111111".U,
							(stateReg === Dl1_state.write) -> Mux( op1(3) === 0.U, Cat(0.U(8.W), lsu_wstrb_align(7,0)), Cat(lsu_wstrb_align(7,0), 0.U(8.W))),
							(stateReg === Dl1_state.wwait) -> Mux( op1(3) === 0.U, Cat(0.U(8.W), lsu_wstrb_align(7,0)), Cat(lsu_wstrb_align(7,0), 0.U(8.W))),
							(stateReg === Dl1_state.pread) -> 0.U,
							(stateReg === Dl1_state.fence) -> 0.U
							))

	for ( i <- 0 until cb ) yield {
		mem.dat_en_w(i) := Mux1H( Seq(
							(stateReg === Dl1_state.cfree) -> false.B,
							(stateReg === Dl1_state.cread) -> false.B,
							// (stateReg === Dl1_state.mwait) -> false.B,
							(stateReg === Dl1_state.cmiss) -> (is_cb_vhit(i) & dl1_mst.io.d.fire),
							(stateReg === Dl1_state.write & dl1_mst.io.d.fire ) -> is_cb_vhit(i),
							(stateReg === Dl1_state.wwait) -> false.B,
							(stateReg === Dl1_state.pread) -> false.B,
							(stateReg === Dl1_state.fence) -> false.B
							))
							
		mem.dat_en_r(i) := Mux1H( Seq(
							(stateReg === Dl1_state.cfree) -> ((stateDnxt === Dl1_state.cread) & is_memory),
							(stateReg === Dl1_state.cread) -> false.B,
							// (stateReg === Dl1_state.mwait) -> false.B,
							(stateReg === Dl1_state.cmiss) -> false.B,
							(stateReg === Dl1_state.write) -> false.B,
							(stateReg === Dl1_state.wwait) -> false.B,
							(stateReg === Dl1_state.pread) -> false.B,
							(stateReg === Dl1_state.fence) -> false.B
							))	

		mem.tag_en_w(i) := Mux1H( Seq(
							(stateReg === Dl1_state.cfree) -> false.B,
							(stateReg === Dl1_state.cread) -> ((stateDnxt === Dl1_state.cmiss) & is_block_replace(i.U)),
							// (stateReg === Dl1_state.mwait) -> ((stateDnxt === Dl1_state.cmiss) & is_block_replace(i.U)),
							(stateReg === Dl1_state.cmiss) -> false.B,
							(stateReg === Dl1_state.write) -> false.B,
							(stateReg === Dl1_state.wwait) -> false.B,
							(stateReg === Dl1_state.pread) -> false.B,
							(stateReg === Dl1_state.fence) -> false.B
							))	

		mem.tag_en_r(i) := Mux1H( Seq(
							(stateReg === Dl1_state.cfree) -> ((stateDnxt === Dl1_state.cread) | ( stateDnxt === Dl1_state.write & is_memory )),
							(stateReg === Dl1_state.cread) -> false.B,
							// (stateReg === Dl1_state.mwait) -> false.B,
							(stateReg === Dl1_state.cmiss) -> dl1_mst.io.a.fire,
							(stateReg === Dl1_state.write) -> false.B,
							(stateReg === Dl1_state.wwait) -> false.B,
							(stateReg === Dl1_state.pread) -> false.B,
							(stateReg === Dl1_state.fence) -> false.B
							))				
	}





	when ( reset.asBool ) {
		for ( i <- 0 until cl; j <- 0 until cb ) yield cache_valid(i)(j) := false.B
	}
	.elsewhen(
				 ( stateReg === Dl1_state.cread & stateDnxt === Dl1_state.cmiss)
				// |( stateReg === Dl1_state.mwait & stateDnxt === Dl1_state.cmiss )
			) {
		cache_valid(cl_sel)(replace_sel) := true.B
	}
	.elsewhen(stateReg === Dl1_state.fence & stateDnxt === Dl1_state.cfree & ~trans_kill) {
		for ( i <- 0 until cl; j <- 0 until cb ) yield cache_valid(i)(j) := false.B
	}

	
	for ( i <- 0 until cb ) yield { is_cb_vhit(i) := cache_valid(cl_sel)(i) & ( mem.tag_info_r(i) === op1_tag ) }





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
		MuxCase( 0.U, cb_num zip dat_sel )		
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



	io.il1_fence_req := is_fence_i
	io.l2c_fence_req := is_fence
	io.l3c_fence_req := is_fence

	when ( ~is_fence & io.cmm_lsu.is_fence_commit ) { is_fence := true.B }
	.elsewhen(stateDnxt === Dl1_state.cfree) { is_fence := false.B }

	when ( io.lsu_iss_exe.fire & lsu_fence_i ) { is_fence_i := true.B }
	.elsewhen(stateDnxt === Dl1_state.cfree) { is_fence_i := false.B }


	when ( io.flush & stateDnxt =/= Dl1_state.cfree ) { trans_kill := true.B }
	.elsewhen( stateDnxt === Dl1_state.cfree ) { trans_kill := false.B	 }










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


	io.sys_chn_ar <> sys_mst_r.io.ar
	io.sys_chn_r  <> sys_mst_r.io.r
	io.sys_chn_aw <> sys_mst_w.io.aw
	io.sys_chn_w  <> sys_mst_w.io.w
	io.sys_chn_b  <> sys_mst_w.io.b

	io.dl1_chn_a  <> dl1_mst.io.a
	io.dl1_chn_d  <> dl1_mst.io.d






}