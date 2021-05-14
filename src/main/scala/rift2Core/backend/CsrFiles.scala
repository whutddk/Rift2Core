/*
* @Author: Ruige Lee
* @Date:   2021-03-30 11:06:02
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-15 09:53:20
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
import rift2Core.define._



class Exe_Port extends Bundle {
	val addr = UInt(12.W)
	val dat_i = UInt(64.W)
	val op_rw = Bool()
	val op_rs = Bool()
	val op_rc = Bool()
}

class Pri_Port extends Bundle {
	val en = Bool()
	val dat = UInt(64.W)
}



// class CsrReg( addr: UInt, init: UInt, ormask: UInt ) extends Module{
// 	val io = IO(new Bundle{
// 		val pri_port = Input(new Pri_Port)
// 		val exe_port = Input(new Exe_Port)
// 		val value = Output(UInt(64.W))
// 	})

// 	val value = RegInit(init)

// 	io.value := value

// 	when( io.pri_port.en ) {
// 		value := io.pri_port.dat
// 	}
// 	.elsewhen(io.exe_port.addr === addr) {
// 		value := MuxCase(value, Array(
// 			io.exe_port.op_rw -> (io.exe_port.dat_i),
// 			io.exe_port.op_rs -> (value | io.exe_port.dat_i),
// 			io.exe_port.op_rc -> (value & ~io.exe_port.dat_i),
// 		)) | ormask 
// 	}
// }

object CsrReg {
	def apply( addr: UInt, init: UInt, ormask: UInt, pp: Pri_Port, ep: Exe_Port ) = {
		val csr_reg = RegInit(init)

		when( pp.en ) {
			csr_reg := pp.dat | ormask 
		}
		.elsewhen(ep.addr === addr) {
			csr_reg := MuxCase(csr_reg, Array(
				ep.op_rw -> ( ep.dat_i),
				ep.op_rs -> (csr_reg | ep.dat_i),
				ep.op_rc -> (csr_reg & ~ep.dat_i),
			)) | ormask 
		}

		csr_reg

		// val mdl = Module(new CsrReg( addr, init, ormask ))
		// mdl.io.pri_port := pp
		// mdl.io.exe_port := ep
		// mdl
	}
}



class U_CsrFiles {
	val exe_port = Wire(new Exe_Port)

	//user trap setup
	val ustatus   = CsrReg( "h000".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val uie       = CsrReg( "h004".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val utvec     = CsrReg( "h005".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)

	//user trap handling
	val uscratch  = CsrReg( "h040".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val uepc      = CsrReg( "h041".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val ucause    = CsrReg( "h042".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val utval     = CsrReg( "h043".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val uip       = CsrReg( "h044".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)

	//user floating point csrs
	val fflags    = CsrReg( "h001".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val frm       = CsrReg( "h002".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val fcsr      = CsrReg( "h003".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)

	//user conter timers
	val cycle        = CsrReg( "hC00".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val time         = CsrReg( "hC01".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val instret      = CsrReg( "hC02".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter3  = CsrReg( "hC03".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter4  = CsrReg( "hC04".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter5  = CsrReg( "hC05".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter6  = CsrReg( "hC06".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter7  = CsrReg( "hC07".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter8  = CsrReg( "hC08".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter9  = CsrReg( "hC09".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter10 = CsrReg( "hC0A".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter11 = CsrReg( "hC0B".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter12 = CsrReg( "hC0C".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter13 = CsrReg( "hC0D".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter14 = CsrReg( "hC0E".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter15 = CsrReg( "hC0F".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter16 = CsrReg( "hC10".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter17 = CsrReg( "hC11".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter18 = CsrReg( "hC12".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter19 = CsrReg( "hC13".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter20 = CsrReg( "hC14".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter21 = CsrReg( "hC15".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter22 = CsrReg( "hC16".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter23 = CsrReg( "hC17".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter24 = CsrReg( "hC18".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter25 = CsrReg( "hC19".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter26 = CsrReg( "hC1A".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter27 = CsrReg( "hC1B".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter28 = CsrReg( "hC1C".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter29 = CsrReg( "hC1D".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter30 = CsrReg( "hC1E".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hpmcounter31 = CsrReg( "hC1F".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
}

class S_CsrFiles {
	val exe_port = Wire(new Exe_Port)

	//supervisor trap setup
	val sstatus    = CsrReg( "h100".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val sedeleg    = CsrReg( "h102".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val sideleg    = CsrReg( "h103".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val sie        = CsrReg( "h104".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val stvec      = CsrReg( "h105".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val scounteren = CsrReg( "h106".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)

	//supervisor trap handling

	val sscratch = CsrReg( "h140".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val sepc     = CsrReg( "h141".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val scause   = CsrReg( "h142".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val stval    = CsrReg( "h143".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val sip      = CsrReg( "h144".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)

	//supervisor protection and translation
	val satp     = CsrReg( "h180".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
}

class H_CsrFiles {
	val exe_port = Wire(new Exe_Port)

	//hypervisor trap setup
	val hstatus    = CsrReg( "h600".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hedeleg    = CsrReg( "h602".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hideleg    = CsrReg( "h603".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hie        = CsrReg( "h604".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hcounteren = CsrReg( "h606".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hgeie      = CsrReg( "h607".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)

	//hypervisor trap handling
	val htval  = CsrReg( "h643".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hip    = CsrReg( "h644".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hvip   = CsrReg( "h645".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val htinst = CsrReg( "h64A".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val hgeip  = CsrReg( "hE12".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)

	//hypervisor protection and translation
	val hgatp  = CsrReg( "h680".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)

	//hypervisor counter timer virtualization registers
	val htimedelta    = CsrReg( "h605".U, 0.U(64.W),  0.U, 0.U.asTypeOf(new Pri_Port), exe_port)

	//virtual supervisor registers
	val vsstatus    = CsrReg( "h200".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val vsie        = CsrReg( "h204".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val vstvec      = CsrReg( "h205".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val vsscratch   = CsrReg( "h240".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val vsepc       = CsrReg( "h241".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val vscause     = CsrReg( "h242".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val vstval      = CsrReg( "h243".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val vsip        = CsrReg( "h244".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val vsatp       = CsrReg( "h280".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
}

class M_CsrFiles {
	val exe_port = Wire(new Exe_Port)
	val clint_csr_info = Wire( new Info_clint_csr )

	val mcause_pri_port = Wire( new Pri_Port )
	val mepc_pri_port   = Wire( new Pri_Port )
	val mtval_pri_port = Wire( new Pri_Port )
	val mstatus_pri_port = Wire( new Pri_Port )













	//machine information register
	val mvendorid    = CsrReg( "hF11".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val marchid      = CsrReg( "hF12".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val mimpid       = CsrReg( "hF13".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val mhartid      = CsrReg( "hF14".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)


	//Machine Trap Setup
	val mstatus      = CsrReg( "h300".U, 0.U(64.W), "h1800".U, mstatus_pri_port, exe_port)
	// val misa         = CsrReg( "h301".U, Cat("b10".U, 0.U(36.W), "b00000101000011000110101101".U), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)//ACDFHIMNSU
	val medeleg      = CsrReg( "h302".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val mideleg      = CsrReg( "h303".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val mie          = CsrReg( "h304".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val mtvec        = CsrReg( "h305".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val mcounteren   = CsrReg( "h306".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)



	//Machine Trap Handling
	val mscratch     = CsrReg( "h340".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val mepc         = CsrReg( "h341".U, 0.U(64.W), 0.U, mepc_pri_port, exe_port)
	val mcause       = CsrReg( "h342".U, 0.U(64.W), 0.U, mcause_pri_port, exe_port)
	val mtval        = CsrReg( "h343".U, 0.U(64.W), 0.U, mtval_pri_port, exe_port)
	val mip          = CsrReg( "h344".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val mtinst       = CsrReg( "h34A".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val mtval2       = CsrReg( "h34B".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)


	//Machine Memory Protection
	val pmpcfg0    = CsrReg( "h3A0".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpcfg2    = CsrReg( "h3A2".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpcfg4    = CsrReg( "h3A4".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpcfg6    = CsrReg( "h3A6".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpcfg8    = CsrReg( "h3A8".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpcfg10   = CsrReg( "h3AA".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpcfg12   = CsrReg( "h3AC".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpcfg14   = CsrReg( "h3AE".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)


	val pmpaddr0    = CsrReg( "h3B0".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr1    = CsrReg( "h3B1".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr2    = CsrReg( "h3B2".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr3    = CsrReg( "h3B3".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr4    = CsrReg( "h3B4".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr5    = CsrReg( "h3B5".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr6    = CsrReg( "h3B6".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr7    = CsrReg( "h3B7".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr8    = CsrReg( "h3B8".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr9    = CsrReg( "h3B9".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr10   = CsrReg( "h3BA".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr11   = CsrReg( "h3BB".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr12   = CsrReg( "h3BC".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr13   = CsrReg( "h3BD".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr14   = CsrReg( "h3BE".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr15   = CsrReg( "h3BF".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr16   = CsrReg( "h3C0".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr17   = CsrReg( "h3C1".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr18   = CsrReg( "h3C2".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr19   = CsrReg( "h3C3".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val pmpaddr20   = CsrReg( "h3C4".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)



	//Machine Counter/Timer

	//0xb00
	val mcycle       = CsrReg( "hB00".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val minstret     = CsrReg( "hB02".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val mhpmcounter3 = CsrReg( "hB03".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)



	//Machine Counter Setup
	val mcountinhibit = CsrReg( "h320".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val mhpmevent3    = CsrReg( "h323".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
}

class D_CsrFiles {
	val exe_port = Wire(new Exe_Port)

	//Debug/Trace Register
	val tselect = CsrReg( "h7A0".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val tdata1  = CsrReg( "h7A1".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val tdata2  = CsrReg( "h7A2".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val tdata3  = CsrReg( "h7A3".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)

	//Debug Mode Register
	val dcsr      = CsrReg( "h7B0".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val dpc       = CsrReg( "h7B1".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val dscratch0 = CsrReg( "h7B2".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
	val dscratch1 = CsrReg( "h7B3".U, 0.U(64.W), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
}

trait CsrFiles {

	val m_csrFiles = new M_CsrFiles
	val h_csrFiles = new H_CsrFiles
	val s_csrFiles = new S_CsrFiles
	val u_csrFiles = new U_CsrFiles
	val d_csrFiles = new D_CsrFiles




	def csr_read(addr: UInt) = MuxCase(0.U, Array(
				( addr === "h000".U ) -> u_csrFiles.ustatus,
				( addr === "h004".U ) -> u_csrFiles.uie,
				( addr === "h005".U ) -> u_csrFiles.utvec,
				( addr === "h040".U ) -> u_csrFiles.uscratch,
				( addr === "h041".U ) -> u_csrFiles.uepc,
				( addr === "h042".U ) -> u_csrFiles.ucause,
				( addr === "h043".U ) -> u_csrFiles.utval,
				( addr === "h044".U ) -> u_csrFiles.uip,
				( addr === "h001".U ) -> u_csrFiles.fflags,
				( addr === "h002".U ) -> u_csrFiles.frm,
				( addr === "h003".U ) -> u_csrFiles.fcsr,
				( addr === "hC00".U ) -> u_csrFiles.cycle,
				( addr === "hC01".U ) -> u_csrFiles.time,
				( addr === "hC02".U ) -> u_csrFiles.instret,
				( addr === "hC03".U ) -> u_csrFiles.hpmcounter3,
				( addr === "hC04".U ) -> u_csrFiles.hpmcounter4,
				( addr === "hC05".U ) -> u_csrFiles.hpmcounter5,
				( addr === "hC06".U ) -> u_csrFiles.hpmcounter6,
				( addr === "hC07".U ) -> u_csrFiles.hpmcounter7,
				( addr === "hC08".U ) -> u_csrFiles.hpmcounter8,
				( addr === "hC09".U ) -> u_csrFiles.hpmcounter9,
				( addr === "hC0A".U ) -> u_csrFiles.hpmcounter10,
				( addr === "hC0B".U ) -> u_csrFiles.hpmcounter11,
				( addr === "hC0C".U ) -> u_csrFiles.hpmcounter12,
				( addr === "hC0D".U ) -> u_csrFiles.hpmcounter13,
				( addr === "hC0E".U ) -> u_csrFiles.hpmcounter14,
				( addr === "hC0F".U ) -> u_csrFiles.hpmcounter15,
				( addr === "hC10".U ) -> u_csrFiles.hpmcounter16,
				( addr === "hC11".U ) -> u_csrFiles.hpmcounter17,
				( addr === "hC12".U ) -> u_csrFiles.hpmcounter18,
				( addr === "hC13".U ) -> u_csrFiles.hpmcounter19,
				( addr === "hC14".U ) -> u_csrFiles.hpmcounter20,
				( addr === "hC15".U ) -> u_csrFiles.hpmcounter21,
				( addr === "hC16".U ) -> u_csrFiles.hpmcounter22,
				( addr === "hC17".U ) -> u_csrFiles.hpmcounter23,
				( addr === "hC18".U ) -> u_csrFiles.hpmcounter24,
				( addr === "hC19".U ) -> u_csrFiles.hpmcounter25,
				( addr === "hC1A".U ) -> u_csrFiles.hpmcounter26,
				( addr === "hC1B".U ) -> u_csrFiles.hpmcounter27,
				( addr === "hC1C".U ) -> u_csrFiles.hpmcounter28,
				( addr === "hC1D".U ) -> u_csrFiles.hpmcounter29,
				( addr === "hC1E".U ) -> u_csrFiles.hpmcounter30,
				( addr === "hC1F".U ) -> u_csrFiles.hpmcounter31,
				( addr === "h100".U ) -> s_csrFiles.sstatus,
				( addr === "h102".U ) -> s_csrFiles.sedeleg,
				( addr === "h103".U ) -> s_csrFiles.sideleg,
				( addr === "h104".U ) -> s_csrFiles.sie,
				( addr === "h105".U ) -> s_csrFiles.stvec,
				( addr === "h106".U ) -> s_csrFiles.scounteren,
				( addr === "h140".U ) -> s_csrFiles.sscratch,
				( addr === "h141".U ) -> s_csrFiles.sepc,
				( addr === "h142".U ) -> s_csrFiles.scause,
				( addr === "h143".U ) -> s_csrFiles.stval,
				( addr === "h144".U ) -> s_csrFiles.sip,
				( addr === "h180".U ) -> s_csrFiles.satp,
				( addr === "h600".U ) -> h_csrFiles.hstatus,
				( addr === "h602".U ) -> h_csrFiles.hedeleg,
				( addr === "h603".U ) -> h_csrFiles.hideleg,
				( addr === "h604".U ) -> h_csrFiles.hie,
				( addr === "h606".U ) -> h_csrFiles.hcounteren,
				( addr === "h607".U ) -> h_csrFiles.hgeie,
				( addr === "h643".U ) -> h_csrFiles.htval,
				( addr === "h644".U ) -> h_csrFiles.hip,
				( addr === "h645".U ) -> h_csrFiles.hvip,
				( addr === "h64A".U ) -> h_csrFiles.htinst,
				( addr === "hE12".U ) -> h_csrFiles.hgeip,
				( addr === "h680".U ) -> h_csrFiles.hgatp,
				( addr === "h605".U ) -> h_csrFiles.htimedelta,
				( addr === "h200".U ) -> h_csrFiles.vsstatus,
				( addr === "h204".U ) -> h_csrFiles.vsie,
				( addr === "h205".U ) -> h_csrFiles.vstvec,
				( addr === "h240".U ) -> h_csrFiles.vsscratch,
				( addr === "h241".U ) -> h_csrFiles.vsepc,
				( addr === "h242".U ) -> h_csrFiles.vscause,
				( addr === "h243".U ) -> h_csrFiles.vstval,
				( addr === "h244".U ) -> h_csrFiles.vsip,
				( addr === "h280".U ) -> h_csrFiles.vsatp,
				( addr === "hF11".U ) -> m_csrFiles.mvendorid,
				( addr === "hF12".U ) -> m_csrFiles.marchid,
				( addr === "hF13".U ) -> m_csrFiles.mimpid,
				( addr === "hF14".U ) -> m_csrFiles.mhartid,
				( addr === "h300".U ) -> m_csrFiles.mstatus,
				( addr === "h301".U ) -> Cat("b10".U, 0.U(36.W), "b00000000000001000100000100".U(26.W)),//"b00000101000011000110101101".U(26.W)),//m_csrFiles.misa,
				( addr === "h302".U ) -> m_csrFiles.medeleg,
				( addr === "h303".U ) -> m_csrFiles.mideleg,
				( addr === "h304".U ) -> m_csrFiles.mie,
				( addr === "h305".U ) -> m_csrFiles.mtvec,
				( addr === "h306".U ) -> m_csrFiles.mcounteren,
				( addr === "h340".U ) -> m_csrFiles.mscratch,
				( addr === "h341".U ) -> m_csrFiles.mepc,
				( addr === "h342".U ) -> m_csrFiles.mcause,
				( addr === "h343".U ) -> m_csrFiles.mtval,
				( addr === "h344".U ) -> m_csrFiles.mip,
				( addr === "h34A".U ) -> m_csrFiles.mtinst,
				( addr === "h34B".U ) -> m_csrFiles.mtval2,
				( addr === "h3A0".U ) -> m_csrFiles.pmpcfg0,
				( addr === "h3A2".U ) -> m_csrFiles.pmpcfg2,
				( addr === "h3A4".U ) -> m_csrFiles.pmpcfg4,
				( addr === "h3A6".U ) -> m_csrFiles.pmpcfg6,
				( addr === "h3A8".U ) -> m_csrFiles.pmpcfg8,
				( addr === "h3AA".U ) -> m_csrFiles.pmpcfg10,
				( addr === "h3AC".U ) -> m_csrFiles.pmpcfg12,
				( addr === "h3AE".U ) -> m_csrFiles.pmpcfg14,
				( addr === "h3B0".U ) -> m_csrFiles.pmpaddr0,
				( addr === "h3B1".U ) -> m_csrFiles.pmpaddr1,
				( addr === "h3B2".U ) -> m_csrFiles.pmpaddr2,
				( addr === "h3B3".U ) -> m_csrFiles.pmpaddr3,
				( addr === "h3B4".U ) -> m_csrFiles.pmpaddr4,
				( addr === "h3B5".U ) -> m_csrFiles.pmpaddr5,
				( addr === "h3B6".U ) -> m_csrFiles.pmpaddr6,
				( addr === "h3B7".U ) -> m_csrFiles.pmpaddr7,
				( addr === "h3B8".U ) -> m_csrFiles.pmpaddr8,
				( addr === "h3B9".U ) -> m_csrFiles.pmpaddr9,
				( addr === "h3BA".U ) -> m_csrFiles.pmpaddr10,
				( addr === "h3BB".U ) -> m_csrFiles.pmpaddr11,
				( addr === "h3BC".U ) -> m_csrFiles.pmpaddr12,
				( addr === "h3BD".U ) -> m_csrFiles.pmpaddr13,
				( addr === "h3BE".U ) -> m_csrFiles.pmpaddr14,
				( addr === "h3BF".U ) -> m_csrFiles.pmpaddr15,
				( addr === "h3C0".U ) -> m_csrFiles.pmpaddr16,
				( addr === "h3C1".U ) -> m_csrFiles.pmpaddr17,
				( addr === "h3C2".U ) -> m_csrFiles.pmpaddr18,
				( addr === "h3C3".U ) -> m_csrFiles.pmpaddr19,
				( addr === "h3C4".U ) -> m_csrFiles.pmpaddr20,
				( addr === "hB00".U ) -> m_csrFiles.mcycle,
				( addr === "hB02".U ) -> m_csrFiles.minstret,
				( addr === "hB03".U ) -> m_csrFiles.mhpmcounter3,
				( addr === "h320".U ) -> m_csrFiles.mcountinhibit,
				( addr === "h323".U ) -> m_csrFiles.mhpmevent3,
				( addr === "h7A0".U ) -> d_csrFiles.tselect,
				( addr === "h7A1".U ) -> d_csrFiles.tdata1,
				( addr === "h7A2".U ) -> d_csrFiles.tdata2,
				( addr === "h7A3".U ) -> d_csrFiles.tdata3,
				( addr === "h7B0".U ) -> d_csrFiles.dcsr,
				( addr === "h7B1".U ) -> d_csrFiles.dpc,
				( addr === "h7B2".U ) -> d_csrFiles.dscratch0,
				( addr === "h7B3".U ) -> d_csrFiles.dscratch1
			))
} 
