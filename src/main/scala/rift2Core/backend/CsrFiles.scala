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
import rift2Core.basic._



class Port extends Bundle {
	val addr = UInt(12.W)
	val dat_i = UInt(64.W)
	val op_rw = Bool()
	val op_rs = Bool()
	val op_rc = Bool()
}



class CsrReg( addr: UInt, init: UInt, port: Port, ormask: UInt ) {
	val value = RegInit(init)

	when(port.addr === addr) {
		value := MuxCase(value, Array(
			port.op_rw -> (port.dat_i),
			port.op_rs -> (value | port.dat_i),
			port.op_rc -> (value & ~port.dat_i),
		)) | ormask 
	}
}




object U_CsrFiles {
	val port = Wire(new Port)

	//user trap setup
	val ustatus    = new CsrReg( "h000".U, 0.U, port, 0.U)
	val uie    = new CsrReg( "h004".U, 0.U, port, 0.U)
	val utvec    = new CsrReg( "h005".U, 0.U, port, 0.U)

	//user trap handling
	val uscratch    = new CsrReg( "h040".U, 0.U, port, 0.U)
	val uepc    = new CsrReg( "h041".U, 0.U, port, 0.U)
	val ucause    = new CsrReg( "h042".U, 0.U, port, 0.U)
	val utval    = new CsrReg( "h043".U, 0.U, port, 0.U)
	val uip    = new CsrReg( "h044".U, 0.U, port, 0.U)

	//user floating point csrs
	val fflags    = new CsrReg( "h001".U, 0.U, port, 0.U)
	val frm    = new CsrReg( "h002".U, 0.U, port, 0.U)
	val fcsr    = new CsrReg( "h003".U, 0.U, port, 0.U)

	//user conter timers
	val cycle    = new CsrReg( "hC00".U, 0.U, port, 0.U)
	val time    = new CsrReg( "hC01".U, 0.U, port, 0.U)
	val instret    = new CsrReg( "hC02".U, 0.U, port, 0.U)
	val hpmcounter3    = new CsrReg( "hC03".U, 0.U, port, 0.U)
	val hpmcounter4    = new CsrReg( "hC04".U, 0.U, port, 0.U)
	val hpmcounter5    = new CsrReg( "hC05".U, 0.U, port, 0.U)
	val hpmcounter6    = new CsrReg( "hC06".U, 0.U, port, 0.U)
	val hpmcounter7    = new CsrReg( "hC07".U, 0.U, port, 0.U)
	val hpmcounter8    = new CsrReg( "hC08".U, 0.U, port, 0.U)
	val hpmcounter9    = new CsrReg( "hC09".U, 0.U, port, 0.U)
	val hpmcounter10    = new CsrReg( "hC0A".U, 0.U, port, 0.U)
	val hpmcounter11    = new CsrReg( "hC0B".U, 0.U, port, 0.U)
	val hpmcounter12    = new CsrReg( "hC0C".U, 0.U, port, 0.U)
	val hpmcounter13    = new CsrReg( "hC0D".U, 0.U, port, 0.U)
	val hpmcounter14    = new CsrReg( "hC0E".U, 0.U, port, 0.U)
	val hpmcounter15    = new CsrReg( "hC0F".U, 0.U, port, 0.U)
	val hpmcounter16    = new CsrReg( "hC10".U, 0.U, port, 0.U)
	val hpmcounter17    = new CsrReg( "hC11".U, 0.U, port, 0.U)
	val hpmcounter18    = new CsrReg( "hC12".U, 0.U, port, 0.U)
	val hpmcounter19    = new CsrReg( "hC13".U, 0.U, port, 0.U)
	val hpmcounter20    = new CsrReg( "hC14".U, 0.U, port, 0.U)
	val hpmcounter21    = new CsrReg( "hC15".U, 0.U, port, 0.U)
	val hpmcounter22    = new CsrReg( "hC16".U, 0.U, port, 0.U)
	val hpmcounter23    = new CsrReg( "hC17".U, 0.U, port, 0.U)
	val hpmcounter24    = new CsrReg( "hC18".U, 0.U, port, 0.U)
	val hpmcounter25    = new CsrReg( "hC19".U, 0.U, port, 0.U)
	val hpmcounter26    = new CsrReg( "hC1A".U, 0.U, port, 0.U)
	val hpmcounter27    = new CsrReg( "hC1B".U, 0.U, port, 0.U)
	val hpmcounter28    = new CsrReg( "hC1C".U, 0.U, port, 0.U)
	val hpmcounter29    = new CsrReg( "hC1D".U, 0.U, port, 0.U)
	val hpmcounter30    = new CsrReg( "hC1E".U, 0.U, port, 0.U)
	val hpmcounter31    = new CsrReg( "hC1F".U, 0.U, port, 0.U)
}

object S_CsrFiles {
	val port = Wire(new Port)

	//supervisor trap setup
	val sstatus    = new CsrReg( "h100".U, 0.U, port, 0.U)
	val sedeleg    = new CsrReg( "h102".U, 0.U, port, 0.U)
	val sideleg    = new CsrReg( "h103".U, 0.U, port, 0.U)
	val sie    = new CsrReg( "h104".U, 0.U, port, 0.U)
	val stvec    = new CsrReg( "h105".U, 0.U, port, 0.U)
	val scounteren    = new CsrReg( "h106".U, 0.U, port, 0.U)

	//supervisor trap handling

	val sscratch    = new CsrReg( "h140".U, 0.U, port, 0.U)
	val sepc    = new CsrReg( "h141".U, 0.U, port, 0.U)
	val scause    = new CsrReg( "h142".U, 0.U, port, 0.U)
	val stval    = new CsrReg( "h143".U, 0.U, port, 0.U)
	val sip    = new CsrReg( "h144".U, 0.U, port, 0.U)

	//supervisor protection and translation
	val satp    = new CsrReg( "h180".U, 0.U, port, 0.U)
}

object H_CsrFiles {
	val port = Wire(new Port)

	//hypervisor trap setup
	val hstatus    = new CsrReg( "h600".U, 0.U, port, 0.U)
	val hedeleg    = new CsrReg( "h602".U, 0.U, port, 0.U)
	val hideleg    = new CsrReg( "h603".U, 0.U, port, 0.U)
	val hie    = new CsrReg( "h604".U, 0.U, port, 0.U)
	val hcounteren    = new CsrReg( "h606".U, 0.U, port, 0.U)
	val hgeie    = new CsrReg( "h607".U, 0.U, port, 0.U)

	//hypervisor trap handling
	val htval    = new CsrReg( "h643".U, 0.U, port, 0.U)
	val hip    = new CsrReg( "h644".U, 0.U, port, 0.U)
	val hvip    = new CsrReg( "h645".U, 0.U, port, 0.U)
	val htinst    = new CsrReg( "h64A".U, 0.U, port, 0.U)
	val hgeip    = new CsrReg( "hE12".U, 0.U, port, 0.U)

	//hypervisor protection and translation
	val hgatp    = new CsrReg( "h680".U, 0.U, port, 0.U)

	//hypervisor counter timer virtualization registers
	val htimedelta    = new CsrReg( "h605".U, 0.U, port, 0.U)

	//virtual supervisor registers
	val vsstatus    = new CsrReg( "h200".U, 0.U, port, 0.U)
	val vsie    = new CsrReg( "h204".U, 0.U, port, 0.U)
	val vstvec    = new CsrReg( "h205".U, 0.U, port, 0.U)
	val vsscratch    = new CsrReg( "h240".U, 0.U, port, 0.U)
	val vsepc    = new CsrReg( "h241".U, 0.U, port, 0.U)
	val vscause    = new CsrReg( "h242".U, 0.U, port, 0.U)
	val vstval    = new CsrReg( "h243".U, 0.U, port, 0.U)
	val vsip    = new CsrReg( "h244".U, 0.U, port, 0.U)
	val vsatp    = new CsrReg( "h280".U, 0.U, port, 0.U)
}

object M_CsrFiles {
	val port = Wire(new Port)
	val clint_csr_info = Wire( new Info_clint_csr )

	//machine information register
	val mvendorid    = new CsrReg( "hF11".U, 0.U, port, 0.U)
	val marchid      = new CsrReg( "hF12".U, 0.U, port, 0.U)
	val mimpid       = new CsrReg( "hF13".U, 0.U, port, 0.U)
	val mhartid      = new CsrReg( "hF14".U, 0.U, port, 0.U)


	//Machine Trap Setup
	val mstatus      = new CsrReg( "h300".U, 0.U, port, "h1800".U )
	val misa         = new CsrReg( "h301".U, Cat("b10".U, 0.U(36.W), "b00000101000011000110101101".U), port, 0.U)//ACDFHIMNSU
	val medeleg      = new CsrReg( "h302".U, 0.U, port, 0.U )
	val mideleg      = new CsrReg( "h303".U, 0.U, port, 0.U )
	val mie          = new CsrReg( "h304".U, 0.U, port, 0.U )
	val mtvec        = new CsrReg( "h305".U, 0.U, port, 0.U )
	val mcounteren   = new CsrReg( "h306".U, 0.U, port, 0.U )



	//Machine Trap Handling
	val mscratch     = new CsrReg( "h340".U, 0.U, port, 0.U )
	val mepc         = new CsrReg( "h341".U, 0.U, port, 0.U )
	val mcause       = new CsrReg( "h342".U, 0.U, port, 0.U )
	val mtval        = new CsrReg( "h343".U, 0.U, port, 0.U )
	val mip          = new CsrReg( "h344".U, (clint_csr_info.is_externInterrupt << 11 | clint_csr_info.is_rtimerInterrupt << 7 | clint_csr_info.is_softwvInterrupt << 3), 0.U.asTypeOf(port), 0.U )
	val mtinst    = new CsrReg( "h34A".U, 0.U, port, 0.U)
	val mtval2    = new CsrReg( "h34B".U, 0.U, port, 0.U)


	//Machine Memory Protection
	val pmpcfg0    = new CsrReg( "h3A0".U, 0.U, port, 0.U)
	// val pmpcfg1    = new CsrReg( "h3A1".U, 0.U, port, 0.U)
	val pmpcfg2    = new CsrReg( "h3A2".U, 0.U, port, 0.U)
	// val pmpcfg3    = new CsrReg( "h3A3".U, 0.U, port, 0.U)
	val pmpcfg4    = new CsrReg( "h3A4".U, 0.U, port, 0.U)
	// val pmpcfg5    = new CsrReg( "h3A5".U, 0.U, port, 0.U)
	val pmpcfg6    = new CsrReg( "h3A6".U, 0.U, port, 0.U)
	// val pmpcfg7    = new CsrReg( "h3A7".U, 0.U, port, 0.U)
	val pmpcfg8    = new CsrReg( "h3A8".U, 0.U, port, 0.U)
	// val pmpcfg9    = new CsrReg( "h3A9".U, 0.U, port, 0.U)
	val pmpcfg10    = new CsrReg( "h3AA".U, 0.U, port, 0.U)
	// val pmpcfg11    = new CsrReg( "h3AB".U, 0.U, port, 0.U)
	val pmpcfg12    = new CsrReg( "h3AC".U, 0.U, port, 0.U)
	// val pmpcfg13    = new CsrReg( "h3AD".U, 0.U, port, 0.U)
	val pmpcfg14    = new CsrReg( "h3AE".U, 0.U, port, 0.U)
	// val pmpcfg15    = new CsrReg( "h3AF".U, 0.U, port, 0.U)


	val pmpaddr0    = new CsrReg( "h3B0".U, 0.U, port, 0.U)
	val pmpaddr1    = new CsrReg( "h3B1".U, 0.U, port, 0.U)
	val pmpaddr2    = new CsrReg( "h3B2".U, 0.U, port, 0.U)
	val pmpaddr3    = new CsrReg( "h3B3".U, 0.U, port, 0.U)
	val pmpaddr4    = new CsrReg( "h3B4".U, 0.U, port, 0.U)
	val pmpaddr5    = new CsrReg( "h3B5".U, 0.U, port, 0.U)
	val pmpaddr6    = new CsrReg( "h3B6".U, 0.U, port, 0.U)
	val pmpaddr7    = new CsrReg( "h3B7".U, 0.U, port, 0.U)
	val pmpaddr8    = new CsrReg( "h3B8".U, 0.U, port, 0.U)
	val pmpaddr9    = new CsrReg( "h3B9".U, 0.U, port, 0.U)
	val pmpaddr10    = new CsrReg( "h3BA".U, 0.U, port, 0.U)
	val pmpaddr11    = new CsrReg( "h3BB".U, 0.U, port, 0.U)
	val pmpaddr12    = new CsrReg( "h3BC".U, 0.U, port, 0.U)
	val pmpaddr13    = new CsrReg( "h3BD".U, 0.U, port, 0.U)
	val pmpaddr14    = new CsrReg( "h3BE".U, 0.U, port, 0.U)
	val pmpaddr15    = new CsrReg( "h3BF".U, 0.U, port, 0.U)
	val pmpaddr16    = new CsrReg( "h3C0".U, 0.U, port, 0.U)
	val pmpaddr17    = new CsrReg( "h3C1".U, 0.U, port, 0.U)
	val pmpaddr18    = new CsrReg( "h3C2".U, 0.U, port, 0.U)
	val pmpaddr19    = new CsrReg( "h3C3".U, 0.U, port, 0.U)
	val pmpaddr20    = new CsrReg( "h3C4".U, 0.U, port, 0.U)



	//Machine Counter/Timer

	//0xb00
	val mcycle       = new CsrReg( "hB00".U, 0.U, port, 0.U )
	val minstret     = new CsrReg( "hB02".U, 0.U, port, 0.U )
	val mhpmcounter3 = new CsrReg( "hB03".U, 0.U, port, 0.U )



	//Machine Counter Setup
	val mcountinhibit = new CsrReg( "h320".U, 0.U, port, 0.U )
	val mhpmevent3 = new CsrReg( "h323".U, 0.U, port, 0.U )
}

object D_CsrFiles {
	val port = Wire(new Port)

	//Debug/Trace Register
	val tselect = new CsrReg( "h7A0".U, 0.U, port, 0.U )
	val tdata1 = new CsrReg( "h7A1".U, 0.U, port, 0.U )
	val tdata2 = new CsrReg( "h7A2".U, 0.U, port, 0.U )
	val tdata3 = new CsrReg( "h7A3".U, 0.U, port, 0.U )

	//Debug Mode Register
	val dcsr = new CsrReg( "h7B0".U, 0.U, port, 0.U )
	val dpc = new CsrReg( "h7B1".U, 0.U, port, 0.U )
	val dscratch0 = new CsrReg( "h7B2".U, 0.U, port, 0.U )
	val dscratch1 = new CsrReg( "h7B3".U, 0.U, port, 0.U )
}

object csrFiles {
	def read( addr: UInt ) = MuxCase(0.U, Array(
								( addr === "h000".U ) -> U_CsrFiles.ustatus.value,
								( addr === "h004".U ) -> U_CsrFiles.uie.value,
								( addr === "h005".U ) -> U_CsrFiles.utvec.value,
								( addr === "h040".U ) -> U_CsrFiles.uscratch.value,
								( addr === "h041".U ) -> U_CsrFiles.uepc.value,
								( addr === "h042".U ) -> U_CsrFiles.ucause.value,
								( addr === "h043".U ) -> U_CsrFiles.utval.value,
								( addr === "h044".U ) -> U_CsrFiles.uip.value,
								( addr === "h001".U ) -> U_CsrFiles.fflags.value,
								( addr === "h002".U ) -> U_CsrFiles.frm.value,
								( addr === "h003".U ) -> U_CsrFiles.fcsr.value,
								( addr === "hC00".U ) -> U_CsrFiles.cycle.value,
								( addr === "hC01".U ) -> U_CsrFiles.time.value,
								( addr === "hC02".U ) -> U_CsrFiles.instret.value,
								( addr === "hC03".U ) -> U_CsrFiles.hpmcounter3.value,
								( addr === "hC04".U ) -> U_CsrFiles.hpmcounter4.value,
								( addr === "hC05".U ) -> U_CsrFiles.hpmcounter5.value,
								( addr === "hC06".U ) -> U_CsrFiles.hpmcounter6.value,
								( addr === "hC07".U ) -> U_CsrFiles.hpmcounter7.value,
								( addr === "hC08".U ) -> U_CsrFiles.hpmcounter8.value,
								( addr === "hC09".U ) -> U_CsrFiles.hpmcounter9.value,
								( addr === "hC0A".U ) -> U_CsrFiles.hpmcounter10.value,
								( addr === "hC0B".U ) -> U_CsrFiles.hpmcounter11.value,
								( addr === "hC0C".U ) -> U_CsrFiles.hpmcounter12.value,
								( addr === "hC0D".U ) -> U_CsrFiles.hpmcounter13.value,
								( addr === "hC0E".U ) -> U_CsrFiles.hpmcounter14.value,
								( addr === "hC0F".U ) -> U_CsrFiles.hpmcounter15.value,
								( addr === "hC10".U ) -> U_CsrFiles.hpmcounter16.value,
								( addr === "hC11".U ) -> U_CsrFiles.hpmcounter17.value,
								( addr === "hC12".U ) -> U_CsrFiles.hpmcounter18.value,
								( addr === "hC13".U ) -> U_CsrFiles.hpmcounter19.value,
								( addr === "hC14".U ) -> U_CsrFiles.hpmcounter20.value,
								( addr === "hC15".U ) -> U_CsrFiles.hpmcounter21.value,
								( addr === "hC16".U ) -> U_CsrFiles.hpmcounter22.value,
								( addr === "hC17".U ) -> U_CsrFiles.hpmcounter23.value,
								( addr === "hC18".U ) -> U_CsrFiles.hpmcounter24.value,
								( addr === "hC19".U ) -> U_CsrFiles.hpmcounter25.value,
								( addr === "hC1A".U ) -> U_CsrFiles.hpmcounter26.value,
								( addr === "hC1B".U ) -> U_CsrFiles.hpmcounter27.value,
								( addr === "hC1C".U ) -> U_CsrFiles.hpmcounter28.value,
								( addr === "hC1D".U ) -> U_CsrFiles.hpmcounter29.value,
								( addr === "hC1E".U ) -> U_CsrFiles.hpmcounter30.value,
								( addr === "hC1F".U ) -> U_CsrFiles.hpmcounter31.value,
								( addr === "h100".U ) -> S_CsrFiles.sstatus.value,
								( addr === "h102".U ) -> S_CsrFiles.sedeleg.value,
								( addr === "h103".U ) -> S_CsrFiles.sideleg.value,
								( addr === "h104".U ) -> S_CsrFiles.sie.value,
								( addr === "h105".U ) -> S_CsrFiles.stvec.value,
								( addr === "h106".U ) -> S_CsrFiles.scounteren.value,
								( addr === "h140".U ) -> S_CsrFiles.sscratch.value,
								( addr === "h141".U ) -> S_CsrFiles.sepc.value,
								( addr === "h142".U ) -> S_CsrFiles.scause.value,
								( addr === "h143".U ) -> S_CsrFiles.stval.value,
								( addr === "h144".U ) -> S_CsrFiles.sip.value,
								( addr === "h180".U ) -> S_CsrFiles.satp.value,
								( addr === "h600".U ) -> H_CsrFiles.hstatus.value,
								( addr === "h602".U ) -> H_CsrFiles.hedeleg.value,
								( addr === "h603".U ) -> H_CsrFiles.hideleg.value,
								( addr === "h604".U ) -> H_CsrFiles.hie.value,
								( addr === "h606".U ) -> H_CsrFiles.hcounteren.value,
								( addr === "h607".U ) -> H_CsrFiles.hgeie.value,
								( addr === "h643".U ) -> H_CsrFiles.htval.value,
								( addr === "h644".U ) -> H_CsrFiles.hip.value,
								( addr === "h645".U ) -> H_CsrFiles.hvip.value,
								( addr === "h64A".U ) -> H_CsrFiles.htinst.value,
								( addr === "hE12".U ) -> H_CsrFiles.hgeip.value,
								( addr === "h680".U ) -> H_CsrFiles.hgatp.value,
								( addr === "h605".U ) -> H_CsrFiles.htimedelta.value,
								( addr === "h200".U ) -> H_CsrFiles.vsstatus.value,
								( addr === "h204".U ) -> H_CsrFiles.vsie.value,
								( addr === "h205".U ) -> H_CsrFiles.vstvec.value,
								( addr === "h240".U ) -> H_CsrFiles.vsscratch.value,
								( addr === "h241".U ) -> H_CsrFiles.vsepc.value,
								( addr === "h242".U ) -> H_CsrFiles.vscause.value,
								( addr === "h243".U ) -> H_CsrFiles.vstval.value,
								( addr === "h244".U ) -> H_CsrFiles.vsip.value,
								( addr === "h280".U ) -> H_CsrFiles.vsatp.value,
								( addr === "hF11".U ) -> M_CsrFiles.mvendorid.value,
								( addr === "hF12".U ) -> M_CsrFiles.marchid.value,
								( addr === "hF13".U ) -> M_CsrFiles.mimpid.value,
								( addr === "hF14".U ) -> M_CsrFiles.mhartid.value,
								( addr === "h300".U ) -> M_CsrFiles.mstatus.value,
								( addr === "h301".U ) -> M_CsrFiles.misa.value,
								( addr === "h302".U ) -> M_CsrFiles.medeleg.value,
								( addr === "h303".U ) -> M_CsrFiles.mideleg.value,
								( addr === "h304".U ) -> M_CsrFiles.mie.value,
								( addr === "h305".U ) -> M_CsrFiles.mtvec.value,
								( addr === "h306".U ) -> M_CsrFiles.mcounteren.value,
								( addr === "h340".U ) -> M_CsrFiles.mscratch.value,
								( addr === "h341".U ) -> M_CsrFiles.mepc.value,
								( addr === "h342".U ) -> M_CsrFiles.mcause.value,
								( addr === "h343".U ) -> M_CsrFiles.mtval.value,
								( addr === "h344".U ) -> M_CsrFiles.mip.value,
								( addr === "h34A".U ) -> M_CsrFiles.mtinst.value,
								( addr === "h34B".U ) -> M_CsrFiles.mtval2.value,
								( addr === "h3A0".U ) -> M_CsrFiles.pmpcfg0.value,
								( addr === "h3A2".U ) -> M_CsrFiles.pmpcfg2.value,
								( addr === "h3A4".U ) -> M_CsrFiles.pmpcfg4.value,
								( addr === "h3A6".U ) -> M_CsrFiles.pmpcfg6.value,
								( addr === "h3A8".U ) -> M_CsrFiles.pmpcfg8.value,
								( addr === "h3AA".U ) -> M_CsrFiles.pmpcfg10.value,
								( addr === "h3AC".U ) -> M_CsrFiles.pmpcfg12.value,
								( addr === "h3AE".U ) -> M_CsrFiles.pmpcfg14.value,
								( addr === "h3B0".U ) -> M_CsrFiles.pmpaddr0.value,
								( addr === "h3B1".U ) -> M_CsrFiles.pmpaddr1.value,
								( addr === "h3B2".U ) -> M_CsrFiles.pmpaddr2.value,
								( addr === "h3B3".U ) -> M_CsrFiles.pmpaddr3.value,
								( addr === "h3B4".U ) -> M_CsrFiles.pmpaddr4.value,
								( addr === "h3B5".U ) -> M_CsrFiles.pmpaddr5.value,
								( addr === "h3B6".U ) -> M_CsrFiles.pmpaddr6.value,
								( addr === "h3B7".U ) -> M_CsrFiles.pmpaddr7.value,
								( addr === "h3B8".U ) -> M_CsrFiles.pmpaddr8.value,
								( addr === "h3B9".U ) -> M_CsrFiles.pmpaddr9.value,
								( addr === "h3BA".U ) -> M_CsrFiles.pmpaddr10.value,
								( addr === "h3BB".U ) -> M_CsrFiles.pmpaddr11.value,
								( addr === "h3BC".U ) -> M_CsrFiles.pmpaddr12.value,
								( addr === "h3BD".U ) -> M_CsrFiles.pmpaddr13.value,
								( addr === "h3BE".U ) -> M_CsrFiles.pmpaddr14.value,
								( addr === "h3BF".U ) -> M_CsrFiles.pmpaddr15.value,
								( addr === "h3C0".U ) -> M_CsrFiles.pmpaddr16.value,
								( addr === "h3C1".U ) -> M_CsrFiles.pmpaddr17.value,
								( addr === "h3C2".U ) -> M_CsrFiles.pmpaddr18.value,
								( addr === "h3C3".U ) -> M_CsrFiles.pmpaddr19.value,
								( addr === "h3C4".U ) -> M_CsrFiles.pmpaddr20.value,
								( addr === "hB00".U ) -> M_CsrFiles.mcycle.value,
								( addr === "hB02".U ) -> M_CsrFiles.minstret.value,
								( addr === "hB03".U ) -> M_CsrFiles.mhpmcounter3.value,
								( addr === "h320".U ) -> M_CsrFiles.mcountinhibit.value,
								( addr === "h323".U ) -> M_CsrFiles.mhpmevent3.value,
								( addr === "h7A0".U ) -> D_CsrFiles.tselect.value,
								( addr === "h7A1".U ) -> D_CsrFiles.tdata1.value,
								( addr === "h7A2".U ) -> D_CsrFiles.tdata2.value,
								( addr === "h7A3".U ) -> D_CsrFiles.tdata3.value,
								( addr === "h7B0".U ) -> D_CsrFiles.dcsr.value,
								( addr === "h7B1".U ) -> D_CsrFiles.dpc.value,
								( addr === "h7B2".U ) -> D_CsrFiles.dscratch0.value,
								( addr === "h7B3".U ) -> D_CsrFiles.dscratch1.value
							))
} 
