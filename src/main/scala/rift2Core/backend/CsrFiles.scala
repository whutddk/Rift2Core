/*
* @Author: Ruige Lee
* @Date:   2021-03-30 11:06:02
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-30 16:11:49
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
import rift2Core.basicElement._

class Port_a extends Bundle {
	val en = Bool()
	val dnxt = UInt(64.W)
}

class Port_b extends Bundle {
	val addr = UInt(12.W)
	val op = UInt(64.W)
	val rw = Bool()
	val rs = Bool()
	val rc = Bool()
}



class CsrReg( addr: UInt, init: UInt, port_a: Port_a, port_b: Port_b, ormask: UInt ) {
	
	val value = RegInit(init)
	when(port_a.en) {
		value := port_a.dnxt | ormask
	}
	.elsewhen(port_b.addr === addr) {
		value := MuxCase(value, Array(
			port_b.rw -> (port_b.op),
			port_b.rs -> (value | port_b.op),
			port_b.rc -> (value & ~port_b.op),
		)) | ormask 
	}
}




class CsrFiles extends Module {
	val io = IO( new Bundle{
		val csr_files_info = Flipped(new Info_csr_files)

		val cmm_csr_info = Input(new Info_cmm_csr)
		val clint_csr_info = Input( new Info_clint_csr )


	})

	val port_a_nc = Wire(new Port_a); { port_a_nc.en := false.B; port_a_nc.dnxt := DontCare }
	val port_b_nc = Wire(new Port_b); { port_b_nc.addr := DontCare; port_b_nc.op := DontCare; port_b_nc.rw := false.B; port_b_nc.rs := false.B; port_b_nc.rc := false.B}

	val port_b_wire = Wire(new Port_b)

	{
		port_b_wire.addr := io.csr_files_info.addr
		port_b_wire.op   := io.csr_files_info.op
		port_b_wire.rw   := io.csr_files_info.rw
		port_b_wire.rs   := io.csr_files_info.rs
		port_b_wire.rc   := io.csr_files_info.rc
	}

val mstatus_port_a = Wire(new Port_a); { mstatus_port_a.dnxt := io.cmm_csr_info.privil_mstatus; mstatus_port_a.en := io.cmm_csr_info.is_trap | io.cmm_csr_info.is_xRet}
val mepc_port_a = Wire(new Port_a); { mepc_port_a.dnxt := io.cmm_csr_info.privil_mepc; mepc_port_a.en := io.cmm_csr_info.is_trap}
val mcause_port_a = Wire(new Port_a); { mcause_port_a.dnxt := io.cmm_csr_info.privil_mcause; mcause_port_a.en := io.cmm_csr_info.is_trap}
val mtval_port_a = Wire(new Port_a); { mtval_port_a.dnxt := io.cmm_csr_info.privil_mtval; mtval_port_a.en := io.cmm_csr_info.is_trap}



//user trap setup
val ustatus    = new CsrReg( "h000".U, 0.U, port_a_nc, port_b_nc, 0.U)
val uie    = new CsrReg( "h004".U, 0.U, port_a_nc, port_b_nc, 0.U)
val utvec    = new CsrReg( "h005".U, 0.U, port_a_nc, port_b_nc, 0.U)

//user trap handling
val uscratch    = new CsrReg( "h040".U, 0.U, port_a_nc, port_b_nc, 0.U)
val uepc    = new CsrReg( "h041".U, 0.U, port_a_nc, port_b_nc, 0.U)
val ucause    = new CsrReg( "h042".U, 0.U, port_a_nc, port_b_nc, 0.U)
val utval    = new CsrReg( "h043".U, 0.U, port_a_nc, port_b_nc, 0.U)
val uip    = new CsrReg( "h044".U, 0.U, port_a_nc, port_b_nc, 0.U)

//user floating point csrs
val fflags    = new CsrReg( "h001".U, 0.U, port_a_nc, port_b_nc, 0.U)
val frm    = new CsrReg( "h002".U, 0.U, port_a_nc, port_b_nc, 0.U)
val fcsr    = new CsrReg( "h003".U, 0.U, port_a_nc, port_b_nc, 0.U)

//user conter timers
val cycle    = new CsrReg( "hC00".U, 0.U, port_a_nc, port_b_nc, 0.U)
val time    = new CsrReg( "hC01".U, 0.U, port_a_nc, port_b_nc, 0.U)
val instret    = new CsrReg( "hC02".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter3    = new CsrReg( "hC03".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter4    = new CsrReg( "hC04".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter5    = new CsrReg( "hC05".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter6    = new CsrReg( "hC06".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter7    = new CsrReg( "hC07".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter8    = new CsrReg( "hC08".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter9    = new CsrReg( "hC09".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter10    = new CsrReg( "hC0A".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter11    = new CsrReg( "hC0B".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter12    = new CsrReg( "hC0C".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter13    = new CsrReg( "hC0D".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter14    = new CsrReg( "hC0E".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter15    = new CsrReg( "hC0F".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter16    = new CsrReg( "hC10".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter17    = new CsrReg( "hC11".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter18    = new CsrReg( "hC12".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter19    = new CsrReg( "hC13".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter20    = new CsrReg( "hC14".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter21    = new CsrReg( "hC15".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter22    = new CsrReg( "hC16".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter23    = new CsrReg( "hC17".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter24    = new CsrReg( "hC18".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter25    = new CsrReg( "hC19".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter26    = new CsrReg( "hC1A".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter27    = new CsrReg( "hC1B".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter28    = new CsrReg( "hC1C".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter29    = new CsrReg( "hC1D".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter30    = new CsrReg( "hC1E".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hpmcounter31    = new CsrReg( "hC1F".U, 0.U, port_a_nc, port_b_nc, 0.U)

//supervisor trap setup
val sstatus    = new CsrReg( "h100".U, 0.U, port_a_nc, port_b_nc, 0.U)
val sedeleg    = new CsrReg( "h102".U, 0.U, port_a_nc, port_b_nc, 0.U)
val sideleg    = new CsrReg( "h103".U, 0.U, port_a_nc, port_b_nc, 0.U)
val sie    = new CsrReg( "h104".U, 0.U, port_a_nc, port_b_nc, 0.U)
val stvec    = new CsrReg( "h105".U, 0.U, port_a_nc, port_b_nc, 0.U)
val scounteren    = new CsrReg( "h106".U, 0.U, port_a_nc, port_b_nc, 0.U)

//supervisor trap handling

val sscratch    = new CsrReg( "h140".U, 0.U, port_a_nc, port_b_nc, 0.U)
val sepc    = new CsrReg( "h141".U, 0.U, port_a_nc, port_b_nc, 0.U)
val scause    = new CsrReg( "h142".U, 0.U, port_a_nc, port_b_nc, 0.U)
val stval    = new CsrReg( "h143".U, 0.U, port_a_nc, port_b_nc, 0.U)
val sip    = new CsrReg( "h144".U, 0.U, port_a_nc, port_b_nc, 0.U)

//supervisor protection and translation
val satp    = new CsrReg( "h180".U, 0.U, port_a_nc, port_b_nc, 0.U)



//hypervisor trap setup
val hstatus    = new CsrReg( "h600".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hedeleg    = new CsrReg( "h602".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hideleg    = new CsrReg( "h603".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hie    = new CsrReg( "h604".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hcounteren    = new CsrReg( "h606".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hgeie    = new CsrReg( "h607".U, 0.U, port_a_nc, port_b_nc, 0.U)

//hypervisor trap handling
val htval    = new CsrReg( "h643".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hip    = new CsrReg( "h644".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hvip    = new CsrReg( "h645".U, 0.U, port_a_nc, port_b_nc, 0.U)
val htinst    = new CsrReg( "h64A".U, 0.U, port_a_nc, port_b_nc, 0.U)
val hgeip    = new CsrReg( "hE12".U, 0.U, port_a_nc, port_b_nc, 0.U)

//hypervisor protection and translation
val hgatp    = new CsrReg( "h680".U, 0.U, port_a_nc, port_b_nc, 0.U)

//hypervisor counter timer virtualization registers
val htimedelta    = new CsrReg( "h605".U, 0.U, port_a_nc, port_b_nc, 0.U)

//virtual supervisor registers
val vsstatus    = new CsrReg( "h200".U, 0.U, port_a_nc, port_b_nc, 0.U)
val vsie    = new CsrReg( "h204".U, 0.U, port_a_nc, port_b_nc, 0.U)
val vstvec    = new CsrReg( "h205".U, 0.U, port_a_nc, port_b_nc, 0.U)
val vsscratch    = new CsrReg( "h240".U, 0.U, port_a_nc, port_b_nc, 0.U)
val vsepc    = new CsrReg( "h241".U, 0.U, port_a_nc, port_b_nc, 0.U)
val vscause    = new CsrReg( "h242".U, 0.U, port_a_nc, port_b_nc, 0.U)
val vstval    = new CsrReg( "h243".U, 0.U, port_a_nc, port_b_nc, 0.U)
val vsip    = new CsrReg( "h244".U, 0.U, port_a_nc, port_b_nc, 0.U)
val vsatp    = new CsrReg( "h280".U, 0.U, port_a_nc, port_b_nc, 0.U)

//machine information register
val mvendorid    = new CsrReg( "hF11".U, 0.U, port_a_nc, port_b_nc, 0.U)
val marchid      = new CsrReg( "hF12".U, 0.U, port_a_nc, port_b_nc, 0.U)
val mimpid       = new CsrReg( "hF13".U, 0.U, port_a_nc, port_b_nc, 0.U)
val mhartid      = new CsrReg( "hF14".U, 0.U, port_a_nc, port_b_nc, 0.U)


//Machine Trap Setup
val mstatus      = new CsrReg( "h300".U, 0.U, mstatus_port_a, port_b_wire, "h1800".U )
val misa         = new CsrReg( "h301".U, Cat("b10".U, 0.U(36.W), "b00000101000011000110101101".U), port_a_nc, port_b_nc, 0.U)//ACDFHIMNSU
val medeleg      = new CsrReg( "h302".U, 0.U, port_a_nc, port_b_wire, 0.U )
val mideleg      = new CsrReg( "h303".U, 0.U, port_a_nc, port_b_wire, 0.U )
val mie          = new CsrReg( "h304".U, 0.U, port_a_nc, port_b_wire, 0.U )
val mtvec        = new CsrReg( "h305".U, 0.U, port_a_nc, port_b_wire, 0.U )
val mcounteren   = new CsrReg( "h306".U, 0.U, port_a_nc, port_b_wire, 0.U )



//Machine Trap Handling
val mscratch     = new CsrReg( "h340".U, 0.U, port_a_nc, port_b_wire, 0.U )
val mepc         = new CsrReg( "h341".U, 0.U, mepc_port_a, port_b_wire, 0.U )
val mcause       = new CsrReg( "h342".U, 0.U, mcause_port_a, port_b_wire, 0.U )
val mtval        = new CsrReg( "h343".U, 0.U, mtval_port_a, port_b_wire, 0.U )
val mip          = new CsrReg( "h344".U, (io.clint_csr_info.is_externInterrupt << 11 | io.clint_csr_info.is_rtimerInterrupt << 7 | io.clint_csr_info.is_softwvInterrupt << 3), port_a_nc, port_b_nc, 0.U )
val mtinst    = new CsrReg( "h34A".U, 0.U, port_a_nc, port_b_nc, 0.U)
val mtval2    = new CsrReg( "h34B".U, 0.U, port_a_nc, port_b_nc, 0.U)


//Machine Memory Protection
val pmpcfg0    = new CsrReg( "h3A0".U, 0.U, port_a_nc, port_b_nc, 0.U)
// val pmpcfg1    = new CsrReg( "h3A1".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpcfg2    = new CsrReg( "h3A2".U, 0.U, port_a_nc, port_b_nc, 0.U)
// val pmpcfg3    = new CsrReg( "h3A3".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpcfg4    = new CsrReg( "h3A4".U, 0.U, port_a_nc, port_b_nc, 0.U)
// val pmpcfg5    = new CsrReg( "h3A5".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpcfg6    = new CsrReg( "h3A6".U, 0.U, port_a_nc, port_b_nc, 0.U)
// val pmpcfg7    = new CsrReg( "h3A7".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpcfg8    = new CsrReg( "h3A8".U, 0.U, port_a_nc, port_b_nc, 0.U)
// val pmpcfg9    = new CsrReg( "h3A9".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpcfg10    = new CsrReg( "h3AA".U, 0.U, port_a_nc, port_b_nc, 0.U)
// val pmpcfg11    = new CsrReg( "h3AB".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpcfg12    = new CsrReg( "h3AC".U, 0.U, port_a_nc, port_b_nc, 0.U)
// val pmpcfg13    = new CsrReg( "h3AD".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpcfg14    = new CsrReg( "h3AE".U, 0.U, port_a_nc, port_b_nc, 0.U)
// val pmpcfg15    = new CsrReg( "h3AF".U, 0.U, port_a_nc, port_b_nc, 0.U)


val pmpaddr0    = new CsrReg( "h3B0".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr1    = new CsrReg( "h3B1".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr2    = new CsrReg( "h3B2".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr3    = new CsrReg( "h3B3".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr4    = new CsrReg( "h3B4".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr5    = new CsrReg( "h3B5".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr6    = new CsrReg( "h3B6".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr7    = new CsrReg( "h3B7".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr8    = new CsrReg( "h3B8".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr9    = new CsrReg( "h3B9".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr10    = new CsrReg( "h3BA".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr11    = new CsrReg( "h3BB".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr12    = new CsrReg( "h3BC".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr13    = new CsrReg( "h3BD".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr14    = new CsrReg( "h3BE".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr15    = new CsrReg( "h3BF".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr16    = new CsrReg( "h3C0".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr17    = new CsrReg( "h3C1".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr18    = new CsrReg( "h3C2".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr19    = new CsrReg( "h3C3".U, 0.U, port_a_nc, port_b_nc, 0.U)
val pmpaddr20    = new CsrReg( "h3C4".U, 0.U, port_a_nc, port_b_nc, 0.U)





//Machine Counter/Timer

//0xb00
val mcycle       = new CsrReg( "hB00".U, 0.U, port_a_nc, port_b_wire, 0.U )
val minstret     = new CsrReg( "hB02".U, 0.U, port_a_nc, port_b_wire, 0.U )
val mhpmcounter3 = new CsrReg( "hB03".U, 0.U, port_a_nc, port_b_wire, 0.U )



//Machine Counter Setup
val mcountinhibit = new CsrReg( "h320".U, 0.U, port_a_nc, port_b_nc, 0.U )
val mhpmevent3 = new CsrReg( "h323".U, 0.U, port_a_nc, port_b_nc, 0.U )


//Debug/Trace Register
val tselect = new CsrReg( "h7A0".U, 0.U, port_a_nc, port_b_nc, 0.U )
val tdata1 = new CsrReg( "h7A1".U, 0.U, port_a_nc, port_b_nc, 0.U )
val tdata2 = new CsrReg( "h7A2".U, 0.U, port_a_nc, port_b_nc, 0.U )
val tdata3 = new CsrReg( "h7A3".U, 0.U, port_a_nc, port_b_nc, 0.U )



//Debug Mode Register
val dcsr = new CsrReg( "h7B0".U, 0.U, port_a_nc, port_b_wire, 0.U )
val dpc = new CsrReg( "h7B1".U, 0.U, port_a_nc, port_b_wire, 0.U )
val dscratch0 = new CsrReg( "h7B2".U, 0.U, port_a_nc, port_b_wire, 0.U )
val dscratch1 = new CsrReg( "h7B3".U, 0.U, port_a_nc, port_b_wire, 0.U )


io.csr_files_info.res := MuxCase(0.U, Array(
	( io.csr_files_info.addr === "h000".U) -> ustatus.value,
	( io.csr_files_info.addr === "h004".U) -> uie.value,
	( io.csr_files_info.addr === "h005".U) -> utvec.value,
	( io.csr_files_info.addr === "h040".U) -> uscratch.value,
	( io.csr_files_info.addr === "h041".U) -> uepc.value,
	( io.csr_files_info.addr === "h042".U) -> ucause.value,
	( io.csr_files_info.addr === "h043".U) -> utval.value,
	( io.csr_files_info.addr === "h044".U) -> uip.value,
	( io.csr_files_info.addr === "h001".U) -> fflags.value,
	( io.csr_files_info.addr === "h002".U) -> frm.value,
	( io.csr_files_info.addr === "h003".U) -> fcsr.value,
	( io.csr_files_info.addr === "hC00".U) -> cycle.value,
	( io.csr_files_info.addr === "hC01".U) -> time.value,
	( io.csr_files_info.addr === "hC02".U) -> instret.value,
	( io.csr_files_info.addr === "hC03".U) -> hpmcounter3.value,
	( io.csr_files_info.addr === "hC04".U) -> hpmcounter4.value,
	( io.csr_files_info.addr === "hC05".U) -> hpmcounter5.value,
	( io.csr_files_info.addr === "hC06".U) -> hpmcounter6.value,
	( io.csr_files_info.addr === "hC07".U) -> hpmcounter7.value,
	( io.csr_files_info.addr === "hC08".U) -> hpmcounter8.value,
	( io.csr_files_info.addr === "hC09".U) -> hpmcounter9.value,
	( io.csr_files_info.addr === "hC0A".U) -> hpmcounter10.value,
	( io.csr_files_info.addr === "hC0B".U) -> hpmcounter11.value,
	( io.csr_files_info.addr === "hC0C".U) -> hpmcounter12.value,
	( io.csr_files_info.addr === "hC0D".U) -> hpmcounter13.value,
	( io.csr_files_info.addr === "hC0E".U) -> hpmcounter14.value,
	( io.csr_files_info.addr === "hC0F".U) -> hpmcounter15.value,
	( io.csr_files_info.addr === "hC10".U) -> hpmcounter16.value,
	( io.csr_files_info.addr === "hC11".U) -> hpmcounter17.value,
	( io.csr_files_info.addr === "hC12".U) -> hpmcounter18.value,
	( io.csr_files_info.addr === "hC13".U) -> hpmcounter19.value,
	( io.csr_files_info.addr === "hC14".U) -> hpmcounter20.value,
	( io.csr_files_info.addr === "hC15".U) -> hpmcounter21.value,
	( io.csr_files_info.addr === "hC16".U) -> hpmcounter22.value,
	( io.csr_files_info.addr === "hC17".U) -> hpmcounter23.value,
	( io.csr_files_info.addr === "hC18".U) -> hpmcounter24.value,
	( io.csr_files_info.addr === "hC19".U) -> hpmcounter25.value,
	( io.csr_files_info.addr === "hC1A".U) -> hpmcounter26.value,
	( io.csr_files_info.addr === "hC1B".U) -> hpmcounter27.value,
	( io.csr_files_info.addr === "hC1C".U) -> hpmcounter28.value,
	( io.csr_files_info.addr === "hC1D".U) -> hpmcounter29.value,
	( io.csr_files_info.addr === "hC1E".U) -> hpmcounter30.value,
	( io.csr_files_info.addr === "hC1F".U) -> hpmcounter31.value,
	( io.csr_files_info.addr === "h100".U) -> sstatus.value,
	( io.csr_files_info.addr === "h102".U) -> sedeleg.value,
	( io.csr_files_info.addr === "h103".U) -> sideleg.value,
	( io.csr_files_info.addr === "h104".U) -> sie.value,
	( io.csr_files_info.addr === "h105".U) -> stvec.value,
	( io.csr_files_info.addr === "h106".U) -> scounteren.value,
    ( io.csr_files_info.addr === "h140".U) -> sscratch.value,
	( io.csr_files_info.addr === "h141".U) -> sepc.value,
	( io.csr_files_info.addr === "h142".U) -> scause.value,
	( io.csr_files_info.addr === "h143".U) -> stval.value,
	( io.csr_files_info.addr === "h144".U) -> sip.value,
	( io.csr_files_info.addr === "h180".U) -> satp.value,
	( io.csr_files_info.addr === "h600".U) -> hstatus.value,
	( io.csr_files_info.addr === "h602".U) -> hedeleg.value,
	( io.csr_files_info.addr === "h603".U) -> hideleg.value,
	( io.csr_files_info.addr === "h604".U) -> hie.value,
	( io.csr_files_info.addr === "h606".U) -> hcounteren.value,
	( io.csr_files_info.addr === "h607".U) -> hgeie.value,
	( io.csr_files_info.addr === "h643".U) -> htval.value,
	( io.csr_files_info.addr === "h644".U) -> hip.value,
	( io.csr_files_info.addr === "h645".U) -> hvip.value,
	( io.csr_files_info.addr === "h64A".U) -> htinst.value,
	( io.csr_files_info.addr === "hE12".U) -> hgeip.value,
	( io.csr_files_info.addr === "h680".U) -> hgatp.value,
	( io.csr_files_info.addr === "h605".U) -> htimedelta.value,
	( io.csr_files_info.addr === "h200".U) -> vsstatus.value,
	( io.csr_files_info.addr === "h204".U) -> vsie.value,
	( io.csr_files_info.addr === "h205".U) -> vstvec.value,
	( io.csr_files_info.addr === "h240".U) -> vsscratch.value,
	( io.csr_files_info.addr === "h241".U) -> vsepc.value,
	( io.csr_files_info.addr === "h242".U) -> vscause.value,
	( io.csr_files_info.addr === "h243".U) -> vstval.value,
	( io.csr_files_info.addr === "h244".U) -> vsip.value,
	( io.csr_files_info.addr === "h280".U) -> vsatp.value,
	( io.csr_files_info.addr === "hF11".U) -> mvendorid.value,
	( io.csr_files_info.addr === "hF12".U) -> marchid.value,
	( io.csr_files_info.addr === "hF13".U) -> mimpid.value,
	( io.csr_files_info.addr === "hF14".U) -> mhartid.value,
	( io.csr_files_info.addr === "h300".U) -> mstatus.value,
	( io.csr_files_info.addr === "h301".U) -> misa.value,
	( io.csr_files_info.addr === "h302".U) -> medeleg.value,
	( io.csr_files_info.addr === "h303".U) -> mideleg.value,
	( io.csr_files_info.addr === "h304".U) -> mie.value,
	( io.csr_files_info.addr === "h305".U) -> mtvec.value,
	( io.csr_files_info.addr === "h306".U) -> mcounteren.value,
	( io.csr_files_info.addr === "h340".U) -> mscratch.value,
	( io.csr_files_info.addr === "h341".U) -> mepc.value,
	( io.csr_files_info.addr === "h342".U) -> mcause.value,
	( io.csr_files_info.addr === "h343".U) -> mtval.value,
	( io.csr_files_info.addr === "h344".U) -> mip.value,
	( io.csr_files_info.addr === "h34A".U) -> mtinst.value,
	( io.csr_files_info.addr === "h34B".U) -> mtval2.value,
	( io.csr_files_info.addr === "h3A0".U) -> pmpcfg0.value,
	( io.csr_files_info.addr === "h3A2".U) -> pmpcfg2.value,
	( io.csr_files_info.addr === "h3A4".U) -> pmpcfg4.value,
	( io.csr_files_info.addr === "h3A6".U) -> pmpcfg6.value,
	( io.csr_files_info.addr === "h3A8".U) -> pmpcfg8.value,
	( io.csr_files_info.addr === "h3AA".U) -> pmpcfg10.value,
	( io.csr_files_info.addr === "h3AC".U) -> pmpcfg12.value,
	( io.csr_files_info.addr === "h3AE".U) -> pmpcfg14.value,
	( io.csr_files_info.addr === "h3B0".U) -> pmpaddr0.value,
	( io.csr_files_info.addr === "h3B1".U) -> pmpaddr1.value,
	( io.csr_files_info.addr === "h3B2".U) -> pmpaddr2.value,
	( io.csr_files_info.addr === "h3B3".U) -> pmpaddr3.value,
	( io.csr_files_info.addr === "h3B4".U) -> pmpaddr4.value,
	( io.csr_files_info.addr === "h3B5".U) -> pmpaddr5.value,
	( io.csr_files_info.addr === "h3B6".U) -> pmpaddr6.value,
	( io.csr_files_info.addr === "h3B7".U) -> pmpaddr7.value,
	( io.csr_files_info.addr === "h3B8".U) -> pmpaddr8.value,
	( io.csr_files_info.addr === "h3B9".U) -> pmpaddr9.value,
	( io.csr_files_info.addr === "h3BA".U) -> pmpaddr10.value,
	( io.csr_files_info.addr === "h3BB".U) -> pmpaddr11.value,
	( io.csr_files_info.addr === "h3BC".U) -> pmpaddr12.value,
	( io.csr_files_info.addr === "h3BD".U) -> pmpaddr13.value,
	( io.csr_files_info.addr === "h3BE".U) -> pmpaddr14.value,
	( io.csr_files_info.addr === "h3BF".U) -> pmpaddr15.value,
	( io.csr_files_info.addr === "h3C0".U) -> pmpaddr16.value,
	( io.csr_files_info.addr === "h3C1".U) -> pmpaddr17.value,
	( io.csr_files_info.addr === "h3C2".U) -> pmpaddr18.value,
	( io.csr_files_info.addr === "h3C3".U) -> pmpaddr19.value,
	( io.csr_files_info.addr === "h3C4".U) -> pmpaddr20.value,
	( io.csr_files_info.addr === "hB00".U) -> mcycle.value,
	( io.csr_files_info.addr === "hB02".U) -> minstret.value,
	( io.csr_files_info.addr === "hB03".U) -> mhpmcounter3.value,
	( io.csr_files_info.addr === "h320".U) -> mcountinhibit.value,
	( io.csr_files_info.addr === "h323".U) -> mhpmevent3.value,
	( io.csr_files_info.addr === "h7A0".U) -> tselect.value,
	( io.csr_files_info.addr === "h7A1".U) -> tdata1.value,
	( io.csr_files_info.addr === "h7A2".U) -> tdata2.value,
	( io.csr_files_info.addr === "h7A3".U) -> tdata3.value,
	( io.csr_files_info.addr === "h7B0".U) -> dcsr.value,
	( io.csr_files_info.addr === "h7B1".U) -> dpc.value,
	( io.csr_files_info.addr === "h7B2".U) -> dscratch0.value,
	( io.csr_files_info.addr === "h7B3".U) -> dscratch1.value
))


} 
