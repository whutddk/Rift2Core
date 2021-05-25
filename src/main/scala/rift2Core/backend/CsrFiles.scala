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

// class Pri_Port extends Bundle {
//   val en = Bool()
//   val dat = UInt(64.W)
// }


object Reg_Exe_Port {

  def apply( csr_reg: UInt, addr: UInt, ep: Exe_Port ): (Bool, UInt) = {
    val enable = (ep.addr === addr) & (ep.op_rw | ep.op_rs | ep.op_rc)
    val dnxt = Mux1H(Seq(
        ep.op_rw -> ( ep.dat_i),
        ep.op_rs -> (csr_reg | ep.dat_i),
        ep.op_rc -> (csr_reg & ~ep.dat_i),
      ))
    return (enable, dnxt)
  }
}



// object CsrReg {
//   def apply( addr: UInt, init: UInt, ormask: UInt, pp: Pri_Port, ep: Exe_Port ) = {
//     val csr_reg = RegInit(init)

//     when( pp.en ) {
//       csr_reg := pp.dat | ormask 
//     }
//     .elsewhen(ep.addr === addr) {
//       csr_reg := MuxCase(csr_reg, Array(
//         ep.op_rw -> ( ep.dat_i),
//         ep.op_rs -> (csr_reg | ep.dat_i),
//         ep.op_rc -> (csr_reg & ~ep.dat_i),
//       )) | ormask 
//     }

//     csr_reg
//   }
// }



trait CsrFiles {
  val exe_port = Wire(new Exe_Port)
  val is_csr_trap = Wire( Bool() )
  val is_csr_mRet = Wire( Bool() )
  val is_csr_sRet = Wire( Bool() )
  val is_csr_uRet = Wire( Bool() )
  val is_csr_xRet = is_csr_mRet | is_csr_sRet | is_csr_uRet

  val commit_pc = Wire(UInt(64.W))


  //user trap setup
  val ustatus = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h000".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val uie = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h004".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val utvec = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h005".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //user trap handling
  val uscratch = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h040".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val uepc = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h041".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val ucause = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h042".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val utval = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h043".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val uip = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h044".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //user floating point csrs
  val fflags = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h001".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val frm = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h002".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val fcsr = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h003".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //user conter timers
  val cycle = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hC00".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val time = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hC01".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }
  
  val instret = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hC02".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val hpmcounter = 
    for ( i <- 0 until 32 ) yield {
      if ( i == 0 || i == 1 || i == 2 ) { 0.U }
      else {
        val value = RegInit(0.U(64.W))
        val (enable, dnxt) = Reg_Exe_Port( value, "hC00".U + i.U, exe_port )
        when(enable) { value := dnxt }
        value 
      }
    }


  //supervisor trap setup
  val sstatus = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h100".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val sedeleg = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h102".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val sideleg = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h103".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val sie = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h104".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val stvec = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h105".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val scounteren = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h106".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //supervisor trap handling

  val sscratch = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h140".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val sepc = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h141".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val scause = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h142".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val stval = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h143".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val sip = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h144".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //supervisor protection and translation
  val satp = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h180".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //hypervisor trap setup
  val hstatus = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h600".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val hedeleg = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h602".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val hideleg = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h603".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val hie = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h604".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val hcounteren = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h606".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val hgeie = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h607".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //hypervisor trap handling
  val htval = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h643".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val hip = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h644".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val hvip = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h645".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val htinst = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h64A".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val hgeip = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hE12".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //hypervisor protection and translation
  val hgatp = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h680".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //hypervisor counter timer virtualization registers
  val htimedelta = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h605".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //virtual supervisor registers
  val vsstatus = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h200".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val vsie = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h204".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val vstvec = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h205".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val vsscratch = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h240".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val vsepc = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h241".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val vscause = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h242".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val vstval = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h243".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val vsip = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h244".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val vsatp = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h280".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }




  val priv_lvl_dnxt = Wire(UInt(2.W))
  val priv_lvl_qout = RegNext(priv_lvl_dnxt, "b11".U(2.W))

    val mpp = mstatus(12,11)
    val spp = mstatus(8)
    priv_lvl_dnxt := Mux1H( Seq(
      is_csr_mRet -> mpp,
      is_csr_sRet -> spp,
      is_csr_uRet -> "b00".U,
      is_csr_trap -> "b11".U
    ))











  //machine information register
  val mvendorid = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hF11".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val marchid = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hF12".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val mimpid = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hF13".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val mhartid = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hF14".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }


  //Machine Trap Setup

  /**
    * Machine Status Registers
    * @param SD (63) whether either fs or xs is dirty
    * @param MBE (37) endian of M-mode, when 0, is little-endian
    * @param SBE (36) endian of S-mode, when 0, is little-endian
    * @param SXL (35,34) XLEN of S-mode, hard-wire to 2.U(64.bits)
    * @param UXL (33,32) XLEN of U-mode, hard-wire to 2.U(64.bits)
    * @param TSR (22) trap sret, when 1, sret in s-mode cause illegal instruction exception
    * @param TW (21) Time out wait for WFI, when 1 wfi cause illegal instruction exception in U\S-MODE
    * @param TVM (20) Trap Virtual Memory, when 1, access satp or Sfence.vma will cause illegal instruction exception
    * @param MXR (19) Make executable Readable. When 0, only loads form pages marked readable
    * @param SUM (18) permit Supervisor User Memory access. When 0, S-mode accesses to page.U === 1 will fault.
    * @param MPRV (17) Memory Privilege; When 1, load store using trans&protect in M-mode; When MRET or SRet to S\U-mode, set to 0.
    * @param XS (16,15) aditional user-mode extension and associated state
    * @param FS (14,13) float point statuw=s
    * @param MPP (12,11) Previous Mode is U-mode or S-mode or M-mode? When MRet, privilege mode update to MPP, MPP set to "U"
    * @param SPP (8) Previous Mode is U-mode or S-mode? When SRet, privilege mode update to SPP, SPP set to "U"
    * @param MPIE (7)  When MRet, MPIE set to 1
    * @param UBE (6) endian of U-mode, when 0, is little-endian
    * @param SPIE (5) When SRet, SPIE set to 1
    * @param MIE (3) M-mode Interrupt Enable; When MRet, update to MPIE
    * @param SIE (1) S-mode Interrupt Enable; When SRet, update to SPIE
    */
  val mstatus = {
    val sd = RegInit(0.U(1.W))
    val mbe = RegInit(0.U(1.W))
    val sbe = RegInit(0.U(1.W))
    val sxl = WireDefault(2.U(2.W))
    val uxl = WireDefault(2.U(2.W))
    val tsr = RegInit(0.U(1.W))
    val tw = RegInit(0.U(1.W))
    val tvm = RegInit(0.U(1.W))
    val mxr = RegInit(0.U(1.W))
    val sum = RegInit(0.U(1.W))
    val mprv = RegInit(0.U(1.W))
    val xs = RegInit(0.U(2.W))
    val fs = RegInit(0.U(2.W))
    val mpp = RegInit(0.U(2.W))
    val spp = RegInit(0.U(1.W))
    val mpie = RegInit(0.U(1.W))
    val ube = RegInit(0.U(1.W))
    val spie = RegInit(0.U(1.W))
    val mie = RegInit(0.U(1.W))
    val sie = RegInit(0.U(1.W))

    val value = Cat( sd, 0.U(25.W), mbe, sbe, sxl, uxl, 0.U(9.W), tsr, tw, tvm, mxr, sum, mprv, xs, fs, mpp, 0.U(2.W), spp, mpie, ube, spie, 0.U(1.W), mie, 0.U(1.W), sie, 0.U(1.W) )

    val (enable, dnxt) = Reg_Exe_Port( value, "h300".U, exe_port )

    when( is_csr_trap ) {
      mpie := Mux( priv_lvl_dnxt === "b11".U, mie, mpie )
      spie := Mux( priv_lvl_dnxt === "b01".U, sie, spie )
      mie  := Mux( priv_lvl_dnxt === "b11".U, 0.U, mie )
      sie  := Mux( priv_lvl_dnxt === "b01".U, 0.U, sie )

      mpp  := Mux( priv_lvl_dnxt === "b11".U, priv_lvl_qout, mpp )
      spp  := Mux( priv_lvl_dnxt === "b01".U, priv_lvl_qout, spp )
    }
    .elsewhen( is_csr_xRet ) {
      mie  := Mux( is_csr_mRet, mpie, mie )
      sie  := Mux( is_csr_sRet, spie, sie )
      mpie := Mux( is_csr_mRet, 1.U, mpie )
      spie := Mux( is_csr_sRet, 1.U, spie )

      mpp  := Mux( is_csr_mRet, "b00".U, mpp )
      spp  := Mux( is_csr_sRet, "b00".U, spp )

      mprv := Mux( (is_csr_mRet & mpp =/= "b11".U) | is_csr_sRet, 0.U, mprv )
    }
    .elsewhen(enable) {
      sd   := dnxt(63)
      mbe  := dnxt(37)
      sbe  := dnxt(36)
      tsr  := dnxt(22)
      tw   := dnxt(21)
      tvm  := dnxt(20)
      mxr  := dnxt(19)
      sum  := dnxt(18)
      mprv := dnxt(17)
      xs   := dnxt(16,15)
      fs   := dnxt(14,13)
      mpp  := dnxt(12,11)
      spp  := dnxt(8)
      mpie := dnxt(7)
      ube  := dnxt(6)
      spie := dnxt(5)
      mie  := dnxt(3)
      sie  := dnxt(1)
    }
    value
  }
  /**
    * Machine ISA register 
    * @param MXL (63,62) is 2.U for XLEN of RiftCore is 64 
    * @param Extensions (25:0) 
    * @note U(20): User mode implement S(18): Supervisor mode implemented N(13): User-level interrupts supported
    * @note M(12): Integer Multiply/Divide extension I(8): RV64I base ISA C(2): Compressed extension
    * 
    */
  val misa = {
    val mxl = RegInit(2.U(2.W))
    val extensions = RegInit("b00000101000011000100000100".U(26.W))
    val value = Cat(mxl, 0.U(36.W), extensions)

    val (enable, dnxt) = Reg_Exe_Port( value, "h301".U, exe_port )
    when(enable) {mxl := dnxt(63,62); extensions := dnxt(25,0) }

    value
  }
  
  
  
  // CsrReg( "h301".U, Cat("b10".U, 0.U(36.W), "b00000101000011000100000100".U), 0.U, 0.U.asTypeOf(new Pri_Port), exe_port)
  
  
  /**
    * Machine Trap Delegation Register
    * 
    * By default, the exception will be handled in M-mode, when the bits set, it's handled in S-mode
    */
  val medeleg = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h302".U, exe_port )
    value 
  }

  /**
    * Machine Trap Delegation Register
    * 
    * By default, the interrupt will be handled in M-mode, when the bits set, it's handled in S-mode
    */
  val mideleg = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h303".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }
  
  /**
    * Machine Interrupt Registers
    *
    * @param meie (11)
    * @param seie (9)
    * @param mtie (7)
    * @param stie (5)
    * @param msie (3)
    * @param ssie (1)
    */
  val mie = {
    val meie = RegInit(0.U(1.W))
    val seie = RegInit(0.U(1.W))
    val mtie = RegInit(0.U(1.W))
    val stie = RegInit(0.U(1.W))
    val msie = RegInit(0.U(1.W))
    val ssie = RegInit(0.U(1.W))
    val value = Cat( 0.U(4.W), meie, 0.U(1.W), seie, 0.U(1.W), mtie, 0.U(1.W), stie, 0.U(1.W), msie, 0.U(1.W),ssie, 0.U(1.W) )

    val (enable, dnxt) = Reg_Exe_Port( value, "h304".U, exe_port )

    when(enable) {
      meie := dnxt(11)
      seie := dnxt(9)
      mtie := dnxt(7)
      stie := dnxt(5)
      msie := dnxt(3)
      ssie := dnxt(1)
    }
    value
  }


  /**
    * Machine Trap-Vector Base-Address Register
    * holds trap vector configuration, consisting of a vector of a vector base address and a bector mode 
    * @param base (63,2)
    * @param mode (1,0)
    */
  val mtvec = {
    val base = RegInit(0.U(62.W))
    val mode = RegInit(0.U(2.W))
    val value = Cat( base, mode )
    val (enable, dnxt) = Reg_Exe_Port( value, "h305".U, exe_port )
    when(enable) { base := dnxt(63,2); mode := dnxt(1,0) }
    value
  }

  val mcounteren = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h306".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }



  //Machine Trap Handling
  val mscratch = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h340".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  /**
    * Machine Exception Program Counter
    * @note hold all valid virtual addresses 
    * when a ***trap*** is taken into ***M-mode***, update to the ***virtual address*** that was interrupted or encountered the exception 
    */
  val mepc = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h341".U, exe_port )

    when(is_csr_trap & priv_lvl_dnxt === "b11".U){
      value := commit_pc
    }
    .elsewhen(enable) { value := dnxt }

    value & ~(1.U(64.W))
  }



  /**
    * Machine Cause Register
    * 
    * when a ***trap*** is taken into ***M-mode***, Indicating the event that caused the trap
    * @param interrupt
    * @param exception_code
    */

  val mcause = {
    val interrupt = RegInit(0.U(1.W))
    val exception_code = RegInit(0.U(63.W))
    val value = Cat(interrupt, exception_code)
    val (enable, dnxt) = Reg_Exe_Port( value, "h342".U, exe_port )

    when( is_interrupt & priv_lvl_dnxt === "b11".U ) {
      interrupt := 1.U
      exception_code := Mux1H(
        is_ssi -> 1.U,
        is_msi -> 3.U,
        is_sti -> 5.U,
        is_mti -> 7.U,
        is_sei -> 9.U,
        is_mei -> 11.U
      )
    }
    .elsewhen( is_exception & priv_lvl_dnxt === "b11".U ) {
      interrupt := 0.U
      exception_code := Mux1H( Seq(
        is_instr_accessFault    -> 1.U,
        is_instr_illeage        -> 2.U,
        is_breakPoint           -> 3.U,
        is_load_misAlign        -> 4.U,
        is_load_accessFault     -> 5.U,
        is_storeAMO_misAlign    -> 6.U,
        is_storeAMO_accessFault -> 7.U,
        is_u_ecall              -> 8.U,
        is_s_ecall              -> 9.U,
        is_m_ecall              -> 11.U,
        is_instr_pageFault      -> 12.U,
        is_load_pageFault       -> 13.U,
        is_storeAMO_pageFault   -> 15.U
      ))
    }
    .elsewhen(enable) {
      interrupt := dnxt(63)
      exception_code := dnxt(62,0)
    }

    value
  }
  

  /**
    * Machine Trap Value Register
    * 
    * When a trap is taken into ***M-mode***, update to ***virtual address*** or ***faulting instruction***
    */
  val mtval = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h343".U, exe_port )
    when( priv_lvl_dnxt === "b11".U ) {
      value := Mux1H( Seq(
        is_instr_accessFault    -> commit_pc,
        is_instr_illeage        -> ill_instr,
        is_breakPoint           -> commit_pc,
        is_load_misAlign        -> commit_pc,
        is_load_accessFault     -> commit_pc,
        is_storeAMO_misAlign    -> commit_pc,
        is_storeAMO_accessFault -> commit_pc,
        is_instr_pageFault      -> commit_pc,
        is_load_pageFault       -> commit_pc,
        is_storeAMO_pageFault   -> commit_pc       
      ))
    }
    .elsewhen(enable) { value := dnxt }

    value 
  }


  /**
    * Machine Interrupt Registers
    * 
    * @note implemented in read-only mode
    *
    * @param meip (11)
    * @param seip (9)
    * @param mtip (7)
    * @param stip (5)
    * @param msip (3)
    * @param ssip (1)
    */
  val mip = {
    val meip = Wire(UInt(1.W))
    val seip = Wire(UInt(1.W))
    val mtip = Wire(UInt(1.W))
    val stip = Wire(UInt(1.W))
    val msip = Wire(UInt(1.W))
    val ssip = Wire(UInt(1.W))
    val value = Cat( 0.U(4.W), meip, 0.U(1.W), seip, 0.U(1.W), mtip, 0.U(1.W), stip, 0.U(1.W), msip, 0.U(1.W),ssip, 0.U(1.W) )

    value
  }

  val mtinst = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h34A".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val mtval2 = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h34B".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }


  //Machine Memory Protection
  val pmpcfg = 
    for ( i <- 0 until 16 ) yield {
      val value = RegInit(0.U(64.W))
      val (enable, dnxt) = Reg_Exe_Port( value, "h3A0".U + i.U, exe_port )
      when(enable) { value := dnxt }
      value 
    }


  val pmpaddr = 
    for ( i <- 0 until 64 ) yield {
      val value = RegInit(0.U(64.W))
      val (enable, dnxt) = Reg_Exe_Port( value, "h3B0".U + i.U, exe_port )
      when(enable) { value := dnxt }
      value 
    }


  //Machine Counter/Timer

  //0xb00
  val mcycle = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hB00".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val minstret = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hB02".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val mhpmcounter = 
  for ( i <- 0 until 32 ) yield {
    if ( i == 0 || i == 1 || i == 2 ) { 0.U }
    else {
      val value = RegInit(0.U(64.W))
      val (enable, dnxt) = Reg_Exe_Port( value, "hB00".U + i.U, exe_port )
      when(enable) { value := dnxt }
      value 
    }
  }



  //Machine Counter Setup
  val mcountinhibit = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h320".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val mhpmevent = 
  for ( i <- 0 until 32 ) yield {
    if ( i == 0 || i == 1 || i == 2 ) { 0.U }
    else {
      val value = RegInit(0.U(64.W))
      val (enable, dnxt) = Reg_Exe_Port( value, "hB20".U + i.U, exe_port )
      when(enable) { value := dnxt }
      value 
    }
  }
  
  
  //Debug/Trace Register
  val tselect = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7A0".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }
  val tdata1 = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7A1".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val tdata2 = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7A2".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val tdata3 = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7A3".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //Debug Mode Register
  val dcsr = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B0".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val dpc = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B1".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val dscratch0 = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B2".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val dscratch1 = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B3".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }







  def csr_read(addr: UInt) = {

    val pmpcfg_arr = {
      val addr_chk = for ( i <- 0 until 16 ) yield { addr === ("h3A0".U + i.U) }
      val reg_sel  = for ( i <- 0 until 16 ) yield { pmpcfg(i) }
      addr_chk zip reg_sel
    }

    val pmpaddr_arr = {
      val addr_chk = for ( i <- 0 until 64 ) yield { addr === ("h3B0".U + i.U) }
      val reg_sel  = for ( i <- 0 until 64 ) yield { pmpaddr(i) }
      addr_chk zip reg_sel
    }

    val hpmcounter_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("hC00".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { hpmcounter(i) }
      addr_chk zip reg_sel
    }

    val mhpmcounter_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("hB00".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { mhpmcounter(i) }
      addr_chk zip reg_sel      
    }

    val mhpmevent_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("h320".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { mhpmevent(i) }
      addr_chk zip reg_sel      
    }

    val normal_arr = Array(
          ( addr === "h000".U ) -> ustatus,
          ( addr === "h004".U ) -> uie,
          ( addr === "h005".U ) -> utvec,
          ( addr === "h040".U ) -> uscratch,
          ( addr === "h041".U ) -> uepc,
          ( addr === "h042".U ) -> ucause,
          ( addr === "h043".U ) -> utval,
          ( addr === "h044".U ) -> uip,
          ( addr === "h001".U ) -> fflags,
          ( addr === "h002".U ) -> frm,
          ( addr === "h003".U ) -> fcsr,
          ( addr === "hC00".U ) -> cycle,
          ( addr === "hC01".U ) -> time,
          ( addr === "hC02".U ) -> instret,
          ( addr === "h100".U ) -> sstatus,
          ( addr === "h102".U ) -> sedeleg,
          ( addr === "h103".U ) -> sideleg,
          ( addr === "h104".U ) -> sie,
          ( addr === "h105".U ) -> stvec,
          ( addr === "h106".U ) -> scounteren,
          ( addr === "h140".U ) -> sscratch,
          ( addr === "h141".U ) -> sepc,
          ( addr === "h142".U ) -> scause,
          ( addr === "h143".U ) -> stval,
          ( addr === "h144".U ) -> sip,
          ( addr === "h180".U ) -> satp,
          ( addr === "h600".U ) -> hstatus,
          ( addr === "h602".U ) -> hedeleg,
          ( addr === "h603".U ) -> hideleg,
          ( addr === "h604".U ) -> hie,
          ( addr === "h606".U ) -> hcounteren,
          ( addr === "h607".U ) -> hgeie,
          ( addr === "h643".U ) -> htval,
          ( addr === "h644".U ) -> hip,
          ( addr === "h645".U ) -> hvip,
          ( addr === "h64A".U ) -> htinst,
          ( addr === "hE12".U ) -> hgeip,
          ( addr === "h680".U ) -> hgatp,
          ( addr === "h605".U ) -> htimedelta,
          ( addr === "h200".U ) -> vsstatus,
          ( addr === "h204".U ) -> vsie,
          ( addr === "h205".U ) -> vstvec,
          ( addr === "h240".U ) -> vsscratch,
          ( addr === "h241".U ) -> vsepc,
          ( addr === "h242".U ) -> vscause,
          ( addr === "h243".U ) -> vstval,
          ( addr === "h244".U ) -> vsip,
          ( addr === "h280".U ) -> vsatp,
          ( addr === "hF11".U ) -> mvendorid,
          ( addr === "hF12".U ) -> marchid,
          ( addr === "hF13".U ) -> mimpid,
          ( addr === "hF14".U ) -> mhartid,
          ( addr === "h300".U ) -> mstatus,
          ( addr === "h301".U ) -> misa,
          ( addr === "h302".U ) -> medeleg,
          ( addr === "h303".U ) -> mideleg,
          ( addr === "h304".U ) -> mie,
          ( addr === "h305".U ) -> mtvec,
          ( addr === "h306".U ) -> mcounteren,
          ( addr === "h340".U ) -> mscratch,
          ( addr === "h341".U ) -> mepc,
          ( addr === "h342".U ) -> mcause,
          ( addr === "h343".U ) -> mtval,
          ( addr === "h344".U ) -> mip,
          ( addr === "h34A".U ) -> mtinst,
          ( addr === "h34B".U ) -> mtval2,

          ( addr === "hB00".U ) -> mcycle,
          ( addr === "hB02".U ) -> minstret,
          ( addr === "h320".U ) -> mcountinhibit,
          ( addr === "h7A0".U ) -> tselect,
          ( addr === "h7A1".U ) -> tdata1,
          ( addr === "h7A2".U ) -> tdata2,
          ( addr === "h7A3".U ) -> tdata3,
          ( addr === "h7B0".U ) -> dcsr,
          ( addr === "h7B1".U ) -> dpc,
          ( addr === "h7B2".U ) -> dscratch0,
          ( addr === "h7B3".U ) -> dscratch1
        )

    Mux1H(pmpcfg_arr ++ pmpaddr_arr ++ hpmcounter_arr ++ mhpmcounter_arr ++ mhpmevent_arr ++ normal_arr )
  }
} 
