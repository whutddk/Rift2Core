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

package rift2Core.privilege.csrFiles


import chisel3._
import chisel3.util._
import rift2Core.define._



abstract class CsrFiles_M extends CsrFiles_port {

  val sstatus: UInt
  val sie: UInt

  lazy val is_ssi = mip(1)  & mie(1)  & mstatus(1)
  lazy val is_msi = mip(3)  & mie(3)  & mstatus(3)
  lazy val is_sti = mip(5)  & mie(5)  & mstatus(1)
  lazy val is_mti = mip(7)  & mie(7)  & mstatus(3)
  lazy val is_sei = mip(9)  & mie(9)  & mstatus(1)
  lazy val is_mei = mip(11) & mie(11) & mstatus(3)

  lazy val is_u_ecall = is_ecall & priv_lvl_qout === "b00".U
  lazy val is_s_ecall = is_ecall & priv_lvl_qout === "b01".U
  lazy val is_m_ecall = is_ecall & priv_lvl_qout === "b11".U

  lazy val is_exception =
    is_instr_accessFault    |
    is_instr_illeage        |
    is_breakPoint           |
    is_load_misAlign        |
    is_load_accessFault     |
    is_storeAMO_misAlign    |
    is_storeAMO_accessFault |
    is_u_ecall              |
    is_s_ecall              |
    is_m_ecall              |
    is_instr_pageFault      |
    is_load_pageFault       |
    is_storeAMO_pageFault


  val is_m_interrupt = is_msi | is_mti | is_mei
  val is_s_interrupt = is_ssi | is_sti | is_sei
  val is_interrupt = is_m_interrupt | is_s_interrupt

  lazy val priv_lvl_dnxt = Wire(UInt(2.W))
  lazy val priv_lvl_qout = RegNext(priv_lvl_dnxt, "b11".U(2.W))

    lazy val mpp = mstatus(12,11)
    lazy val spp = mstatus(8)
    priv_lvl_dnxt := Mux1H( Seq(
      is_mRet -> mpp,
      is_sRet -> spp,
    //   is_uRet -> "b00".U,
      is_trap -> "b11".U
    ))


  //user floating point csrs
  lazy val fflags = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h001".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val frm = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h002".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val fcsr = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h003".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //user conter timers
  /**
    * Hardware Performance Monitor -- cycle (read-only)
    * @return a count of the number of clock cycles executed by the ***processor core*** on which the hart is running from an arbitrary start time in the past
    *
    */
  lazy val cycle = {

    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hC00".U, exe_port )
    value := value + 1.U
    value 
  }

  /**
    * Hardware Performance Monitor -- time (read-only)
    * @return a count of the number of ***rtc*** cycles executed by the ***processor core*** on which the hart is running from an arbitrary start time in the past
    *
    */
  lazy val time = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hC01".U, exe_port )
    val rtc_toggle = {
      val rtc_0 = ShiftRegister(rtc_clock, 3)
      val rtc_1 = ShiftRegister(rtc_clock, 4)
      rtc_0 ^ rtc_1
    }
    when(rtc_toggle) { value := value + 1.U }
    value 
  }
  
  /**
    * Hardware Performance Monitor -- instret (read-only)
    * 
    * @return the number of instructions the hart has retired
    */

  lazy val instret = {
    val value = RegInit(0.U(64.W))
    value := value + retired_cnt
    value 
  }

  lazy val hpmcounter = 
    for ( i <- 0 until 32 ) yield {
      if ( i == 0 || i == 1 || i == 2 ) { 0.U }
      else {
        val value = RegInit(0.U(64.W))
        val (enable, dnxt) = Reg_Exe_Port( value, "hC00".U + i.U, exe_port )
        when(enable) { value := dnxt }
        value 
      }
    }







  //machine information register
  lazy val mvendorid = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hF11".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val marchid = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hF12".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val mimpid = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hF13".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val mhartid = {
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

  lazy val mstatus = {
    val sd = sstatus(63)
    val mbe = RegInit(0.U(1.W))
    val sbe = RegInit(0.U(1.W))
    val sxl = WireDefault(2.U(2.W))
    val uxl = sstatus(33,32)
    val tsr = RegInit(0.U(1.W))
    val tw = RegInit(0.U(1.W))
    val tvm = RegInit(0.U(1.W))
    val mxr = sstatus(19)
    val sum = sstatus(18)
    val mprv = RegInit(0.U(1.W))
    val xs = sstatus(16,15)
    val fs = sstatus(14,13)
    val mpp = RegInit(0.U(2.W))
    val spp = sstatus(8)
    val mpie = RegInit(0.U(1.W))
    val ube = sstatus(6)
    val spie = sstatus(5)
    val mie = RegInit(0.U(1.W))
    val sie = sstatus(1)

    val value = Cat( sd, 0.U(25.W), mbe, sbe, sxl, uxl, 0.U(9.W), tsr, tw, tvm, mxr, sum, mprv, xs, fs, mpp, 0.U(2.W), spp, mpie, ube, spie, 0.U(1.W), mie, 0.U(1.W), sie, 0.U(1.W) )

    val (enable, dnxt) = Reg_Exe_Port( value, "h300".U, exe_port )

    when( is_trap ) {
      mpie := Mux( priv_lvl_dnxt === "b11".U, mie, mpie )
      mie  := Mux( priv_lvl_dnxt === "b11".U, 0.U, mie )
      mpp  := Mux( priv_lvl_dnxt === "b11".U, priv_lvl_qout, mpp )

    }
    .elsewhen( is_xRet ) {
      mie  := Mux( is_mRet, mpie, mie )
      mpie := Mux( is_mRet, 1.U, mpie )
      mpp  := Mux( is_mRet, "b00".U, mpp )

      mprv := Mux( (is_mRet & mpp =/= "b11".U) | is_sRet, 0.U, mprv )
    }
    .elsewhen(enable) {

      mbe  := dnxt(37)
      sbe  := dnxt(36)
      tsr  := dnxt(22)
      tw   := dnxt(21)
      tvm  := dnxt(20)
      mprv := dnxt(17)
      mpp  := dnxt(12,11)
      mpie := dnxt(7)
      mie  := dnxt(3)
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
  lazy val misa = {
    val mxl = RegInit(2.U(2.W))
    val extensions = RegInit("b00000101000011000100000100".U(26.W))
    val value = Cat(mxl, 0.U(36.W), extensions)

    val (enable, dnxt) = Reg_Exe_Port( value, "h301".U, exe_port )
    when(enable) {mxl := dnxt(63,62); extensions := dnxt(25,0) }

    value
  }
  
  
  

  
  /**
    * Machine Trap Delegation Register
    * 
    * By default, the exception will be handled in M-mode, when the bits set, it's handled in S-mode
    */
  lazy val medeleg = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h302".U, exe_port )
    value 
  }

  /**
    * Machine Trap Delegation Register
    * 
    * By default, the interrupt will be handled in M-mode, when the bits set, it's handled in S-mode
    */
  lazy val mideleg = {
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
  lazy val mie = {
    val meie = RegInit(0.U(1.W))
    val seie = sie(9)
    val mtie = RegInit(0.U(1.W))
    val stie = sie(5)
    val msie = RegInit(0.U(1.W))
    val ssie = sie(1)
    val value = Cat( 0.U(4.W), meie, 0.U(1.W), seie, 0.U(1.W), mtie, 0.U(1.W), stie, 0.U(1.W), msie, 0.U(1.W),ssie, 0.U(1.W) )

    val (enable, dnxt) = Reg_Exe_Port( value, "h304".U, exe_port )

    when(enable) {
      meie := dnxt(11)
      mtie := dnxt(7)
      msie := dnxt(3)
    }
    value
  }


  /**
    * Machine Trap-Vector Base-Address Register
    * holds trap vector configuration, consisting of a vector of a vector base address and a bector mode 
    * @param base (63,2)
    * @param mode (1,0) read only in this version
    */
  lazy val mtvec = {
    val base = RegInit(0.U(62.W))
    val mode = WireDefault(0.U(2.W))
    val value = Cat( base, mode )
    val (enable, dnxt) = Reg_Exe_Port( value, "h305".U, exe_port )
    when(enable) { base := dnxt(63,2) & ~("b11".U(62.W)) }
    value
  }

  /**
    * Machine Counter-Enable Register -- mcounteren
    *
    * @param HPM (31,3) Whether is allowed to access hpmcounter(x) in S-mode
    * @param IR (2) Whether is allowed to access instret in S-mode
    * @param TM (1) Whether is allowed to access time in S-mode
    * @param CY (0) Whether is allowed to access cycle in S-mode
    */
  lazy val mcounteren = {
    val hpm = RegInit(0.U(32.W))
    val ir  = RegInit(0.U(1.W))
    val tm  = RegInit(0.U(1.W))
    val cy  = RegInit(0.U(1.W))
    val value = Cat( hpm(31,3), ir, tm, cy )
    val (enable, dnxt) = Reg_Exe_Port( value, "h306".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }



  //Machine Trap Handling
  /**
    * Machine Scratch Register -- mscratch
    *
    * it's used to hold a pointer to a M-mode hart-local context space and swapped with a user register upon entry to an M-mode trap handler
    */
  lazy val mscratch = {
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
  lazy val mepc = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h341".U, exe_port )

    when(is_trap & priv_lvl_dnxt === "b11".U){
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

  lazy val mcause = {
    val interrupt = RegInit(0.U(1.W))
    val exception_code = RegInit(0.U(63.W))
    val value = Cat(interrupt, exception_code)
    val (enable, dnxt) = Reg_Exe_Port( value, "h342".U, exe_port )

    when( (is_m_interrupt) & priv_lvl_dnxt === "b11".U ) {
      interrupt := 1.U
      exception_code := Mux1H( Seq(
        // is_ssi -> 1.U,
        is_msi -> 3.U,
        // is_sti -> 5.U,
        is_mti -> 7.U,
        // is_sei -> 9.U,
        is_mei -> 11.U
      ))
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
  lazy val mtval = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h343".U, exe_port )
    when( priv_lvl_dnxt === "b11".U ) {
      value := Mux1H( Seq(
        is_instr_accessFault    -> ill_vaddr,
        is_instr_illeage        -> ill_instr,
        is_breakPoint           -> ill_vaddr,
        is_load_misAlign        -> ill_vaddr,
        is_load_accessFault     -> ill_vaddr,
        is_storeAMO_misAlign    -> ill_vaddr,
        is_storeAMO_accessFault -> ill_vaddr,
        is_instr_pageFault      -> ill_vaddr,
        is_load_pageFault       -> ill_vaddr,
        is_storeAMO_pageFault   -> ill_vaddr       
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
  lazy val mip = {
    val meip = clint_ex_m
    val seip = clint_ex_s
    val mtip = clint_tm_m
    val stip = clint_tm_s
    val msip = clint_sw_m
    val ssip = clint_sw_s
    val value =
      Cat(
        0.U(4.W), meip, 0.U(1.W), seip,
        0.U(1.W), mtip, 0.U(1.W), stip,
        0.U(1.W), msip, 0.U(1.W), ssip, 0.U(1.W) )

    value
  }

  lazy val mtinst = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h34A".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val mtval2 = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h34B".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }


  //Machine Memory Protection
  lazy val pmpcfg = 
    for ( i <- 0 until 16 ) yield {
      val value = RegInit(0.U(64.W))
      val (enable, dnxt) = Reg_Exe_Port( value, "h3A0".U + i.U, exe_port )
      when(enable) { value := dnxt }
      value 
    }


  lazy val pmpaddr = 
    for ( i <- 0 until 64 ) yield {
      val value = RegInit(0.U(64.W))
      val (enable, dnxt) = Reg_Exe_Port( value, "h3B0".U + i.U, exe_port )
      when(enable) { value := dnxt }
      value 
    }


  //Machine Counter/Timer

  //0xb00
  /**
    * Hardware Performance Monitor -- mcycle
    *
    * @return the number of clock cycles executed by the processor core
    */
  lazy val mcycle = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hB00".U, exe_port )
    when(enable) { value := dnxt }
    .otherwise { value := value + 1.U }
    value 
  }

  /**
    * Hardware Performance Monitor -- minstret
    *
    * @return the number of instructions the hart has retired
    */
  lazy val minstret = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hB02".U, exe_port )
    when(enable) { value := dnxt }
    .otherwise { value := value + retired_cnt }
    value 
  }

  /**
    * Hardware Performance Monitor -- mhpmcounter 3~31
    *
    * @return 
    */
  lazy val mhpmcounter = 
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
  /**
    * 
    * when set, the counter will not increase, all hard-wire to 0 in this version
    * 
    */
  lazy val mcountinhibit = {
    val value = WireDefault(0.U(64.W))
    // val (enable, dnxt) = Reg_Exe_Port( value, "h320".U, exe_port )
    // when(enable) { value := dnxt }
    value 
  }

  lazy val mhpmevent = 
    for ( i <- 0 until 32 ) yield {
      if ( i == 0 || i == 1 || i == 2 ) { 0.U }
      else {
        val value = RegInit(0.U(64.W))
        val (enable, dnxt) = Reg_Exe_Port( value, "hB20".U + i.U, exe_port )
        when(enable) { value := dnxt }
        value 
    }
  }


}