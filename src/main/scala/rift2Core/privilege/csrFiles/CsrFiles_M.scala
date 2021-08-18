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


  val is_ssi = mip(1)  & mie(1)  & mstatus(1)
  val is_msi = mip(3)  & mie(3)  & mstatus(3)
  val is_sti = mip(5)  & mie(5)  & mstatus(1)
  val is_mti = mip(7)  & mie(7)  & mstatus(3)
  val is_sei = mip(9)  & mie(9)  & mstatus(1)
  val is_mei = mip(11) & mie(11) & mstatus(3)



  val is_exception =
    is_instr_access_fault   |
    is_instr_paging_fault   |
    is_instr_illeage        |
    is_breakPoint           |
    is_load_misAlign        |
    is_load_access_fault     |
    is_storeAMO_misAlign    |
    is_storeAMO_access_fault |
    is_storeAMO_paging_fault |
    is_ecall_M              |
    is_ecall_S              |
    is_ecall_U              |
    is_load_paging_fault       


  lazy val is_m_interrupt = is_msi | is_mti | is_mei
  lazy val is_s_interrupt = is_ssi | is_sti | is_sei
  lazy val is_interrupt = is_m_interrupt | is_s_interrupt






  //user floating point csrs
  fflags := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h001".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  frm := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h002".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  fcsr := {
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
  cycle := {

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
  time := {
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

  instret := {
    val value = RegInit(0.U(64.W))
    value := value + retired_cnt
    value 
  }

  
  for ( i <- 0 until 32 ) yield {
    hpmcounter(i) := {
      if ( i == 0 || i == 1 || i == 2 ) { 0.U }
      else {
        val value = RegInit(0.U(64.W))
        val (enable, dnxt) = Reg_Exe_Port( value, "hC00".U + i.U, exe_port )
        when(enable) { value := dnxt }
        value 
      }        
    }
  }







  //machine information register
  mvendorid := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hF11".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  marchid := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hF12".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  mimpid := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hF13".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  mhartid := {
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


  mstatus := {
    val sd = RegInit(0.U(1.W))
    val mbe = RegInit(0.U(1.W))
    val sbe = RegInit(0.U(1.W))
    val sxl = 2.U(2.W)
    val uxl = 2.U(2.W)
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

    val (enable0, dnxt0) = Reg_Exe_Port( value, "h100".U, exe_port )
    val (enable1, dnxt1) = Reg_Exe_Port( value, "h300".U, exe_port )

    when( is_trap ) {
      when( priv_lvl_dnxt === "b11".U ) {
        mpie := mie
        mie  := 0.U
        mpp  := priv_lvl_qout
      }
      when( priv_lvl_dnxt === "b01".U ) {
        spp  := Mux( priv_lvl_qout === "b00".U, 0.U, 1.U )
        spie := sie
        sie  := 0.U
      }
    }
    .elsewhen( is_mRet ) {
      mie  := mpie
      mpie := 1.U
      mpp  := "b00".U

      mprv := Mux( mpp =/= "b11".U, 0.U, mprv )
    }
    .elsewhen( is_sRet ) {
      spie := 1.U
      sie  := spie
    }
    .elsewhen(enable0) {
      sd   := dnxt0(63)
      mxr  := dnxt0(19)
      sum  := dnxt0(18)
      xs   := dnxt0(16,15)
      fs   := dnxt0(14,13)
      spp  := dnxt0(8)
      ube  := dnxt0(6)
      spie := dnxt0(5)
      sie  := dnxt0(1)
    }
    .elsewhen(enable1) {
      sd   := dnxt1(63)
      mbe  := dnxt1(37)
      sbe  := dnxt1(36)
      tsr  := dnxt1(22)
      tw   := dnxt1(21)
      tvm  := dnxt1(20)
      mxr  := dnxt1(19)
      sum  := dnxt1(18)
      mprv := dnxt1(17)
      xs   := dnxt1(16,15)
      fs   := dnxt1(14,13)
      mpp  := dnxt1(12,11)
      spp  := dnxt1(8)
      mpie := dnxt1(7)
      ube  := dnxt1(6)
      spie := dnxt1(5)
      mie  := dnxt1(3)
      sie  := dnxt1(1)
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
  misa := {
    val mxl = WireDefault(2.U(2.W))
    val extensions = {
      if (true) {
        WireDefault("b00000101000001000100000101".U(26.W))  
      }
      else {
        WireDefault("b00000000000001000100000101".U(26.W))
      }
    
    }

    val value = Cat(mxl, 0.U(36.W), extensions)



    value
  }
  
  
  

  
  /**
    * Machine Trap Delegation Register
    * 
    * By default, the exception will be handled in M-mode, when the bits set, it's handled in S-mode
    */
  medeleg := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h302".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  /**
    * Machine Trap Delegation Register
    * 
    * By default, the interrupt will be handled in M-mode, when the bits set, it's handled in S-mode
    */
  mideleg := {
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
  mie := {
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
  mtvec := {
    val base = RegInit(0.U(62.W))
    val mode = WireDefault(0.U(2.W))
    val value = Cat( base, mode )
    val (enable, dnxt) = Reg_Exe_Port( value, "h305".U, exe_port )
    when(enable) { base := dnxt(63,2) }
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
  mcounteren := {
    val hpm = RegInit(0.U(32.W))
    val ir  = RegInit(0.U(1.W))
    val tm  = RegInit(0.U(1.W))
    val cy  = RegInit(0.U(1.W))
    val value = Cat( hpm(31,3), ir, tm, cy )
    val (enable, dnxt) = Reg_Exe_Port( value, "h306".U, exe_port )
    when(enable) { hpm := dnxt; ir := dnxt(2); tm := dnxt(1); cy := dnxt(0) }
    value 
  }



  //Machine Trap Handling
  /**
    * Machine Scratch Register -- mscratch
    *
    * it's used to hold a pointer to a M-mode hart-local context space and swapped with a user register upon entry to an M-mode trap handler
    */
  mscratch := {
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
  mepc := {
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

  mcause := {
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
        is_instr_misAlign        -> 0.U,
        is_instr_access_fault    -> 1.U,
        is_instr_illeage         -> 2.U,
        is_breakPoint            -> 3.U,
        is_load_misAlign         -> 4.U,
        is_load_access_fault     -> 5.U,
        is_storeAMO_misAlign     -> 6.U,
        is_storeAMO_access_fault -> 7.U,
        is_ecall_U               -> 8.U,
        is_ecall_S               -> 9.U,
        is_ecall_M               -> 11.U,
        is_instr_paging_fault    -> 12.U,
        is_load_paging_fault     -> 13.U,
        is_storeAMO_paging_fault -> 15.U,
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
  mtval := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h343".U, exe_port )
    when( priv_lvl_dnxt === "b11".U ) {
      value := Mux1H( Seq(
        is_instr_access_fault    -> ill_ivaddr,
        is_instr_paging_fault    -> ill_ivaddr,
        is_instr_illeage         -> ill_instr,
        is_breakPoint            -> 0.U,
        is_load_misAlign         -> ill_dvaddr,
        is_load_access_fault     -> ill_dvaddr,
        is_storeAMO_misAlign     -> ill_dvaddr,
        is_storeAMO_access_fault -> ill_dvaddr,
        is_load_paging_fault     -> ill_dvaddr,
        is_storeAMO_paging_fault -> ill_dvaddr    
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
  mip := {
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

  mtinst := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h34A".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  mtval2 := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h34B".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }


  //Machine Memory Protection
  for ( i <- 0 until 16 ) yield {
    pmpcfg(i) := {
      val value = RegInit(0.U(64.W))
      val (enable, dnxt) = Reg_Exe_Port( value, "h3A0".U + i.U, exe_port )
      when(enable) { value := dnxt }
      value 
    }

  }


   
    for ( i <- 0 until 64 ) yield {
      pmpaddr(i) := {
        val value = RegInit(0.U(64.W))
        val (enable, dnxt) = Reg_Exe_Port( value, "h3B0".U + i.U, exe_port )
        when(enable) { value := dnxt }
        value 
      }
    }


  //Machine Counter/Timer

  //0xb00
  /**
    * Hardware Performance Monitor -- mcycle
    *
    * @return the number of clock cycles executed by the processor core
    */
  mcycle := {
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
  minstret := {
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

  for ( i <- 0 until 32 ) yield {
    mhpmcounter(i) := {
      if ( i == 0 || i == 1 || i == 2 ) { 0.U }
      else {
        val value = RegInit(0.U(64.W))
        val (enable, dnxt) = Reg_Exe_Port( value, "hB00".U + i.U, exe_port )
        when(enable) { value := dnxt }
        value 
      }
    }
  }



  //Machine Counter Setup
  /**
    * 
    * when set, the counter will not increase, all hard-wire to 0 in this version
    * 
    */
  mcountinhibit := {
    val value = WireDefault(0.U(64.W))
    // val (enable, dnxt) = Reg_Exe_Port( value, "h320".U, exe_port )
    // when(enable) { value := dnxt }
    value 
  }

  
  for ( i <- 0 until 32 ) yield {
    mhpmevent(i) := {
      if ( i == 0 || i == 1 || i == 2 ) { 0.U }
      else {
        val value = RegInit(0.U(64.W))
        val (enable, dnxt) = Reg_Exe_Port( value, "hB20".U + i.U, exe_port )
        when(enable) { value := dnxt }
        value 
      }
    }
  }


}
