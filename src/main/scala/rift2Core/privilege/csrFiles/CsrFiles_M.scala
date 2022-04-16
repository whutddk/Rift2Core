/*
  Copyright (c) 2020 - 2022 Wuhan University of Technology <295054118@whut.edu.cn>

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




trait CsrFiles_M { this: BaseCommit =>


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

  val is_interrupt = Wire(Bool())
  val is_nomask_interrupt = Wire(Bool())
  val is_m_interrupt = Wire(Bool())
  val is_s_interrupt = Wire(Bool())

  is_m_interrupt := is_msi | is_mti | is_mei
  is_s_interrupt := is_ssi | is_sti | is_sei
  is_interrupt := ((is_m_interrupt | is_s_interrupt) & ~is_step_int_block) | is_nomask_interrupt





  def update_fcsr( in: CMMState_Bundle): FCSRBundle = {
    val fcsr = WireDefault( in.csrfiles.fcsr )

    val (enable0, dnxt0) = Reg_Exe_Port( in.csrfiles.fcsr.fflags, "h001".U, in.exe_fport )
    val (enable1, dnxt1) = Reg_Exe_Port( in.csrfiles.fcsr.frm,    "h002".U, in.exe_fport )
    val (enable2, dnxt2) = Reg_Exe_Port( in.csrfiles.fcsr.asUInt, "h003".U, in.exe_fport )  
        

    when(enable0) { fcsr.fflags := dnxt0 }
    .elsewhen(enable1) { fcsr.frm := dnxt1 }
    .elsewhen(enable2) { fcsr.fflags := dnxt2(4,0); fcsr.frm := dnxt2(7,5) }   

    return fcsr
  }


  //user conter timers
  /**
    * Hardware Performance Monitor -- cycle (read-only)
    * @return a count of the number of clock cycles executed by the ***processor core*** on which the hart is running from an arbitrary start time in the past
    *
    */
  def update_cycle( in: CMMState_Bundle): UInt = {
    val cycle = WireDefault( in.csrfiles.cycle )

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.cycle, "hC00".U, in.exe_port )
    when( enable ) { cycle := dnxt }
    .otherwise { cycle := in.csrfiles.cycle + 1.U }

    return cycle
  }



  /**
    * Hardware Performance Monitor -- time (read-only)
    * @return a count of the number of ***rtc*** cycles executed by the ***processor core*** on which the hart is running from an arbitrary start time in the past
    *
    */
  def update_time( in: CMMState_Bundle ): UInt = {
    val time = WireDefault( in.csrfiles.time )
    val rtc = ShiftRegisters( in.rtc_clock, 3, false.B, true.B )
    when(rtc(3) ^ rtc(2)) { time := in.csrfiles.time + 1.U }
    return time
  }
      
  /**
    * Hardware Performance Monitor -- instret (read-only)
    * 
    * @return the number of instructions the hart has retired
    */
  def update_instret( in: CMMState_Bundle ): UInt = {
    val instret = in.csrfiles.instret + in.retired_cnt
    return instret
  }
  
  def update_hpmcounter( in: CMMState_Bundle ): Vec[UInt] = {
    val hpmcounter = WireDefault( VecInit(in.csrfiles.hpmcounter)
    ( 0 until 32 ).map{ i => 
      if ( i == 0 || i == 1 || i == 2 ) {}
      else {
        val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.hpmcounter(i), "hC00".U + i.U, in.exe_port )
        when(enable) { hpmcounter := dnxt }
      }
    }
    return hpmcounter
  }



  //machine information register
  def update_mvendorid( in: CMMState_Bundle ) = 0.U
  def update_marchid  ( in: CMMState_Bundle ) = 0.U
  def update_mimpid   ( in: CMMState_Bundle ) = 0.U
  def update_mhartid  ( in: CMMState_Bundle ) = 0.U



  //Machine Trap Setup

  /**
    * Machine Status Registers
    * @param SD (63) whether either fs or xs is dirty
    * @param MBE (37) endian of M-mode, when 0, is little-endian, hard-wire to 0.U(64.bits)
    * @param SBE (36) endian of S-mode, when 0, is little-endian, hard-wire to 0.U(64.bits)
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
    * @param UBE (6) endian of U-mode, when 0, is little-endian, hard-wire to 0.U(64.bits)
    * @param SPIE (5) When SRet, SPIE set to 1
    * @param MIE (3) M-mode Interrupt Enable; When MRet, update to MPIE
    * @param SIE (1) S-mode Interrupt Enable; When SRet, update to SPIE
    */

  // val sd  = ((xs === 3.U) || (fs === 3.U)).asUInt
  // val mbe  = 0.U(1.W)
  // val sbe  = 0.U(1.W)
  // val sxl  = 2.U(2.W)
  // val uxl  = 2.U(2.W)
  // val (tsr, tsr_dnxt)   = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (tw, tw_dnxt)     = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (tvm, tvm_dnxt)   = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (mxr, mxr_dnxt)   = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (sum, sum_dnxt)   = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (mprv, mprv_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val xs   = 0.U(2.W)
  // val (fs, fs_dnxt)     = SuperscalarReg( init = 0.U(2.W), is_reitred = is_retired_v )
  // val (mpp, mpp_dnxt)   = SuperscalarReg( init = "b11".U(2.W), is_reitred = is_retired_v )
  // val (spp, spp_dnxt)   = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (mpie, mpie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val ube  = 0.U(1.W)
  // val (spie, spie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (mie, mie_dnxt)   = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (sie, sie_dnxt)   = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )


  // val mstatus = Cat( sd, 0.U(25.W), mbe, sbe, sxl, uxl, 0.U(9.W), tsr, tw, tvm, mxr, sum, mprv, xs, fs, mpp, 0.U(2.W), spp, mpie, ube, spie, 0.U(1.W), mie, 0.U(1.W), sie, 0.U(1.W) )
    
  // ( 0 until cm ).map{ i => {
  //   val value = Wire(UInt(32.W))
  //   if ( i == 0 ) { value := mstatus }
  //   else { value := Cat( sd, 0.U(25.W), mbe, sbe, sxl, uxl, 0.U(9.W), tsr_dnxt(i-1), tw_dnxt(i-1), tvm_dnxt(i-1), mxr_dnxt(i-1), sum_dnxt(i-1), mprv_dnxt(i-1), xs, fs_dnxt(i-1), mpp_dnxt(i-1), 0.U(2.W), spp_dnxt(i-1), mpie_dnxt(i-1), ube, spie_dnxt(i-1), 0.U(1.W), mie_dnxt(i-1), 0.U(1.W), sie_dnxt(i-1), 0.U(1.W) ) }
      
  //   val (enable0, dnxt0) = Reg_Exe_Port( value, "h100".U, exe_port(i) )
  //   val (enable1, dnxt1) = Reg_Exe_Port( value, "h300".U, exe_port(i) )

  //   when( is_trap_v(i) ) {
  //     when( priv_lvl_dnxt(i) === "b11".U ) {
  //       mpie_dnxt(i) := {if ( i == 0 ) mie else mie_dnxt(i-1)}
  //       mie_dnxt(i)  := 0.U
  //       mpp_dnxt(i)  := {if ( i == 0 ) priv_lvl else priv_lvl_dnxt(i-1)}
  //     }
  //     when( priv_lvl_dnxt(i) === "b01".U ) {
  //       spp_dnxt(i)  := Mux( (if( i == 0 ) priv_lvl else priv_lvl_dnxt(i-1) === "b00".U), 0.U, 1.U )
  //       spie_dnxt(i) := {if ( i == 0 ) sie else sie_dnxt(i-1)}
  //       sie_dnxt(i)  := 0.U
  //     }
  //   }
  //   .elsewhen( is_mRet_v(i) ) {
  //     mie_dnxt(i)  := {if ( i == 0 ) mpie else mpie_dnxt(i-1)}
  //     mpie_dnxt(i) := 1.U
  //     mpp_dnxt(i)  := "b00".U

  //     mprv_dnxt(i) := Mux( priv_lvl_dnxt(i) =/= "b11".U, 0.U, mprv_dnxt(i-1) )
  //   }
  //   .elsewhen( is_sRet_v(i) ) {
  //     spie_dnxt(i) := 1.U
  //     sie_dnxt(i)  := spie_dnxt(i-1)

  //     mprv_dnxt(i) := Mux( priv_lvl_dnxt(i) =/= "b11".U, 0.U, mprv_dnxt(i-1) )
  //   }
  //   .elsewhen(enable0) {
  //     // sd   := dnxt0(63)
  //     mxr_dnxt(i)  := dnxt0(19)
  //     sum_dnxt(i)  := dnxt0(18)
  //     // xs   := dnxt0(16,15)
  //     fs_dnxt(i)   := dnxt0(14,13)
  //     spp_dnxt(i)  := dnxt0(8)
  //     // ube  := dnxt0(6)
  //     spie_dnxt(i) := dnxt0(5)
  //     sie_dnxt(i)  := dnxt0(1)
  //   }
  //   .elsewhen(enable1) {
  //     // sd   := dnxt1(63)
  //     tsr_dnxt(i)  := dnxt1(22)
  //     tw_dnxt(i)   := dnxt1(21)
  //     tvm_dnxt(i)  := dnxt1(20)
  //     mxr_dnxt(i)  := dnxt1(19)
  //     sum_dnxt(i)  := dnxt1(18)
  //     mprv_dnxt(i) := dnxt1(17)
  //     // xs   := dnxt1(16,15)
  //     fs_dnxt(i)   := dnxt1(14,13)
  //     mpp_dnxt(i)  := dnxt1(12,11)
  //     spp_dnxt(i)  := dnxt1(8)
  //     mpie_dnxt(i) := dnxt1(7)
  //     spie_dnxt(i) := dnxt1(5)
  //     mie_dnxt(i)  := dnxt1(3)
  //     sie_dnxt(i)  := dnxt1(1)
  //   }

  //   when(is_fpu_state_change(i) & ( (if ( i == 0 ) fs else fs_dnxt(i-1)) =/= 0.U)) {
  //     fs_dnxt(i) := 3.U
  //   }
   
  //   }
  // }

  def update_mstatus( in: CMMState_Bundle ): MStatusBundle = {
    val mstatus = WireDefault( in.csrfiles.mstatus )

    mstatus.sd  := ((mstatus.xs === 3.U) || (mstatus.fs === 3.U)).asUInt
    mstatus.mbe := 0.U(1.W)
    mstatus.sbe := 0.U(1.W)
    mstatus.sxl := 2.U(2.W)
    mstatus.uxl := 2.U(2.W)
    mstatus.xs   = 0.U(2.W)
    mstatus.ube  = 0.U(1.W)
      
    val (enable0, dnxt0) = Reg_Exe_Port( in.csrfiles.mstatus, "h100".U, exe_port )
    val (enable1, dnxt1) = Reg_Exe_Port( in.csrfiles.mstatus, "h300".U, exe_port )

    when( in.is_trap ) {
      when( in.priv_lvl_dnxt === "b11".U ) {
        mstatus.mpie := in.mstatus.mie
        mstatus.mie  := 0.U
        mstatus.mpp  := in.priv_lvl_qout
      }
      when( in.priv_lvl_dnxt === "b01".U ) {
        mstatus.spp  := Mux( (in.priv_lvl_qout === "b00".U), 0.U, 1.U )
        mstatus.spie := in.csrfiles.sie
        mstatus.sie  := 0.U
      }
    }
    .elsewhen( in.is_mRet ) {
      mstatus.mie  := in.csrfiles.mpie
      mstatus.mpie := 1.U
      mstatus.mpp  := "b00".U

      mstatus.mprv := Mux( in.priv_lvl_dnxt =/= "b11".U, 0.U, in.csrfiles.mstatus.mprv )
    }
    .elsewhen( in.is_sRet ) {
      mstatus.spie := 1.U
      mstatus.sie  := in.csrfiles.mstatus.spie

      mstatus.mprv := Mux( in.priv_lvl_dnxt(i) =/= "b11".U, 0.U, in.csrfiles.mstatus.mprv )
    }
    .elsewhen(enable0) {

      mstatus.mxr  := dnxt0(19)
      mstatus.sum  := dnxt0(18)
      mstatus.fs   := dnxt0(14,13)
      mstatus.spp  := dnxt0(8)
      mstatus.spie := dnxt0(5)
      mstatus.sie  := dnxt0(1)
    }
    .elsewhen(enable1) {
      mstatus.tsr  := dnxt1(22)
      mstatus.tw   := dnxt1(21)
      mstatus.tvm  := dnxt1(20)
      mstatus.mxr  := dnxt1(19)
      mstatus.sum  := dnxt1(18)
      mstatus.mprv := dnxt1(17)
      mstatus.fs   := dnxt1(14,13)
      mstatus.mpp  := dnxt1(12,11)
      mstatus.spp  := dnxt1(8)
      mstatus.mpie := dnxt1(7)
      mstatus.spie := dnxt1(5)
      mstatus.mie  := dnxt1(3)
      mstatus.sie  := dnxt1(1)
    }

    when(in.is_fpu_state_change & in.csrfiles.mstatus.fs =/= 0.U)) {
      mstatus.fs := 3.U
    }
    return mstatus
  }


  /**
    * Machine ISA register 
    * @param MXL (63,62) is 2.U for XLEN of RiftCore is 64 
    * @param Extensions (25:0) 
    * @note U(20): User mode implement S(18): Supervisor mode implemented N(13): User-level interrupts supported
    * @note M(12): Integer Multiply/Divide extension I(8): RV64I base ISA C(2): Compressed extension
    * 
    */
  def update_misa( in: CMMState_Bundle ) = {
    val mxl = WireDefault(2.U(2.W))
    val extensions = {
      if (true) { //fpu
        WireDefault("b00000101000001000100101101".U(26.W))  
      } else if (true) { //none fpu, has S-mode U-mode
        WireDefault("b00000101000001000100000101".U(26.W))
      } else {
        WireDefault("b00000000000001000100000101".U(26.W))
      }
    
    }
    Cat(mxl, 0.U(36.W), extensions)
  }
  
  
  

  
  /**
    * Machine Trap Delegation Register
    * 
    * By default, the exception will be handled in M-mode, when the bits set, it's handled in S-mode
    */
  def update_medeleg( in: CMMState_Bundle ): UInt = {
    val medeleg = WireDefault( in.csrfiles.medeleg )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.medeleg, "h302".U, exe_port )
    when(enable) { medeleg := dnxt }
    return medeleg
  } 
  
  /**
    * Machine Trap Delegation Register
    * 
    * By default, the interrupt will be handled in M-mode, when the bits set, it's handled in S-mode
    */
  def update_mideleg( in: CMMState_Bundle ): UInt = {
    val mideleg = WireDefault( in.csrfiles.mideleg )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mideleg, "h303".U, exe_port )
    when(enable) { mideleg := dnxt }
    return mideleg
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
  def update_mie( in: CMMState_Bundle ): MSIntBundle = {
    val mie = WireDefault( in.csrfiles.mie )

    val (enable0, dnxt0) = Reg_Exe_Port( value, "h304".U, exe_port )
    val (enable1, dnxt1) = Reg_Exe_Port( value, "h104".U, exe_port )

    when(enable0) {
      mie.meie := dnxt0(11)
      mie.seie := dnxt0(9)
      mie.mtie := dnxt0(7)
      mie.stie := dnxt0(5)
      mie.msie := dnxt0(3)
      mie.ssie := dnxt0(1)
    }
    .elsewhen(enable1) {
      mie.seie := dnxt1(9)
      mie.stie := dnxt1(5)
      mie.ssie := dnxt1(1)
    }      

  }



  /**
    * Machine Trap-Vector Base-Address Register
    * holds trap vector configuration, consisting of a vector of a vector base address and a bector mode 
    * @param base (63,2)
    * @param mode (1,0) read only in this version hardwire to 0.U
    */

  def update_mtvec( in: CMMState_Bundle ): = TVecBundle {
    val mtvec = WireDefault( in.csrFiles.mtvec )

    mtvec.mode := 0.U
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mtvec, "h305".U, exe_port(i) )
    when(enable) { mtvec.base := dnxt(63,2) }    

    return mtvec
  }




  /**
    * Machine Counter-Enable Register -- mcounteren
    *
    * @param HPM (31,3) Whether is allowed to access hpmcounter(x) in S-mode
    * @param IR (2) Whether is allowed to access instret in S-mode
    * @param TM (1) Whether is allowed to access time in S-mode
    * @param CY (0) Whether is allowed to access cycle in S-mode
    */

  def update_mcounteren( in: CMMState_Bundle ): CounterenBundle = {
    val mcounteren = WireDefault( in.csrfiles.mcounteren )

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mcounteren, "h306".U, exe_port )
    when(enable) { mcounteren.mhpm := dnxt }
    return mcounteren
  }


  //Machine Trap Handling
  /**
    * Machine Scratch Register -- mscratch
    *
    * it's used to hold a pointer to a M-mode hart-local context space and swapped with a user register upon entry to an M-mode trap handler
    */
  def update_mscratch( in: CMMState_Bundle ): UInt = {
    val mscratch = WireDefault( in.csrfiles.mscratch )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mscratch, "h340".U, exe_port )
    when(enable) { mscratch := dnxt }
    return mscratch
  }


  /**
    * Machine Exception Program Counter
    * @note hold all valid virtual addresses 
    * when a ***trap*** is taken into ***M-mode***, update to the ***virtual address*** that was interrupted or encountered the exception 
    */
  def update_mepc( in: CMMState_Bundle ): UInt = {
    val mepc = WireDefault( in.csrfiles.mepc )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mepc, "h341".U, exe_port(i) )

    when( in.is_trap & in.priv_lvl_dnxt === "b11".U){ mepc := in.commit_pc }
    .elsewhen(enable) { mepc := dnxt }
    return mepc
  }

  /**
    * Machine Cause Register
    * 
    * when a ***trap*** is taken into ***M-mode***, Indicating the event that caused the trap
    * @param interrupt
    * @param exception_code
    */
  def update_mcause( in: CMMState_Bundle ): CauseBundle = {
    val mcause = WireDefault( in.csrfiles.mcause )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mcause, "h342".U, exe_port(i) )

    when( in.is_m_interrupt & in.priv_lvl_dnxt(i) === "b11".U ) {
      mcause.interrupt := 1.U
      mcause.exception_code := Mux1H( Seq(
        // is_ssi -> 1.U,
        in.is_msi(i) -> 3.U,
        // is_sti -> 5.U,
        in.is_mti(i) -> 7.U,
        // is_sei -> 9.U,
        in.is_mei(i) -> 11.U
      ))
    }
    .elsewhen( in.is_exception & in.priv_lvl_dnxt === "b11".U ) {
      mcause.interrupt := 0.U
      mcause.exception_code := Mux1H( Seq(
        in.is_instr_misAlign        -> 0.U,
        in.is_instr_access_fault    -> 1.U,
        in.is_instr_illeage         -> 2.U,
        in.is_breakPoint            -> 3.U,
        in.is_load_misAlign         -> 4.U,
        in.is_load_access_fault     -> 5.U,
        in.is_storeAMO_misAlign     -> 6.U,
        in.is_storeAMO_access_fault -> 7.U,
        in.is_ecall_U               -> 8.U,
        in.is_ecall_S               -> 9.U,
        in.is_ecall_M               -> 11.U,
        in.is_instr_paging_fault    -> 12.U,
        in.is_load_paging_fault     -> 13.U,
        in.is_storeAMO_paging_fault -> 15.U,
      ))
    }
    .elsewhen(enable) {
      mcause.interrupt      := dnxt(63)
      mcause.exception_code := dnxt(62,0)
    }
    return mcause
  }

  

  /**
    * Machine Trap Value Register
    * 
    * When a trap is taken into ***M-mode***, update to ***virtual address*** or ***faulting instruction***
    */
  def update_mtval( in: CMMState_Bundle ): UInt = {
    val mtval = WireDefault( in.csrfiles.mtval )

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mtval, "h343".U, exe_port )

    when( in.priv_lvl_dnxt(i) === "b11".U ) {
      mtval := Mux1H( Seq(
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
    .elsewhen(enable) { mtval := dnxt }
    return mtval
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

  def update_mip( in: CMMState_Bundle ): MSIntBundle = {
    val mip = WireDefault( in.csrfiles.mip )
    
    mip.meip := clint_ex_m
    mip.seip := clint_ex_s
    mip.mtip := clint_tm_m
    mip.stip := clint_tm_s
    mip.msip := clint_sw_m
    mip.ssip := clint_sw_s

    return mip
  }


  def update_mtinst( in: CMMState_Bundle ): UInt = {
    val mtinst = WireDefault( in.csrfiles.mtinst )

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mtinst, "h34A".U, exe_port )
    when(enable) { mtinst := dnxt }    

    return mtinst
  }

  def update_mtval2( in: CMMState_Bundle ): UInt = {
    val mtval2 = WireDefault( in.csrfiles.mtval2 )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mtval2, "h34B".U, exe_port )
    when(enable) { mtval2 := dnxt }
    return mtval2
  }


  //Machine Memory Protection
  def update_pmpcfg( in: CMMState_Bundle ): Vec[Vec[PmpcfgBundle]] = {
    val pmpcfg = WireDefault( in.csrfiles.pmpcfg )

    for ( i <- 0 until 16 ) yield {
      for ( j <- 0 until 8 ) yield {
        if ( i % 2 == 0 ) {
          val (enable, dnxt) = Reg_Exe_Port( Cat(in.csrfiles.pmpcfg(i).map{_.asUInt}.revert), "h3A0".U + i.U, exe_port )
          
          when( enable & pmpcfg(i)(j).L ) {
              pmpcfg(i)(j).L := dnxt(8*j+7)
              pmpcfg(i)(j).A := dnxt(8*j+4,8*j+3)
              pmpcfg(i)(j).X := dnxt(8*j+2)
              pmpcfg(i)(j).W := dnxt(8*j+1)
              pmpcfg(i)(j).R := dnxt(8*j+0)
          }
        } else {
          pmpcfg(i)(j) := 0.U.asTypeOf(new PmpcfgBundle)
        }        
      }
    }
    return pmpcfg
  }

  // val (pmpcfg_L, pmpcfg_L_dnxt) =  for ( i <- 0 until 64 ) yield { SuperscalarReg( init = 0.U(1.W),  is_reitred = is_retired_v )}
  // val (pmpcfg_A, pmpcfg_A_dnxt) =  for ( i <- 0 until 64 ) yield { SuperscalarReg( init = 0.U(2.W),  is_reitred = is_retired_v )}
  // val (pmpcfg_X, pmpcfg_X_dnxt) =  for ( i <- 0 until 64 ) yield { SuperscalarReg( init = 0.U(1.W),  is_reitred = is_retired_v )}
  // val (pmpcfg_W, pmpcfg_W_dnxt) =  for ( i <- 0 until 64 ) yield { SuperscalarReg( init = 0.U(1.W),  is_reitred = is_retired_v )}
  // val (pmpcfg_R, pmpcfg_R_dnxt) =  for ( i <- 0 until 64 ) yield { SuperscalarReg( init = 0.U(1.W),  is_reitred = is_retired_v )}
  
  // val pmpcfg_buf = for ( i <- 0 until 64 ) yield 
  //   Cat(  pmpcfg_L(i), 0.U(2.W), pmpcfg_A(i), pmpcfg_X(i), pmpcfg_W(i), pmpcfg_R(i) )

  // val pmpcfg_buf_dnxt = for ( i <- 0 until 64 ) yield {
  //   ( 0 until cm ).map { t =>
  //     Cat(  pmpcfg_L_dnxt(i)(t), 0.U(2.W), pmpcfg_A_dnxt(i)(t), pmpcfg_X_dnxt(i)(t), pmpcfg_W_dnxt(i)(t), pmpcfg_R_dnxt(i)(t) )
  //   }
  // }

  // val pmpcfg = for ( i <- 0 until 16 ) yield {
  //   Cat( for ( j <- 7 to 0 ) yield { pmpcfg_buf(8*i+j) }
  // }

  // val pmpcfg_dnxt = 
  // for ( i <- 0 until 16 ) yield {
  //   ( 0 until cm ).map{ t => {
  //     Cat( for ( j <- 7 to 0 ) yield { pmpcfg_buf_dnxt(8*i+j) }
  //   }} 
  // }

  // for ( i <- 0 until 16 ) yield {
  //   ( 0 until cm ).map{ t => {
      
  //     val value =
  //       if ( t == 0 ) pmpcfg
  //       else pmpcfg_dnxt
      
  //     val (enable, dnxt) = Reg_Exe_Port( value, "h3A0".U + i.U, exe_port(t) )
  //       for( j <- 0 unitl 8 ) yield {
  //         when(enable & 
  //               {if ( t == 0 ) ~pmpcfg_L(i*8+j) else ~pmpcfg_L_dnxt(i*8+j)(t-1) }
  //         ) {

  //             pmpcfg_L_dnxt(i*8+j)(t) := dnxt(t)(8*j+7)
  //             pmpcfg_A_dnxt(i*8+j)(t) := dnxt(t)(8*j+4,8*j+3)
  //             pmpcfg_X_dnxt(i*8+j)(t) := dnxt(t)(8*j+2)
  //             pmpcfg_W_dnxt(i*8+j)(t) := dnxt(t)(8*j+1)
  //             pmpcfg_R_dnxt(i*8+j)(t) := dnxt(t)(8*j+0)

  //         }              
  //       }

  //     }
  //   }
  // }

  // for ( i <- 0 until 16 ) yield {  
  //   pmpcfg(i) := {
  //     val buf = RegInit( VecInit(Seq.fill(8)(0.U(8.W))))
  //     val value = Cat( for ( i <- 0 until 8 ) yield { buf(7-i) } )
  //     val lock = VecInit(
  //       for( j <- 0 until 8 ) yield { buf(j)(7).asBool }
  //     )
  //     val (enable, dnxt) = Reg_Exe_Port( value, "h3A0".U + i.U, exe_port )
  //     for ( j <- 0 until 8 ) yield {
  //       when( enable & ~lock(j) ) {
  //         buf(j) := dnxt(8*j+7, 8*j)

  //         }
  //     }
  //     value 
  //   }

  // }

  def update_pmpaddr( in: CMMState_Bundle ): Vec[UInt] = {
    val pmpaddr = WireDefault(in.csrfiles.pmpaddr)

    for( i <- 0 until 64 ) yield {

      val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.pmpaddr(i), "h3B0".U + i.U, exe_port )
      // val cfg_idx = i/8*2
      // val bit_idx = 8*(i%8) + 7
      val lock = in.csrfiles.pmpcfg(i/8*2)(i%8).L
      when(enable & lock =/= 1.U ) { pmpaddr(i) := dnxt }    
    }

    return pmpaddr
  }


    // for ( i <- 0 until 64 ) yield {
    //   pmpaddr(i) := {
    //     val value = RegInit(0.U(38.W)) // for dromajo diff-test, support sv39 only
    //     val cfg_idx = i/8*2
    //     val bit_idx = 8*(i%8) + 7
    //     val lock = pmpcfg(cfg_idx)(bit_idx)
    //     val (enable, dnxt) = Reg_Exe_Port( value, "h3B0".U + i.U, exe_port )
    //     when(enable) {
    //       when( lock =/= 1.U ) {
    //         value := dnxt
    //       }

    //     }
    //     value 
    //   }
    // }


  //Machine Counter/Timer

  //0xb00
  /**
    * Hardware Performance Monitor -- mcycle
    *
    * @return the number of clock cycles executed by the processor core
    */
  def update_mcycle( in: CMMState_Bundle ): UInt = {
    val mcycle = WireDefault(in.csrfiles.mcycle)

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mcycle, "hB00".U, exe_port )
      when(enable) { mcycle := dnxt }
      .otherwise { mcycle := in.csrfiles.mcycle + 1.U }
    return mcycle
  }


  /**
    * Hardware Performance Monitor -- minstret
    *
    * @return the number of instructions the hart has retired
    */
  def update_minstret( in: CMMState_Bundle ): UInt = {
    val minstret = WireDefault( in.csrfiles.minstret )

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.minstret, "hB02".U, exe_port )
    when(enable) { minstret := dnxt }
    .otherwise { minstret := in.csrfiles.minstret + in.retired_cnt }

    return minstret  
  }



  /**
    * Hardware Performance Monitor -- mhpmcounter 3~31
    *
    * @return 
    */
  def update_mhpmcounter( in: CMMState_Bundle ): Vec[UInt] = {
    val mhpmcounter = WireDefault( in.csrfiles.mhpmcounter )
    for ( i <- 0 until 32 ) yield {
      val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mhpmcounter(i), "hB00".U + i.U, exe_port )
      when(enable) { mhpmcounter(i) := dnxt }
    }    
  }
    return mhpmcounter
  }


  //Machine Counter Setup
  /**
    * 
    * when set, the counter will not increase, all hard-wire to 0 in this version
    * 
    */
  def update_mcountinhibit( in: CMMState_Bundle ) = 0.U(64.W)


  def update_mhpmevent( in: CMMState_Bundle ): Vec[UInt]= {
    val mhpmevent = WireDefault(in.csrfiles.mhpmevent)

    for ( i <- 0 until 32 ) yield {
      if ( i == 0 || i == 1 || i == 2 ) { mhpmevent(i) := 0.U }
      else {
        val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mhpmevent(i), "hB20".U + i.U, exe_port )
        when(enable) { mhpmevent(i) := dnxt }
      }
    }

    return mhpmevent
  }


}
