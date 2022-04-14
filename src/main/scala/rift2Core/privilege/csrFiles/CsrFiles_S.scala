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
import base._

trait CsrFiles_S { this: BaseXSSoc =>



  //supervisor trap setup
  
  /**
    * Supervisor Status Register -- sstatus
    * 
    * @param SD (63) whether either fs or xs is dirty
    * @param UXL (33,32) XLEN of U-mode, hard-wire to 2.U(64.bits)
    * @param MXR (19) Make executable Readable. When 0, only loads form pages marked readable
    * @param SUM (18) permit Supervisor User Memory access. When 0, S-mode accesses to page.U === 1 will fault.
    * @param XS (16,15) aditional user-mode extension and associated state
    * @param FS (14,13) float point statuw=s
    * @param SPP (8) Previous Mode is U-mode or S-mode? When SRet, privilege mode update to SPP, SPP set to "U"
    * @param UBE (6) endian of U-mode, when 0, is little-endian
    * @param SPIE (5) When SRet, SPIE set to 1
    * @param SIE (1) S-mode Interrupt Enable; When SRet, update to SPIE
    * 
    */  
  val sstatus = {
    mstatus & Cat( "b1".U, 0.U(29.W), "b11".U, 0.U(12.W), "b11011110000101100010".U )
  }



/**
  * Supervisor Interrupt Register -- sie (enable)
  * @note read-only, meie(11), mtie(7), msie(3) is visible and maskable when mideleg(x) set
  * 
  */
  val (meie, meie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  val (seie, seie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  val (mtie, mtie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  val (stie, stie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  val (msie, msie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  val (ssie, ssie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )

  val sie =
      Cat(
        0.U(4.W), meie & mideleg(11), 0.U(1.W), seie,
        0.U(1.W), mtie & mideleg(7),  0.U(1.W), stie,
        0.U(1.W), msie & mideleg(3),  0.U(1.W), ssie, 0.U(1.W) )

  val sie_dnxt = ( 0 until cm ).map{ t =>
    Cat(
      0.U(4.W), meie_dnxt(t) & mideleg_dnxt(t)(11), 0.U(1.W), seie_dnxt(t),
      0.U(1.W), mtie_dnxt(t) & mideleg_dnxt(t)(7),  0.U(1.W), stie_dnxt(t),
      0.U(1.W), msie_dnxt(t) & mideleg_dnxt(t)(3),  0.U(1.W), ssie_dnxt(t), 0.U(1.W) )    
  }

  ( 0 until cm ).map{ t => {
    val value = if ( t == 0 ) sie else sie_dnxt(t-1)
    val (enable0, dnxt0) = Reg_Exe_Port( value, "h104".U, exe_port(t) )
    val (enable1, dnxt1) = Reg_Exe_Port( value, "h304".U, exe_port(t) )

    when(enable0) {
      meie_dnxt(t) := dnxt0(11)
      seie_dnxt(t) := dnxt0(9)
      mtie_dnxt(t) := dnxt0(7)
      stie_dnxt(t) := dnxt0(5)
      msie_dnxt(t) := dnxt0(3)
      ssie_dnxt(t) := dnxt0(1)

    }
    .elsewhen(enable1) {
      seie_dnxt(t) := dnxt1(9)
      stie_dnxt(t) := dnxt1(5)
      ssie_dnxt(t) := dnxt1(1)
    }
  }}


  /**
    * Supervisor Trap Vector Base Address Register --stvec
    *
    * @note holdstrap vector configuration
    * @param base (63,2) vector base address, either va or pa
    * @param mode (1,0) vector mode,hard-wire to 0 in this version
    */
  val stvec_mode = 0.U(2.W)
  val (stvec_base, stvec_base_dnxt) = SuperscalarReg( init = 0.U(62.W), is_reitred = is_retired_v )

  val stvec = Cat(stvec_base, stvec_mode)
  val stvec_dnxt = ( 0 until cm ).map{ t => Cat(stvec_base_dnxt(t), stvec_mode) }

  ( 0 until cm ).map{ t => {
    val value = if ( t == 0 ) stvec else stvec_dnxt(t-1)
    val (enable, dnxt) = Reg_Exe_Port( value, "h105".U, exe_port(t) )
    when(enable) { stvec_base_dnxt(t) := dnxt(63,2) }
  }}

  /**
    * Supervisor Timers and Performance Counters -- Counter-Enable Register -- scounteren
    * 
    * @note controls the availability of the hardware performance monitoring counters to U-mode
    * @param HPM (31,3) Whether is allowed to access hpmcounter(x) in U-mode
    * @param IR (2) Whether is allowed to access instret in U-mode
    * @param TM (1) Whether is allowed to access time in U-mode
    * @param CY (0) Whether is allowed to access cycle in U-mode
    */
  val (shpm, shpm_dnxt) = SuperscalarReg( init = 0.U(32.W), is_reitred = is_retired_v )
  val (sir , sir_dnxt)  = SuperscalarReg( init = 0.U(1.W),  is_reitred = is_retired_v )
  val (stm , stm_dnxt)  = SuperscalarReg( init = 0.U(1.W),  is_reitred = is_retired_v )
  val (scy , scy_dnxt)  = SuperscalarReg( init = 0.U(1.W),  is_reitred = is_retired_v )

  val scounteren = Cat( shpm(31,3), sir, stm, scy )

  ( 0 until cm ).map{ t => {
    val value = if( t == 0 ) mcounteren else Cat( shpm_dnxt(i-1)(31,3), sir_dnxt(i-1), stm_dnxt(i-1), scy_dnxt(i-1) )
    val (enable, dnxt) = Reg_Exe_Port( value, "h306".U, exe_port(i) )
      when(enable) {
        shpm_dnxt(t) := dnxt(31,3)
        sir_dnxt(t)  := dnxt(2)
        stm_dnxt(t)  := dnxt(1)
        scy_dnxt(t)  := dnxt(0)
      }    
    }
  }


  //supervisor trap handling

  /**
    * Supervisor Scratch Register -- sscratch
    *
    * @note used to hold a pointer to the hart-local supervisor context while the hart is executing user code
    */
  val (sscratch, sscratch_dnxt) = SuperscalarReg( init = 0.U(64.W), is_reitred = is_retired_v )

  ( 0 until cm ).map{ t => {
    val value = if ( t == 0 ) sscratch else sscratch_dnxt(t-1)
    val (enable, dnxt) = Reg_Exe_Port( value, "h140".U, exe_port(t) )
    when(enable) { sscratch_dnxt(t) := dnxt }
  }}


  /**
    * Supervisor Exception Program Counter -- sepc
    * 
    * hold virtual addresses: when a trap is taken into S-mode, sepc is written with the virtual address of
    * the instruction that was interrupted or that encountered the exception
    */

  val (sepc, sepc_dnxt) = SuperscalarReg( init = 0.U(64.W), is_reitred = is_retired_v )

  ( 0 until cm ).map{ t => {
    val value = if ( t == 0 ) sepc else sepc_dnxt(t-1)
    val (enable, dnxt) = Reg_Exe_Port( value, "h141".U, exe_port(t) )

    when( is_trap_v(t) & priv_lvl_dnxt(t) === "b01".U ) {
      sepc_dnxt(t) := commit_pc(t)(63,1)
    }
    .elsewhen(enable) { sepc_dnxt(t) := dnxt(63,1) }
  }}

  /**
    * Supervisor Cause Register -- scause
    * 
    * when a trap is taken into S-mode, scause is written with a code indicating the event that cause the trap
    * @return
    */

  val (scause_int, scause_int_dnxt) = SuperscalarReg( init = 0.U(1.W),  is_reitred = is_retired_v )
  val (scause_exc, scause_int_dnxt) = SuperscalarReg( init = 0.U(63.W), is_reitred = is_retired_v )

  val scause = Cat(scause_int, scause_exc)
  val scause_dnxt = ( 0 until cm ).map{ t => Cat(scause_int_dnxt(t), scause_exc_dnxt(t)) }

  ( 0 until cm ).map{ t => {
    val value = if ( t == 0 ) scause else scause_dnxt(t-1)
    val (enable, dnxt) = Reg_Exe_Port( value, "h142".U, exe_port(t) )

    when( (is_m_interrupt_v(t) | is_s_interrupt_v(t) ) & priv_lvl_dnxt(t) === "b01".U ) {
      scause_int_dnxt(t) := 1.U
      scause_int_dnxt(t) := Mux1H( Seq(
        is_ssi(t) -> 1.U,
        is_msi(t) -> 3.U,
        is_sti(t) -> 5.U,
        is_mti(t) -> 7.U,
        is_sei(t) -> 9.U,
        is_mei(t) -> 11.U
      ))
    }
    .elsewhen( is_exception_v(t) & priv_lvl_dnxt(t) === "b01".U ) {
      scause_int_dnxt(t) := 0.U
      scause_int_dnxt(t) := Mux1H( Seq(
        is_instr_misAlign_v(t)        -> 0.U,
        is_instr_access_fault_v(t)    -> 1.U,
        is_instr_illeage_v(t)         -> 2.U,
        is_breakPoint_v(t)            -> 3.U,
        is_load_misAlign_v(t)         -> 4.U,
        is_load_access_fault_v(t)     -> 5.U,
        is_storeAMO_misAlign_v(t)     -> 6.U,
        is_storeAMO_access_fault_v(t) -> 7.U,
        is_ecall_U_v(t)               -> 8.U,
        is_ecall_S_v(t)               -> 9.U,
        is_ecall_M_v(t)               -> 11.U,
        is_instr_paging_fault_v(t)    -> 12.U,
        is_load_paging_fault_v(t)     -> 13.U,
        is_storeAMO_paging_fault_v(t) -> 15.U,
      ))
    }
    .elsewhen(enable) {
      scause_int_dnxt(t) := dnxt(63)
      scause_int_dnxt(t) := dnxt(62,0)
    }

  }}

  /**
    * Supervisor Trap Value Register -- stval
    * 
    * when a trap is taken into S-mode, stval is written with exception-specific information to assist softwave in handling the trap
    *
    * @return
    */
  val (stval, stval_dnxt) = SuperscalarReg( init = 0.U(64.W), is_reitred = is_retired_v )

  ( 0 until cm ).map{ t => {
    val value = if ( t == 0 ) stval else stval_dnxt(t-1)
    val (enable, dnxt) = Reg_Exe_Port( value, "h143".U, exe_port(t) )

    when( priv_lvl_dnxt(t) === "b01".U ) {
      stval_dnxt(t) := Mux1H( Seq(
        is_instr_access_fault_v(t)    -> ill_ivaddr(t),
        is_instr_paging_fault_v(t)    -> ill_ivaddr(t),
        is_instr_illeage_v(t)         -> ill_instr(t),
        is_breakPoint_v(t)            -> 0.U,
        is_load_misAlign_v(t)         -> ill_dvaddr(t),
        is_load_access_fault_v(t)     -> ill_dvaddr(t),
        is_storeAMO_misAlign_v(t)     -> ill_dvaddr(t),
        is_storeAMO_access_fault_v(t) -> ill_dvaddr(t),
        is_load_paging_fault_v(t)     -> ill_dvaddr(t),
        is_storeAMO_paging_fault_v(t) -> ill_dvaddr(t)       
      ))
    }
    .elsewhen(enable) { stval_dnxt(t) := dnxt }

  }}


/**
  * Supervisor Interrupt Register -- sip
  * @note read-only, meip(11), mtip(7), msip(3) is visible when mideleg(x) set
  * 
  */

    val meip = clint_ex_m
    val seip = clint_ex_s
    val mtip = clint_tm_m
    val stip = clint_tm_s
    val msip = clint_sw_m
    val ssip = clint_sw_s

  val sip =
    Cat(
      0.U(4.W), meip & mideleg(11), 0.U(1.W), seip,
      0.U(1.W), mtip & mideleg(7),  0.U(1.W), stip,
      0.U(1.W), msip & mideleg(3),  0.U(1.W), ssip, 0.U(1.W) )

  val sip_dnxt = ( 0 until cm ).map{ t => 
    Cat(
      0.U(4.W), meip & mideleg_dnxt(t)(11), 0.U(1.W), seip,
      0.U(1.W), mtip & mideleg_dnxt(t)(7),  0.U(1.W), stip,
      0.U(1.W), msip & mideleg_dnxt(t)(3),  0.U(1.W), ssip, 0.U(1.W) )
  }


  /**
    * Supervisor Address protection and translation Register -- satp
    *
    * @param mode (63,60) select the current address-translation scheme  
    * @param asid (59,44) address space identifier, which facilitates address-translation fences on a per-address-space basis
    * @param PPN (43,0) physical page number (ppn) of the root page table
    */

  // val (satp, satp_dnxt) = SuperscalarReg( init = 0.U(64.W), is_reitred = is_retired_v )

  // ( 0 until cm ).map{ t => {
  //   val value = if ( t == 0 ) sepc else sepc_dnxt(t-1)
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h141".U, exe_port(t) )

  //   when( is_trap_v(t) & priv_lvl_dnxt(t) === "b01".U ) {
  //     sepc_dnxt(t) := commit_pc(t)(63,1)
  //   }
  //   .elsewhen(enable) { sepc_dnxt(t) := dnxt(63,1) }
  // }}
  val (mode, mode_dnxt) = SuperscalarReg( init = 0.U(4.W), is_reitred = is_retired_v )
  val (asid, asid_dnxt) = SuperscalarReg( init = 0.U(16.W), is_reitred = is_retired_v )
  val (ppn , ppn_dnxt)  = SuperscalarReg( init = 0.U(44.W), is_reitred = is_retired_v )

  /** @note only sv39 supportted */
  val satp = Cat( mode & "b1000".U, asid, ppn ) 
  val satp_dnxt = ( 0 until cm ).map{ t => Cat( mode_dnxt(t) & "b1000".U, asid_dnxt(t), ppn_dnxt(t) ) }

  ( 0 until cm ).map{ t => {
    val value = if( t == 0 ) satp else satp_dnxt(t-1) 
    val (enable, dnxt) = Reg_Exe_Port( value, "h180".U, exe_port(t) )
    when(enable) { mode_dnxt(t) := dnxt(63,60); asid_dnxt(t) := dnxt(59,44); ppn_dnxt(t) := dnxt(43,0) }
  }}



}
