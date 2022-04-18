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

trait CsrFiles_S {  this: BaseCommit =>



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

  // val sstatus = {
  //   mstatus & Cat( "b1".U, 0.U(29.W), "b11".U, 0.U(12.W), "b11011110000101100010".U )
  // }



/**
  * Supervisor Interrupt Register -- sie (enable)
  * @note read-only, meie(11), mtie(7), msie(3) is visible and maskable when mideleg(x) set
  * 
  */
  // val (meie, meie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (seie, seie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (mtie, mtie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (stie, stie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (msie, msie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (ssie, ssie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )

  // val sie = mie
  //     Cat(
  //       0.U(4.W), meie & mideleg(11), 0.U(1.W), seie,
  //       0.U(1.W), mtie & mideleg(7),  0.U(1.W), stie,
  //       0.U(1.W), msie & mideleg(3),  0.U(1.W), ssie, 0.U(1.W) )

  // val sie_dnxt = ( 0 until cm ).map{ t =>
  //   Cat(
  //     0.U(4.W), meie_dnxt(t) & mideleg_dnxt(t)(11), 0.U(1.W), seie_dnxt(t),
  //     0.U(1.W), mtie_dnxt(t) & mideleg_dnxt(t)(7),  0.U(1.W), stie_dnxt(t),
  //     0.U(1.W), msie_dnxt(t) & mideleg_dnxt(t)(3),  0.U(1.W), ssie_dnxt(t), 0.U(1.W) )    
  // }

  // ( 0 until cm ).map{ t => {
  //   val value = if ( t == 0 ) sie else sie_dnxt(t-1)
  //   val (enable0, dnxt0) = Reg_Exe_Port( value, "h104".U, exe_port(t) )
  //   val (enable1, dnxt1) = Reg_Exe_Port( value, "h304".U, exe_port(t) )

  //   when(enable0) {
  //     meie_dnxt(t) := dnxt0(11)
  //     seie_dnxt(t) := dnxt0(9)
  //     mtie_dnxt(t) := dnxt0(7)
  //     stie_dnxt(t) := dnxt0(5)
  //     msie_dnxt(t) := dnxt0(3)
  //     ssie_dnxt(t) := dnxt0(1)

  //   }
  //   .elsewhen(enable1) {
  //     seie_dnxt(t) := dnxt1(9)
  //     stie_dnxt(t) := dnxt1(5)
  //     ssie_dnxt(t) := dnxt1(1)
  //   }
  // }}


  /**
    * Supervisor Trap Vector Base Address Register --stvec
    *
    * @note holdstrap vector configuration
    * @param base (63,2) vector base address, either va or pa
    * @param mode (1,0) vector mode,hard-wire to 0 in this version
    */
  def update_stvec( in: CMMState_Bundle ): TVecBundle = {
    val stvec = WireDefault( in.csrfiles.stvec )

    stvec.mode := 0.U(2.W)

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.stvec, "h105".U, exe_port )
    when(enable) { stvec.base := dnxt(63,2) }

    return stvec
  }


  /**
    * Supervisor Timers and Performance Counters -- Counter-Enable Register -- scounteren
    * 
    * @note controls the availability of the hardware performance monitoring counters to U-mode
    * @param HPM (31,3) Whether is allowed to access hpmcounter(x) in U-mode
    * @param IR (2) Whether is allowed to access instret in U-mode
    * @param TM (1) Whether is allowed to access time in U-mode
    * @param CY (0) Whether is allowed to access cycle in U-mode
    */

  def update_scounteren( in: CMMState_Bundle ): CounterenBundle = {
    val scounteren = WireDefault( in.csrfiles.scounteren )
 
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.scounteren, "h306".U, exe_port )
      when(enable) { scounteren.hpm := dnxt }    


    return scounteren
  }

  //supervisor trap handling

  /**
    * Supervisor Scratch Register -- sscratch
    *
    * @note used to hold a pointer to the hart-local supervisor context while the hart is executing user code
    */
  def update_sscratch( in: CMMState_Bundle ): UInt = {
    val sscratch = WireDefault( in.csrfiles.sscratch )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.sscratch, "h140".U, exe_port )
    when(enable) { sscratch := dnxt }
    return sscratch
  }


  /**
    * Supervisor Exception Program Counter -- sepc
    * 
    * hold virtual addresses: when a trap is taken into S-mode, sepc is written with the virtual address of
    * the instruction that was interrupted or that encountered the exception
    */
  def update_sepc( in: CMMState_Bundle ): UInt = {
    val sepc = WireDefault( in.csrfiles.sepc )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.sepc, "h141".U, exe_port )

    when( in.is_trap & in.priv_lvl_dnxt === "b01".U ) { sepc := commit_pc }
    .elsewhen(enable) { sepc := dnxt }
    return sepc
  }

  /**
    * Supervisor Cause Register -- scause
    * 
    * when a trap is taken into S-mode, scause is written with a code indicating the event that cause the trap
    * @return
    */
  def update_scause( in: CMMState_Bundle ): UInt = {
    val scause = WireDefault( in.csrfiles.scause )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.scause, "h142".U, exe_port )

    when( ( in.is_m_interrupt | in.is_s_interrupt ) & in.priv_lvl_dnxt(t) === "b01".U ) {
      scause.interrupt := 1.U
      scause.exception_code := Mux1H( Seq(
        in.is_ssi -> 1.U,
        in.is_msi -> 3.U,
        in.is_sti -> 5.U,
        in.is_mti -> 7.U,
        in.is_sei -> 9.U,
        in.is_mei -> 11.U
      ))
    }
    .elsewhen( in.is_exception & in.priv_lvl_dnxt === "b01".U ) {
      scause.interrupt := 0.U
      scause.exception_code := Mux1H( Seq(
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
      scause.interrupt      := dnxt(63)
      scause.exception_code := dnxt(62,0)
    }
    return scause
  }

  /**
    * Supervisor Trap Value Register -- stval
    * 
    * when a trap is taken into S-mode, stval is written with exception-specific information to assist softwave in handling the trap
    *
    * @return
    */
  def update_stval( in: CMMState_Bundle ): UInt = {
    val stval = WireDefault( in.csrfiles.stval )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.stval, "h143".U, exe_port )

    when( in.priv_lvl_dnxt === "b01".U ) {
      stval := Mux1H( Seq(
        in.is_instr_access_fault    -> ill_ivaddr,
        in.is_instr_paging_fault    -> ill_ivaddr,
        in.is_instr_illeage         -> ill_instr,
        in.is_breakPoint            -> 0.U,
        in.is_load_misAlign         -> ill_dvaddr,
        in.is_load_access_fault     -> ill_dvaddr,
        in.is_storeAMO_misAlign     -> ill_dvaddr,
        in.is_storeAMO_access_fault -> ill_dvaddr,
        in.is_load_paging_fault     -> ill_dvaddr,
        in.is_storeAMO_paging_fault -> ill_dvaddr       
      ))
    }
    .elsewhen(enable) { stval := dnxt }
    return stval
  }



/**
  * Supervisor Interrupt Register -- sip
  * @note read-only, meip(11), mtip(7), msip(3) is visible when mideleg(x) set
  * 
  */

  //   val meip = clint_ex_m
  //   val seip = clint_ex_s
  //   val mtip = clint_tm_m
  //   val stip = clint_tm_s
  //   val msip = clint_sw_m
  //   val ssip = clint_sw_s

  // val sip =
  //   Cat(
  //     0.U(4.W), meip & mideleg(11), 0.U(1.W), seip,
  //     0.U(1.W), mtip & mideleg(7),  0.U(1.W), stip,
  //     0.U(1.W), msip & mideleg(3),  0.U(1.W), ssip, 0.U(1.W) )

  // val sip_dnxt = ( 0 until cm ).map{ t => 
  //   Cat(
  //     0.U(4.W), meip & mideleg_dnxt(t)(11), 0.U(1.W), seip,
  //     0.U(1.W), mtip & mideleg_dnxt(t)(7),  0.U(1.W), stip,
  //     0.U(1.W), msip & mideleg_dnxt(t)(3),  0.U(1.W), ssip, 0.U(1.W) )
  // }


  /**
    * Supervisor Address protection and translation Register -- satp
    *
    * @param mode (63,60) select the current address-translation scheme  
    * @param asid (59,44) address space identifier, which facilitates address-translation fences on a per-address-space basis
    * @param PPN (43,0) physical page number (ppn) of the root page table
    */

  def update_satp( in: CMMState_Bundle ): SatpBundle = {
    val satp = WireDefault( in.csrfiles.satp )

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.satp, "h180".U, exe_port )
    when(enable) {
      /** @note only sv39 supportted */
      satp.mode := dnxt(63,60) & "b1000".U
      satp.asid := dnxt(59,44)
      satp.ppn  := dnxt(43,0)
    }

    return stap
  }



}
