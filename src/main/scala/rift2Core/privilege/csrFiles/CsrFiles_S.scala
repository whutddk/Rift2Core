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

abstract class CsrFiles_S extends CsrFiles_U {



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
  sstatus := {
    mstatus & Cat( "b1".U, 0.U(29.W), "b11".U, 0.U(12.W), "b11011110000101100010".U )
  }



/**
  * Supervisor Interrupt Register -- sie (enable)
  * @note read-only, meie(11), mtie(7), msie(3) is visible and maskable when mideleg(x) set
  * 
  */

  sie := {
    val meie = RegInit(0.U(1.W))
    val seie = RegInit(0.U(1.W))
    val mtie = RegInit(0.U(1.W))
    val stie = RegInit(0.U(1.W))
    val msie = RegInit(0.U(1.W))
    val ssie = RegInit(0.U(1.W))
    val value =
      Cat(
        0.U(4.W), meie & mideleg(11), 0.U(1.W), seie,
        0.U(1.W), mtie & mideleg(7),  0.U(1.W), stie,
        0.U(1.W), msie & mideleg(3),  0.U(1.W), ssie, 0.U(1.W) )

    val (enable0, dnxt0) = Reg_Exe_Port( value, "h104".U, exe_port )
    val (enable1, dnxt1) = Reg_Exe_Port( value, "h304".U, exe_port )

    when(enable0) {
      meie := dnxt0(11)
      seie := dnxt0(9)
      mtie := dnxt0(7)
      stie := dnxt0(5)
      msie := dnxt0(3)
      ssie := dnxt0(1)

    }
    .elsewhen(enable1) {
      seie := dnxt1(9)
      stie := dnxt1(5)
      ssie := dnxt1(1)
    }
    value
  }


  /**
    * Supervisor Trap Vector Base Address Register --stvec
    *
    * @note holdstrap vector configuration
    * @param base (63,2) vector base address, either va or pa
    * @param mode (1,0) vector mode,hard-wire to 0 in this version
    */
  stvec := {
    val base = RegInit(0.U(62.W))
    val mode = WireDefault(0.U(2.W))
    val value = Cat(base, mode)
    val (enable, dnxt) = Reg_Exe_Port( value, "h105".U, exe_port )

    when(enable) { base := dnxt(63,2) }
    value 
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

  scounteren := {
    val hpm = RegInit(0.U(32.W))
    val ir  = RegInit(0.U(1.W))
    val tm  = RegInit(0.U(1.W))
    val cy  = RegInit(0.U(1.W))
    val value = Cat( hpm(31,3), ir, tm, cy )
    val (enable, dnxt) = Reg_Exe_Port( value, "h106".U, exe_port )
    when(enable) { hpm := dnxt; ir := dnxt(2); tm := dnxt(1); cy := dnxt(0) }
    value 
  }

  //supervisor trap handling

  /**
    * Supervisor Scratch Register -- sscratch
    *
    * @note used to hold a pointer to the hart-local supervisor context while the hart is executing user code
    */
  sscratch := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h140".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }


  /**
    * Supervisor Exception Program Counter -- sepc
    * 
    * hold virtual addresses: when a trap is taken into S-mode, sepc is written with the virtual address of
    * the instruction that was interrupted or that encountered the exception
    */
  sepc := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h141".U, exe_port )

    when( is_trap & priv_lvl_dnxt === "b01".U ) {
      value := commit_pc
    }
    .elsewhen(enable) { value := dnxt }
    value & ~(1.U(64.W))
  }

  /**
    * Supervisor Cause Register -- scause
    * 
    * when a trap is taken into S-mode, scause is written with a code indicating the event that cause the trap
    * @return
    */

  scause := {

    val interrupt = RegInit(0.U(1.W))
    val exception_code = RegInit(0.U(63.W))
    val value = Cat(interrupt, exception_code)
    val (enable, dnxt) = Reg_Exe_Port( value, "h142".U, exe_port )

    when( (is_m_interrupt | is_s_interrupt) & priv_lvl_dnxt === "b01".U ) {
      interrupt := 1.U
      exception_code := Mux1H( Seq(
        is_ssi -> 1.U,
        is_msi -> 3.U,
        is_sti -> 5.U,
        is_mti -> 7.U,
        is_sei -> 9.U,
        is_mei -> 11.U
      ))
    }
    .elsewhen( is_exception & priv_lvl_dnxt === "b01".U ) {
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
    * Supervisor Trap Value Register -- stval
    * 
    * when a trap is taken into S-mode, stval is written with exception-specific information to assist softwave in handling the trap
    *
    * @return
    */
  stval := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h143".U, exe_port )
    when( priv_lvl_dnxt === "b01".U ) {
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
  * Supervisor Interrupt Register -- sip
  * @note read-only, meip(11), mtip(7), msip(3) is visible when mideleg(x) set
  * 
  */

  sip := {
    val meip = clint_ex_m
    val seip = clint_ex_s
    val mtip = clint_tm_m
    val stip = clint_tm_s
    val msip = clint_sw_m
    val ssip = clint_sw_s
    val value =
      Cat(
        0.U(4.W), meip & mideleg(11), 0.U(1.W), seip,
        0.U(1.W), mtip & mideleg(7),  0.U(1.W), stip,
        0.U(1.W), msip & mideleg(3),  0.U(1.W), ssip, 0.U(1.W) )

    value
  }

  /**
    * Supervisor Address protection and translation Register -- satp
    *
    * @param mode (63,60) select the current address-translation scheme  
    * @param asid (59,44) address space identifier, which facilitates address-translation fences on a per-address-space basis
    * @param PPN (43,0) physical page number (ppn) of the root page table
    */

  satp := {
    val mode = RegInit(0.U(4.W))
    val asid = RegInit(0.U(16.W))
    val ppn  = RegInit(0.U(44.W))
    val value = Cat( mode & "b1000".U, asid, ppn )
    val (enable, dnxt) = Reg_Exe_Port( value, "h180".U, exe_port )
    when(enable) { mode := dnxt(63,60); asid := dnxt(59,44); ppn := dnxt(43,0) }
    value 
  }

}
