


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


package rift2Core.privilege

import chisel3._
import chisel3.util._

import rift2Core.define._
import rift2Core.backend._
import rift2Core.privilege.csrFiles._

class CMMState_Bundle extends Bundle {
  




  def new_priv: UInt = {
    val new_priv = WireDefault(pirv)

    when(emu_reset) { new_priv := "b11".U}

    when(is_mRet) { new_priv := mstatus(12,11) }
    when(is_sRet) { new_priv := mstatus(8) }
    when(is_dRet) { new_priv := dcsr(1,0) }
        
    when(is_ssi) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(1),  "b11".U, "b01".U ) ) }
    when(is_msi) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(3),  "b11".U, "b01".U ) ) }
    when(is_sti) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(5),  "b11".U, "b01".U ) ) }
    when(is_mti) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(7),  "b11".U, "b01".U ) ) }
    when(is_sei) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(9),  "b11".U, "b01".U ) ) }
    when(is_mei) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(11), "b11".U, "b01".U ) ) }
    when(is_nomask_interrupt ) { new_priv := "b11".U }


    when(is_instr_misAlign       ) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(0),  "b11".U, "b01".U) )}
    when(is_instr_access_fault   ) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(1),  "b11".U, "b01".U) )}
    when(is_instr_illeage        ) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(2),  "b11".U, "b01".U) )}
    when(is_breakPoint           ) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(3),  "b11".U, "b01".U) )}
    when(is_load_misAlign        ) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(4),  "b11".U, "b01".U) )}
    when(is_load_access_fault    ) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(5),  "b11".U, "b01".U) )}
    when(is_storeAMO_misAlign    ) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(6),  "b11".U, "b01".U) )}
    when(is_storeAMO_access_fault) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(7),  "b11".U, "b01".U) )}
    when(is_ecall_U              ) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(8),  "b11".U, "b01".U) )}
    when(is_ecall_S              ) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(9),  "b11".U, "b01".U) )}
    when(is_ecall_M              ) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(11), "b11".U, "b01".U) )}
    when(is_instr_paging_fault   ) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(12), "b11".U, "b01".U) )}
    when(is_load_paging_fault    ) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(13), "b11".U, "b01".U) )}
    when(is_storeAMO_paging_fault) { new_priv := Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(15), "b11".U, "b01".U) )}
  }
  return new_priv
}


abstract class Privilege extends CsrFiles{

// priv_lvl_dnxt :=
//     WireDefault(
//       Mux1H( Seq(
//         emu_reset -> "b11".U,

//         is_mRet -> mstatus(12,11),
//         is_sRet -> mstatus(8),
//         is_dRet -> dcsr(1,0),
        

//         is_ssi -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(1),  "b11".U, "b01".U ) ),
//         is_msi -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(3),  "b11".U, "b01".U ) ),
//         is_sti -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(5),  "b11".U, "b01".U ) ),
//         is_mti -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(7),  "b11".U, "b01".U ) ),
//         is_sei -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(9),  "b11".U, "b01".U ) ),
//         is_mei -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(11), "b11".U, "b01".U ) ),
//         is_nomask_interrupt -> "b11".U,


//         is_instr_misAlign        -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(0),  "b11".U, "b01".U) ),
//         is_instr_access_fault    -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(1),  "b11".U, "b01".U) ),
//         is_instr_illeage         -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(2),  "b11".U, "b01".U) ),
//         is_breakPoint            -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(3),  "b11".U, "b01".U) ),
//         is_load_misAlign         -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(4),  "b11".U, "b01".U) ),
//         is_load_access_fault     -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(5),  "b11".U, "b01".U) ),
//         is_storeAMO_misAlign     -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(6),  "b11".U, "b01".U) ),
//         is_storeAMO_access_fault -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(7),  "b11".U, "b01".U) ),
//         is_ecall_U               -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(8),  "b11".U, "b01".U) ),
//         is_ecall_S               -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(9),  "b11".U, "b01".U) ),
//         is_ecall_M               -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(11), "b11".U, "b01".U) ),
//         is_instr_paging_fault    -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(12), "b11".U, "b01".U) ),
//         is_load_paging_fault     -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(13), "b11".U, "b01".U) ),
//         is_storeAMO_paging_fault -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(15), "b11".U, "b01".U) ),

//       ))
//     )

  priv_lvl_enable := is_trap | is_mRet | is_sRet | is_dRet | emu_reset

}


