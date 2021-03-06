


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


package rift2Core.privilege

import chisel3._
import chisel3.util._

import rift2Core.define._
import rift2Core.backend._
import rift2Core.privilege.csrFiles._


abstract class Privilege extends CsrFiles{

  priv_lvl_dnxt :=
    WireDefault(
      Mux1H( Seq(
        is_mRet -> mstatus(12,11),
        is_sRet -> mstatus(8),

        is_ssi -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(1),  "b11".U, "b01".U ) ),
        is_msi -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(3),  "b11".U, "b01".U ) ),
        is_sti -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(5),  "b11".U, "b01".U ) ),
        is_mti -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(7),  "b11".U, "b01".U ) ),
        is_sei -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(9),  "b11".U, "b01".U ) ),
        is_mei -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux( ~mideleg(11), "b11".U, "b01".U ) ),

        is_instr_misAlign        -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(0),  "b11".U, "b01".U) ),
        is_instr_access_fault    -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(1),  "b11".U, "b01".U) ),
        is_instr_illeage         -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(2),  "b11".U, "b01".U) ),
        is_breakPoint            -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(3),  "b11".U, "b01".U) ),
        is_load_misAlign         -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(4),  "b11".U, "b01".U) ),
        is_load_access_fault     -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(5),  "b11".U, "b01".U) ),
        is_storeAMO_misAlign     -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(6),  "b11".U, "b01".U) ),
        is_storeAMO_access_fault -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(7),  "b11".U, "b01".U) ),
        is_ecall_U               -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(8),  "b11".U, "b01".U) ),
        is_ecall_S               -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(9),  "b11".U, "b01".U) ),
        is_ecall_M               -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(11), "b11".U, "b01".U) ),
        is_instr_paging_fault    -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(12), "b11".U, "b01".U) ),
        is_load_paging_fault     -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(13), "b11".U, "b01".U) ),
        is_storeAMO_paging_fault -> Mux( priv_lvl_qout === "b11".U, "b11".U, Mux(~medeleg(15), "b11".U, "b01".U) ),

      ))
    )

  priv_lvl_enable := is_trap | is_mRet | is_sRet

}


