

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


class Exe_Port extends Bundle {
  val addr = UInt(12.W)
  val dat_i = UInt(64.W)
  val op_rw = Bool()
  val op_rs = Bool()
  val op_rc = Bool()
}


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

abstract class CsrFiles_port extends Module{
  val exe_port: Exe_Port
  val is_trap: Bool
  val is_mRet: Bool
  val is_sRet: Bool
  // val is_uRet: Bool
  val is_xRet = is_mRet | is_sRet //| is_uRet

  val commit_pc: UInt
  val ill_instr: UInt
  val ill_vaddr: UInt


  val is_instr_accessFault:    Bool
  val is_instr_illeage:        Bool
  val is_breakPoint:           Bool
  val is_load_misAlign:        Bool
  val is_load_accessFault:     Bool
  val is_storeAMO_misAlign:    Bool
  val is_storeAMO_accessFault: Bool
  val is_ecall:                Bool
  val is_instr_pageFault:      Bool
  val is_load_pageFault:       Bool
  val is_storeAMO_pageFault:   Bool


  val retired_cnt: UInt

  val clint_sw_m: Bool
  val clint_sw_s: Bool
  val clint_tm_m: Bool
  val clint_tm_s: Bool
  val clint_ex_m: Bool
  val clint_ex_s: Bool

  val rtc_clock: Bool
}


