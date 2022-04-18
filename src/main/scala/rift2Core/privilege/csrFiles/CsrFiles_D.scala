
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

trait CsrFiles_D {  this: BaseCommit =>
  val ResetReq  = Wire(Bool())


  def update_DMode( in: CMMState_Bundle ): Bool = {
    val DMode = WireDefault( in.csrFiles.DMode )
    when( in.is_debug_interrupt ) { DMode := true.B }
    .elsewhen( in.is_dRet ) { DMode := false.B }
    return DMode
  }


  val is_step = WireDefault(dcsr(2).asBool & is_inDebugMode)

  val is_debug_interrupt = Wire(Bool())

  val is_ebreak_retired = Wire(Bool())
  val is_single_step = Wire(Bool())
  val is_trigger = Wire(Bool())
  val is_halt_int = Wire(Bool())

  is_step_int_block := ~dcsr(11) & is_inDebugMode

  val is_ebreak_breakpointn = Mux1H(Seq(
    (priv_lvl_qout === "b11".U) -> dcsr(15),
    (priv_lvl_qout === "b01".U) -> dcsr(13),
    (priv_lvl_qout === "b00".U) -> dcsr(12),
  ))

  val emu_reset = RegInit( false.B )
  when( ResetReq ) { emu_reset := true.B }
  .elsewhen( emu_reset ) { emu_reset := false.B }




  //Debug/Trace Register
  def update_tselect( in: CMMState_Bundle): UInt = {
    val tselect = WireDefault( in.csrfiles.tselect )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.tselect, "h7A0".U, exe_port )
    when(enable) { tselect := Mux( dnxt >= 0.U, ~dnxt, dnxt ) }
    return tselect
  }

  def update_tdata1( in: CMMState_Bundle): UInt = {
    val tdata1 = WireDefault( in.csrfiles.tdata1 )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.tdata1, "h7A1".U, exe_port )
    when(enable) { tdata1 := dnxt }
    return tdata1
  }

  def update_tdata2( in: CMMState_Bundle): UInt = {
    val tdata2 = WireDefault( in.csrfiles.tdata2 )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.tdata2, "h7A2".U, exe_port )
    when(enable) { tdata2 := dnxt }
    return tdata2
  }

  def update_tdata3( in: CMMState_Bundle ): UInt = {
    val tdata3 = WireDefault( in.csrfiles.tdata3 )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.tdata3, "h7A3".U, exe_port )
    when(enable) { tdata3 := dnxt }
    return tdata3
  }

  def update_dcsr( in: CMMState_Bundle ): DcsrBundle = {
    val dcsr = WireDefault( in.csrfiles.dcsr )
    dcsr.xdebugver := 4.U(4.W)
    dcsr.reserved0 := 0.U
    dcsr.reserved1 := 0.U
    dcsr.stepie    := 0.U(1.W) 
    dcsr.stopcount := 0.U(1.W)
    dcsr.stoptime  := 0.U(1.W)
    dcsr.reserved2 := 0.U
    dcsr.mprven    := 0.U(1.W)


    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.dcsr, "h7B0".U, exe_port )
    when(false.B) {}
    .elsewhen( is_debug_interrupt ){
      dcsr.prv := in.priv_lvl
    }
    .elsewhen(enable) {
      dcsr.step := dnxt(2)
      dcsr.prv  := dnxt(1,0)
    }

    return dcsr
  } 

  def update_dpc( in: CMMState_Bundle ): UInt = {
    val dpc  = WireDefault( in.csrfiles.dpc )

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.dpc, "h7B1".U, exe_port )
    when(enable) { dpc := dnxt }
    .elsewhen( in.is_inDebugMode_dnxt === true.B ) {
      dpc := Mux1H(Seq(
        in.is_ebreak_retired -> in.commit_pc,
        in.is_single_step    -> in.commit_pc,
        in.is_trigger        -> 0.U,
        in.is_halt_int       -> in.commit_pc,
      ))
    }

    return dpc
  }

  def update_dscratch0( in: CMMState_Bundle ): UInt = {
    val dscratch0 = WireDefault( in.csrfiles.dscratch0 )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.dscratch0, "h7B2".U, exe_port )
    when(enable) { dscratch0 := dnxt }
    return dscratch0
  }

  def update_dscratch1( in: CMMState_Bundle ): UInt = {
    val dscratch1 = WireDefault( in.csrfiles.dscratch1 )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.dscratch1, "h7B3".U, exe_port )
    when(enable) { dscratch1 := dnxt }
    return dscratch1
  }

  def update_dscratch2( in: CMMState_Bundle ): UInt = {
    val dscratch2 = WireDefault( in.csrfiles.dscratch2 )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.dscratch2, "h7B4".U, exe_port )
    when(enable) { dscratch2 := dnxt }
    return dscratch2
  }


}



