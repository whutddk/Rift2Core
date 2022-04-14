
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

trait CsrFiles_D { this: BaseCsrFiles =>
  val ResetReq  = Wire(Bool())
  // val is_halt_request   = Wire(Bool())

  val is_inDebugMode_dnxt = Wire(Bool())
  val is_inDebugMode_en = Wire(Bool())
  val is_inDebugMode = RegEnable(next = is_inDebugMode_dnxt, init=false.B, enable=is_inDebugMode_en)
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

  val (tselect, tselect_dnxt) = SuperscalarReg( init = 0.U(64.W), is_reitred = is_retired_v )
  ( 0 until cm ).map{ t =>
    val value = if ( t == 0 ) tselect else tselect_dnxt(t-1)
    val (enable, dnxt) = Reg_Exe_Port( value, "h7A0".U, exe_port(t) )
    when(enable) { tselect_dnxt(t) := Mux( dnxt >= 0.U, ~dnxt, dnxt ) }    
  }

  val (tdata1, tdata1_dnxt) = SuperscalarReg( init = 0.U(64.W), is_reitred = is_retired_v )
  ( 0 until cm ).map{ t =>
    val value = if ( t == 0 ) tdata1 else tdata1_dnxt(t-1)
    val (enable, dnxt) = Reg_Exe_Port( value, "h7A1".U, exe_port(t) )
    when(enable) { tdata1_dnxt(t) := dnxt }    
  }


  val (tdata2, tdata2_dnxt) = SuperscalarReg( init = 0.U(64.W), is_reitred = is_retired_v )
  ( 0 until cm ).map{ t =>
    val value = if ( t == 0 ) tdata2 else tdata2_dnxt(t-1)
    val (enable, dnxt) = Reg_Exe_Port( value, "h7A2".U, exe_port(t) )
    when(enable) { tdata2_dnxt(t) := dnxt }    
  }

  val (tdata3, tdata3_dnxt) = SuperscalarReg( init = 0.U(64.W), is_reitred = is_retired_v )
  ( 0 until cm ).map{ t =>
    val value = if ( t == 0 ) tdata3 else tdata3_dnxt(t-1)
    val (enable, dnxt) = Reg_Exe_Port( value, "h7A3".U, exe_port(t) )
    when(enable) { tdata3_dnxt(t) := dnxt }    
  }

  //Debug Mode Register
  val dcsr_xdebugver = 4.U(4.W)
  /** ebreak will trap in debugMode */
  val (dcsr_ebreakm, dcsr_ebreakm_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  //ebreak will trap in debugMode
  val (dcsr_ebreaks, dcsr_ebreaks_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  //ebreak will trap in debugMode
  val (dcsr_ebreaku, dcsr_ebreaku_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  //Interrupts will be ban in step mode
  val dcsr_stepie  = 0.U(1.W) 
  val dcsr_stopcount = 0.U(1.W)
  val dcsr_stoptime = 0.U(1.W)
  val (dcsr_cause, dcsr_cause_dnxt) = SuperscalarReg( init = 0.U(3.W), is_reitred = is_retired_v )
  val dcsr_mprven = 0.U(1.W)
  val (dcsr_nmip, dcsr_nmip_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  val (dcsr_step, dcsr_step_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  val (dcsr_prv, dcsr_prv_dnxt  ) = SuperscalarReg( init = 3.U(2.W), is_reitred = is_retired_v )

  val dcsr      = Cat( dcsr_xdebugver, 0.U(12.W), dcsr_ebreakm, dcsr_ebreaks, dcsr_ebreaku, dcsr_stepie, dcsr_stopcount, dcsr_stoptime, dcsr_cause, 0.U(1.W), dcsr_mprven, dcsr_nmip, dcsr_step, dcsr_prv )
  val dcsr_dnxt = ( 0 until cm ).map{ t => 
    Cat( dcsr_xdebugver_dnxt(t), 0.U(12.W), dcsr_ebreakm_dnxt(t), dcsr_ebreaks_dnxt(t), dcsr_ebreaku_dnxt(t), dcsr_stepie_dnxt(t), dcsr_stopcount_dnxt(t), dcsr_stoptime_dnxt(t), dcsr_cause_dnxt(t), 0.U(1.W), dcsr_mprven_dnxt(t), dcsr_nmip_dnxt(t), dcsr_step_dnxt(t), dcsr_prv_dnxt(t) )
  }}

  ( 0 until cm ).map{ t => {
    val value = { if ( t == 0 ) {dcsr} else {dcsr_dnxt(t-1)} }
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B0".U, exe_port(t) )
    when(false.B) {}
    .elsewhen( is_inDebugMode_dnxt(t) & is_inDebugMode_en(t) ){
      prv(t) := if ( t == 0 ) priv_lvl else priv_lvl_dnxt(t-1)
    }
    .elsewhen(enable) {
      step(t) := dnxt(2)
      prv(t)  := dnxt(1,0)
    }

  }}

  val (dpc, dpc_dnxt) = SuperscalarReg( init = 0.U(64.W), is_reitred = is_retired_v )

  ( 0 until cm ).map{ t => {
    val value = if( t == 0 ) {dpc} else {dpc_dnxt(t-1)}
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B1".U, exe_port(t) )
    when(enable) { dpc_dnxt(t) := dnxt }
    .elsewhen( is_inDebugMode_dnxt(t) === true.B ) {
      dpc_dnxt(t) := Mux1H(Seq(
        is_ebreak_retired -> commit_pc(t),
        is_single_step    -> commit_pc(t),
        is_trigger        -> 0.U,
        is_halt_int       -> commit_pc(t),
      ))
    }
  }}

  val (dscratch0, dscratch0_dnxt) = SuperscalarReg( init = 0.U(64.W), is_reitred = is_retired_v )
  ( 0 until cm ).map{ t => {
    val value = if( t == 0 ) dscratch0 else dscratch0_dnxt(t-1)
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B2".U, exe_port(t) )
    when(enable) { dscratch0_dnxt(t) := dnxt }    
  }}


  val (dscratch1, dscratch1_dnxt) = SuperscalarReg( init = 0.U(64.W), is_reitred = is_retired_v )
  ( 0 until cm ).map{ t => {
    val value = if( t == 0 ) dscratch1 else dscratch1_dnxt(t-1)
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B3".U, exe_port(t) )
    when(enable) { dscratch1_dnxt(t) := dnxt }    
  }}

  val (dscratch2, dscratch2_dnxt) = SuperscalarReg( init = 0.U(64.W), is_reitred = is_retired_v )
  ( 0 until cm ).map{ t => {
    val value = if( t == 0 ) dscratch2 else dscratch2_dnxt(t-1)
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B4".U, exe_port(t) )
    when(enable) { dscratch2_dnxt(t) := dnxt }    
  }}


}



