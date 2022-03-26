
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

abstract class CsrFiles_D extends CsrFiles_M {
  val ResetReq  = Wire(Bool())
  val is_halt_request   = Wire(Bool())

  val is_inDebugMode_dnxt = Wire(Bool())
  val is_inDebugMode = RegNext(init=false.B,next = is_inDebugMode_dnxt)
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
  tselect := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7A0".U, exe_port )
    when(enable) { value := Mux( dnxt >= 0.U, ~dnxt, dnxt ) }
    value 
  }
  tdata1 := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7A1".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  tdata2 := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7A2".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  tdata3 := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7A3".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //Debug Mode Register
  dcsr := {

  val xdebugver = 4.U(4.W)
    val ebreakm = RegInit(1.U(1.W))//ebreak will trap in debugMode
    val ebreaks = RegInit(1.U(1.W))//ebreak will trap in debugMode
    val ebreaku = RegInit(1.U(1.W))//ebreak will trap in debugMode
    val stepie  = 0.U(1.W) //Interrupts will be ban in step mode
    val stopcount = 0.U(1.W)
    val stoptime = 0.U(1.W)
    val cause = RegInit(0.U(3.W))
    val mprven = 0.U(1.W)
    val nmip = RegInit(0.U(1.W))
    val step = RegInit(0.U(1.W))
    val prv = RegInit(3.U(2.W))

    val value = Cat( xdebugver, 0.U(12.W), ebreakm, ebreaks, ebreaku, stepie, stopcount, stoptime, cause, 0.U(1.W), mprven, nmip, step, prv )


    val (enable, dnxt) = Reg_Exe_Port( value, "h7B0".U, exe_port )

    when(false.B) {}
    .elsewhen( priv_lvl_enable ){
      prv := priv_lvl_dnxt

    }
    .elsewhen(enable) {
      step := dnxt(2)
      prv  := dnxt(1,0)
    }


    value 
  }

  dpc := {

    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B1".U, exe_port )
    when(enable) { value := dnxt }
    .elsewhen( is_inDebugMode_dnxt === true.B ) {
      value := Mux1H(Seq(
        is_ebreak_retired -> commit_pc,
        is_single_step  -> commit_pc,
        is_trigger      -> 0.U,
        is_halt_request -> commit_pc,
      ))
    }
    value 
  }

  dscratch0 := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B2".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  dscratch1 := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B3".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  dscratch2 := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B4".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }
}



