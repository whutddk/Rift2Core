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

abstract class CsrFiles_U extends CsrFiles_D {
    

  //user trap setup
  // lazy val ustatus = {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h000".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // lazy val uie = {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h004".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // lazy val utvec = {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h005".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  //user trap handling
  // lazy val uscratch = {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h040".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // lazy val uepc = {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h041".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // lazy val ucause = {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h042".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // lazy val utval = {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h043".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // lazy val uip = {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h044".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // lazy val sedeleg = {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h102".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // lazy val sideleg = {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h103".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

}
