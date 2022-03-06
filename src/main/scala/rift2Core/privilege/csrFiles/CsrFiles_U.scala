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

abstract class CsrFiles_U extends CsrFiles_D {
    

  //user trap setup
  // ustatus := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h000".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // uie := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h004".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // utvec := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h005".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  //user trap handling
  // uscratch := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h040".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // uepc := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h041".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // ucause := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h042".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // utval := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h043".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // uip := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h044".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // sedeleg := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h102".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // sideleg := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h103".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

}
