
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

abstract class CsrFiles_D extends CsrFiles_M {


    
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
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B0".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  dpc := {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h7B1".U, exe_port )
    when(enable) { value := dnxt }
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
}



