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

abstract class CsrFiles_H extends CsrFiles_S {

  //hypervisor trap setup
  lazy val hstatus = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h600".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val hedeleg = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h602".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val hideleg = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h603".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  val hie = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h604".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val hcounteren = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h606".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val hgeie = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h607".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //hypervisor trap handling
  lazy val htval = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h643".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val hip = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h644".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val hvip = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h645".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val htinst = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h64A".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val hgeip = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "hE12".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //hypervisor protection and translation
  lazy val hgatp = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h680".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //hypervisor counter timer virtualization registers
  lazy val htimedelta = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h605".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  //virtual supervisor registers
  lazy val vsstatus = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h200".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val vsie = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h204".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val vstvec = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h205".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val vsscratch = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h240".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val vsepc = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h241".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val vscause = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h242".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val vstval = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h243".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val vsip = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h244".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

  lazy val vsatp = {
    val value = RegInit(0.U(64.W))
    val (enable, dnxt) = Reg_Exe_Port( value, "h280".U, exe_port )
    when(enable) { value := dnxt }
    value 
  }

}

