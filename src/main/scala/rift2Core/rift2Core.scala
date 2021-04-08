/*
* @Author: Ruige Lee
* @Date:   2021-03-18 16:11:48
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-23 19:17:01
*/


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


package rift2Core

import chisel3._
import chisel3.util._
import rift2Core.basic._

class Rift2Core extends RawModule {
  val io = IO(new Bundle{
    val a = Input(UInt(1.W))
    val b = Input(UInt(1.W))
    val c = Output(UInt(1.W))
  })


  io.c := io.a | io.b



  val regfiles = Module(new Regfiles)

  

}




