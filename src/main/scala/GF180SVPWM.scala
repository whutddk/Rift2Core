/*
  Copyright (c) 2020 - 2024 Wuhan University of Technology <295054118@whut.edu.cn>

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

package rift2Chip

import chisel3._
import chisel3.util._

import scala.math._

class SVPWM(precision: Int = 4096) extends Module{
  require( isPow2(precision) )
  val io = IO(new Bundle{
    val freIn  = Input(Bool())
    val freOut = Output(Bool())
  })


  val freInCnt   = RegInit(0.U(64.W))
  val freInLatch = RegInit(0.U(64.W))
  val freInShift = ShiftRegisters(io.freIn, 4)

  when( freInShift(3) & ~freInShift(2) ) {
    freInLatch := freInCnt
    freInCnt    := 0.U
  } .otherwise {
    freInCnt := freInCnt + 1.U
  }


  val freDiv     = RegInit(0.U(64.W))
  val sectionCnt = RegInit(0.U(64.W))
  val sectionSel = RegInit(0.U(64.W))

  when( sectionCnt >= freDiv ) {
    sectionCnt := 0.U

    when( sectionSel >= (precision-1).U ){
      sectionSel := 0.U
      freDiv     := freInLatch >> log2Ceil(precision)
    } .otherwise{
      sectionSel := sectionSel + 1.U
    }
  } .otherwise{
    sectionCnt := sectionCnt + 1.U
  }



  val dutyCnt = RegInit(0.U(64.W))

  val dutyList = Mux1H(
    ( 0 until precision).map{ i =>
      { (sectionSel === i.U) -> ((50000000 + 50000000*sin(i*2*Pi/precision)).toInt).U }
    }
  )

  when( dutyCnt >= (100000000).U ) {
    io.freOut := true.B
    dutyCnt := 1.U
  } .elsewhen( dutyCnt >= dutyList ){
    io.freOut := false.B
    dutyCnt := dutyCnt + 1.U 
  } .otherwise {
    io.freOut := true.B
    dutyCnt := dutyCnt + 1.U 
  }


}

