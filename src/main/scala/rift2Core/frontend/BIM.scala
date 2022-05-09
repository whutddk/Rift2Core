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

package rift2Core.frontend

import chisel3._
import chisel3.util._

class Brslv_Bundle extends IFetchBundle {
  val pc = UInt(64.W)
  val ghr = UInt(64.W)
  val old_info = new 
  val is_misPredict = Bool()

}

class BIM extends IFetchModule {
  val io = IO(new Bundle{
    val pc  = Input(UInt(64.W))
    val ghr = Input(UInt(64.W))

    val brslv = Flipped(Valid())

    val prediction = Output(Bool())

    val flush = Input(Bool())
  })

  /** BIM needs power reset to initialize the ram */
  val por_reset = RegInit(true.B)
  val (reset_cl, reset_end) = Counter( range(0, bim_cl), por_reset )
  when( reset_end ) { por_reset := false.B }


  /** branch history table predict bits
    * @note when successfully predict, keep this bit
    * @note when mis-predict, revert or not this bit considering bim_H
    */
  val bim_P = Mem( bim_cl, Bool() )

  /** branch history table histroy bits 
    * @note when successfully predict, set this bit to true.B
    * @note when mis-predict, revert this bit
    */
  val bim_H = Mem( bim_cl, Bool() )

  /** branch resolve write cache line */
  val brslv_cl = HashTo0( in = HashTwo0( in1 = io.brslv.bits.pc, in2 = io.brslv.bits.ghr ), len = bht_cl)
  /** branch predice read cache line */
  val brpdr_cl = HashTo0( in = HashTwo0( in1 = io.pc, in2 = io.ghr ), len = bim_cl)


  // no overlap mis-predict will happened, for the following instr will be flushed
  when( pur_reset ) {
    bim_P.write( reset_cl, true.B )
    bim_H.write( reset_cl, false.B )
  } .elsewhen( io.brslv.valid ) {
    when( io.brslv.bits.is_misPredict ) {
      when( io.brslv.old_info.bim_H === false.B ) { bim_P.write(brslv_cl, ~io.brslv.old_info.bim_P)
      bim_H.write(brslv_cl, ~io.brslv.old_info.bim_H )
    } .otherwise {
      bim_H.write(brslv_cl, true.B)
    }
  }

  io.prediction := bim_P.read(brpdr_cl)
}

