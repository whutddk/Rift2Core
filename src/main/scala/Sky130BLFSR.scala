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

package rift2Chip

import chisel3._
import chisel3.util._

import chisel3.util.random._

class Sky130BLFSR extends Module{
  val io = IO(new Bundle{
    val num = Output(Vec(4, UInt(7.W)))

    val lock = Input(Bool())
  })
  def zero  = "b1000000".U
  def one   = "b1111001".U
  def two   = "b0100100".U
  def three = "b0110000".U
  def four  = "b0011001".U
  def five  = "b0010010".U
  def six   = "b0000010".U
  def seven = "b1111000".U
  def eight = "b0000000".U
  def nine  = "b1101111".U
  def A     = "b0001000".U
  def B     = "b0000011".U
  def C     = "b1000110".U
  def D     = "b0100001".U
  def E     = "b0000110".U
  def F     = "b0001110".U


  val lfsr = LFSR(16, ~io.lock)


  for( i <- 0 until 4 ) {
    when( io.lock ) {
      io.num(i) := "b1111111".U
    } .otherwise {
      io.num(i) := Mux1H(Seq(
        (lfsr(i*4+3, i*4) === 0.U) -> zero, 
        (lfsr(i*4+3, i*4) === 1.U) -> one, 
        (lfsr(i*4+3, i*4) === 2.U) -> two, 
        (lfsr(i*4+3, i*4) === 3.U) -> three, 
        (lfsr(i*4+3, i*4) === 4.U) -> four, 
        (lfsr(i*4+3, i*4) === 5.U) -> five, 
        (lfsr(i*4+3, i*4) === 6.U) -> six, 
        (lfsr(i*4+3, i*4) === 7.U) -> seven, 
        (lfsr(i*4+3, i*4) === 8.U) -> eight, 
        (lfsr(i*4+3, i*4) === 9.U) -> nine, 
        (lfsr(i*4+3, i*4) === 10.U) -> A, 
        (lfsr(i*4+3, i*4) === 11.U) -> B, 
        (lfsr(i*4+3, i*4) === 12.U) -> C, 
        (lfsr(i*4+3, i*4) === 13.U) -> D, 
        (lfsr(i*4+3, i*4) === 14.U) -> E, 
        (lfsr(i*4+3, i*4) === 15.U) -> F, 

      ))
    }
  }


}