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


package base

import chisel3._
import chisel3.util._

class ageMatrix(entry: Int) {

  val ageMatrixR = Wire( Vec( entry, Vec(entry, Bool()) ) )
  val ageMatrixW = Reg(  Vec( entry, Vec(entry, Bool()) ) )


  for ( i <- 0 until entry ) {
    for( j <- 0 until i ) {
      ageMatrixR(i)(j) := ~ageMatrixW(j)(i)
    }
    ageMatrixR(i)(i) := DontCare
    for( j <- i until entry ) {
      ageMatrixR(i)(j) := ageMatrixW(i)(j)
    }
  }

  def update( chn: Int ) = {
    ageMatrixW(chn).map{ x => {x := true.B} }
    ageMatrixW.map{ x => {x(chn) := false.B} }
  }

  def MatrixMask( maskCond: Vec[Bool] ): (Vec[Vec[Bool]], Bool, UInt) = {
    require( maskCond.length == entry )

    val matrixOut = Wire( Vec( entry, Vec( entry, Bool()) ))
    for ( i <- 0 until entry ){
      for ( j <- 0 until entry ){
        matrixOut(i)(j) := (ageMatrixR(i)(j) & ~maskCond(j)) | maskCond(i)
      }
    }

    val isValid   = ~matrixOut.forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }}
    val oldestIdx = Mux1H(( 0 until entry ).map{ i => { (matrixOut(i).forall( (y: Bool) => (y === false.B) )) -> i.U } })

    assert(
      matrixOut.forall( (x: Vec[Bool]) => x.forall{(y: Bool) => (y === true.B)} ) |
      PopCount( matrixOut.map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
    )

    return (matrixOut, isValid, oldestIdx)
  }


}

