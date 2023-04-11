/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

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

class PLRU( cb: Int, cl: Int ) {
  val lvl = log2Ceil(cb)
  val sel = 
    for ( _ <- 0 until cl ) yield { 
      for ( i <- 0 until lvl ) yield {
        def n = scala.math.pow(2,i).toInt
        Reg( Vec(n, Bool() ))
      }
    }


  def update( cbSel: UInt, clSel: UInt ) = {
    require( cbSel.getWidth == lvl )

    for ( i <- 0 until cl ) {
      when( i.U === clSel ) {
        for( j <- 0 until lvl ) {
          if( j == 0 ) {
            sel(i)(j)(0) := cbSel(lvl-1-j)
          } else {
            def n = scala.math.pow(2,j).toInt
            for( k <- 0 until n ) {
              when( k.U === cbSel(lvl-1, lvl-1-(j-1)) ) {
                sel(i)(j)(k) := cbSel(lvl-1-j)            
              }
            }
          }
        }         
      }
    }
  }

  def replace(clSel: UInt): UInt = {
    val rpl = Wire( Vec( lvl, Bool() ))
    val selInCl = 
      for ( j <- 0 until lvl ) yield {
        def n = scala.math.pow(2,j).toInt
        for( k <- 0 until n ) yield {
          Mux1H( (0 until cl).map{ i => (clSel === i.U) -> sel(i)(j)(k) } )
        }
      }

    for( j <- 0 until lvl ) {
      if( j == 0 ) {
        rpl(j) := ~selInCl(j)(0)
      } else {
        def n = scala.math.pow(2,j).toInt
        def cmp = Cat( (0 until j).map{ k => rpl(k)} )
        rpl(j) :=
          Mux1H( for( k <- 0 until n ) yield{ (k.U === cmp) -> ~selInCl(j)(k) })
      }
    }

    return Cat(rpl)
  }


}


class LRU( cb: Int, cl: Int ) {
  val history =
    for ( _ <- 0 until cl ) yield {
      RegInit( VecInit( Seq.fill(cb){ 0.U(cb.W) } ) )
    }

  def update( cbSel: UInt, clSel: UInt ) = {
    for( i <- 0 until cl ) {
      when( i.U === clSel ) {
        for( j <- 0 until cb ) {
          when( j.U === cbSel ) {
            history(i)(j) := Cat( history(i)(j), 1.U(1.W) )
          } .otherwise {
            history(i)(j) := Cat( history(i)(j), 0.U(1.W) )
          }
        }
      }
    }
  }

  def replace(clSel: UInt): UInt = {
    val historyInCl = 
      // for ( j <- 0 until cb ) yield {
        Mux1H( (0 until cl).map{ i => (clSel === i.U) -> history(i) } )
      // }

    historyInCl.indexWhere( (x:UInt) => (x === 0.U))
  }
}