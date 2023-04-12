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

package rift2Core.backend

import chisel3._
import chisel3.util._
import rift2Core.define._

import rift2Chip._
import org.chipsalliance.cde.config._


class MultiplerAccumulater() extends Module{
  // require( vParams.elen <= 64 )

  class MultiplerAccumulaterIO extends Bundle{
    // val req = Flipped(Decoupled(  ))
    // val rsp = Decoupled(  )
    val a   = Input(UInt(128.W))
    val b   = Input(UInt(64.W))
    val c   = Input(UInt(128.W))
    // val vm  = Input(  )
    val resh = Output(UInt(64.W))
    val resl = Output(UInt(64.W))

    val sew = Input( UInt(3.W) )
    val op  = Input( UInt(2.W) )    
  }

  val io: MultiplerAccumulaterIO = IO(new MultiplerAccumulaterIO)

  val op  = io.op
  val sew = io.sew

  val a   = Wire(Vec(8, UInt(64.W)))
  val b   = Wire(Vec(8, UInt(8.W)))
  val c   = Wire(Vec(8, UInt(8.W)))
  val res = Wire(Vec(8, UInt(128.W)))

  ( 0 until 8 ).map{ i =>
    a(i)   := Mux( op.extract(1), ~io.a + 1.U , io.a )
    b(i)   := io.b(8*i+7, 8*i)
    c(i)   := Mux( op.extract(0), ~io.c + 1.U, io.c)(8*i+7, 8*i)
    res(i) := a(i)*b(i)+c(i)
  }

  val sew8Res = ( 0 until 8 ).map{ i => 
    res(i)(15,0)
  }

  val sew16Res = ( 0 until 4 ).map{ i => 
    (res(2*i) + res(2*i+1) << 8)(31,0)
  }

  val sew32Res = ( 0 until 2 ).map{ i => 
    (
      (res(4*i)   << 0 ) +
      (res(4*i+1) << 8 ) +
      (res(4*i+2) << 16) +
      (res(4*i+3) << 24)
    ).apply(63,0)
  }

  val sew64Res = 
    (
      (res(0) << 0 ) +
      (res(1) << 8 ) +
      (res(2) << 16) +
      (res(3) << 24) +
      (res(4) << 32) +
      (res(5) << 40) +
      (res(6) << 48) +
      (res(7) << 56)
    ).apply(127,0)

  io.resh := 
    Mux1H(Seq(
      ( sew === "b000".U ) -> Cat( (0 until 8).map{ i =>  sew8Res(i)(15,8)  }.reverse ),
      ( sew === "b001".U ) -> Cat( (0 until 4).map{ i => sew16Res(i)(31,16) }.reverse ),
      ( sew === "b010".U ) -> Cat( (0 until 2).map{ i => sew32Res(i)(63,32) }.reverse ),
      ( sew === "b011".U ) -> sew64Res(127, 64),      
    ))

  io.resl := 
    Mux1H(Seq(
      ( sew === "b000".U ) -> Cat( (0 until 8).map{ i =>  sew8Res(i)(7,0)  }.reverse ),
      ( sew === "b001".U ) -> Cat( (0 until 4).map{ i => sew16Res(i)(15,0) }.reverse ),
      ( sew === "b010".U ) -> Cat( (0 until 2).map{ i => sew32Res(i)(31,0) }.reverse ),
      ( sew === "b011".U ) -> sew64Res(63, 0),      
    ))
}

