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


class Vector_Commit_Bundle()(implicit p: Parameters) extends RiftBundle{
  val isComfirm = Output(Bool())
  val isAbort   = Output(Bool())
  val raw       = Output(UInt(( log2Ceil(32) ).W))

  val isWroteback  = Input(Bool())
  val isExcepiton  = Input(Bool())
  val excepitonIdx = Input(UInt((log2Ceil(vParams.vlen/8).W)))
}

class Vector_WriteBack_Bundle()(implicit p: Parameters) extends RiftBundle {
  val res = UInt(8.W)
  // val isMask = Bool()
  // val isExcepiton = Bool()
}

abstract class VRegfilesBase()(implicit p: Parameters) extends RiftModule {

  class VRegFilesIO extends Bundle{
    val molloc = Flipped(Decoupled(new UInt(5.W)) ) //molloc at read op
    val readOp = Vec( 32, Valid(UInt( (vParams.vlen).W )) )

    val writeBack = Vec(wbc, Vec( 32, Vec(8, (new Valid(new Vector_WriteBack_Bundle)))))
    val exception = 

    val commit = Vec(cmm, Flipped(new Vector_Commit_Bundle))
    val diffReg = Output(Vec(32, UInt((vParams.vlen/8).W)))    
  }

  val io: RegFilesIO = IO( new VRegFilesIO)

  val wbFiles =
    for( _ <- 0 until 32 ) yield {
      for( _ <- 0 until vParams.vlen/8 ) yield
      Reg(UInt(8.U))
    }

  val files =
    for( _ <- 0 until 32 ) yield {
      Reg(UInt(vParams.vlen))
    }

  val isMolloced =
    for( _ <- 0 until 32 ) yield {
      // for( _ <- 0 until vParams.vlen/8 ) yield {
        RegInit(false.B)      
      // }
    }

  val isWroteBack =
    for( _ <- 0 until 32 ) yield {
      for( _ <- 0 until vParams.vlen/8 ) yield {
        RegInit(true.B)
      }
    }

  val isExcepiton =
    for( _ <- 0 until 32 ) yield {
      for( _ <- 0 until vParams.vlen/8 ) yield {
        RegInit(false.B)      
      }
    }

}

trait VRegMolloc{ this: VRegfilesBase =>
  io.molloc.ready := 
    Mux1H( ( 0 until 32 ).map{ i => (i.U === io.molloc.bits) -> ~(isMolloced(i)) } )

  when( io.molloc.fire ){
    ( 0 until 32 ).map{ i =>
      when( i.U === io.molloc.bits ) {
        assert( isMolloced(i) === false.B )
        isMolloced(i) := true.B
        isWroteBack(i) := 0.U.asTypeOf(isWroteBack(i))
      }
    }
  }
}

trait VRegReadOp{ this: VRegfilesBase =>
  for( i <- 0 until 32 ){
    io.readOp(i).valid :=
      (~isMolloced(i)) |
      (isMolloced(i) & isWroteBack(i).reduce(_&_))

    io.readOp(i).bits := 
      Mux(
        isMolloced(i),
        Mux( isWroteBack(i).reduce(_&_), wbFiles, 0.U),
        files(i)
      )
  }
}

trait VRegWriteBack{ this: VRegfilesBase =>

  for( chn <- 0 until wbc ) {
    for( i <- 0 until 32 ) {
      for( j <- 0 until 8 ) {
        when( io.writeBack(i)(j).fire ){

          assert( isMolloced(i) )

          when( ~io.writeBack(i)(j).bits.isMask ) {
            wbFiles(i)(j) := io.writeBack(chn)(i)(j).bits.res
          }

          isExcepiton(i)(j) := io.writeBack(i)(j).bits.isException
          isWroteBack(i)(j) := true.B

        }
      }
    }
  }

}




/**
  * In order regfile commit donot need override
  */
trait VRegCommit{ this: VRegfilesBase =>
  for ( i <- 0 until cmm ){ 
    val idx = io.commit(i).raw
    io.commit(i).isWroteback  := isWroteBack(idx).reduce(_&_)
    io.commit(i).isExcepiton  := isExcepiton(idx).reduce(_|_)
    io.commit(i).excepitonIdx := isExcepiton(idx).indexWhere( (x: Bool) => (x === true.B) )

    when( io.commit(i).isAbort ){
      isMolloced  := 0.U.asTypeOf(isMolloced)
      isWroteBack := 0.U.asTypeOf(isWroteBack)
      isExcepiton := 0.U.asTypeOf(isExcepiton)
    }
    .elsewhen( io.commit(i).isComfirm ){
      files(idx) := Cat( wbFiles(idx).reverse )
      isMolloced(idx)  := false.B
      isWroteBack(idx) := 0.U.asTypeOf(isWroteBack(idx))
      isExcepiton(idx) := 0.U.asTypeOf(isExcepiton(idx))
    }
  }
}

class VRegfiles()(implicit p: Parameters) extends VRegfilesBase
with VRegMolloc
with VRegReadOp
with VRegWriteBack
with VRegCommit{

}