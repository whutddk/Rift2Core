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


class Vector_Molloc_Bundle()(implicit p: Parameters) extends RiftBundle{
  val idx    = UInt(6.W)
  val vsew   = UInt(2.W)
  val isMask = Vec( vParams.vlen / 8, Bool() )
}


class Vector_Commit_Bundle()(implicit p: Parameters) extends RiftBundle{
  val isComfirm = Output(Bool())
  val isAbort   = Output(Bool())
  val phy       = Output(UInt(( log2Ceil(33) ).W))

  val isWroteback  = Input(Bool())
  val isException  = Input(Bool())
  val exceptionIdx = Input(UInt((log2Ceil(vParams.vlen/8).W)))
}

class Vector_WriteBack_Bundle()(implicit p: Parameters) extends WriteBack_info(dw = 8) {
  val eleIdx = UInt( (log2Ceil(vParams.vlen/8)).W )

  val isException = Bool()
}


abstract class VRegFilesBase()(implicit p: Parameters) extends RiftModule {

  class VRegFilesIO extends Bundle{
    val molloc = Flipped(Decoupled(new Vector_Molloc_Bundle ) ) //molloc at read op
    val readOp = Vec( 32, Valid(UInt( (vParams.vlen).W )) )

    val writeBack  = Valid(new Vector_WriteBack_Bundle)

    val commit = Vec(cmChn, Flipped(new Vector_Commit_Bundle))
    val diffReg = Output(Vec(32, UInt((vParams.vlen/8).W)))
  }

  val io: VRegFilesIO = IO( new VRegFilesIO)

  val wbFiles =
    for( _ <- 0 until 33 ) yield {
      for( _ <- 0 until vParams.vlen/8 ) yield
      Reg(UInt(64.U))
    }

  val files =
    for( _ <- 0 until 33 ) yield {
      Reg(UInt(vParams.vlen))
    }

  val isMolloced =
    for( _ <- 0 until 33 ) yield {
      RegInit(false.B)      
    }
  
  val vsew =
    for( _ <- 0 until 33 ) yield {
      RegInit(UInt(2.W))      
    }

  val isWroteBack =
    for( _ <- 0 until 33 ) yield {
      for( _ <- 0 until vParams.vlen/8 ) yield {
        RegInit(true.B)
      }
    }

  val isException =
    for( _ <- 0 until 33 ) yield {
      for( _ <- 0 until vParams.vlen/8 ) yield {
        RegInit(false.B)      
      }
    }

  val isMask =
    for( _ <- 0 until 33 ) yield {
      for( _ <- 0 until vParams.vlen/8 ) yield {
        RegInit(false.B)      
      }
    }
}

trait VRegMolloc{ this: VRegFilesBase =>
  io.molloc.ready := 
    Mux1H( ( 0 until 32 ).map{ i => (i.U === io.molloc.bits) -> ~(isMolloced(i)) } )

  when( io.molloc.fire ){
    ( 0 until 32 ).map{ i =>
      when( i.U === io.molloc.bits.idx ) {
        assert( isMolloced(i) === false.B )
        isMolloced(i) := true.B
        vsew(i)       := io.molloc.bits.sew

        for( j <- 0 until vParams.vlen/8 ) {
          when( io.molloc.bits.sew === "b00".U ){
            isWroteBack(i)(j) := io.molloc.bits.isMask(j)
            isMask(i)(j)      := io.molloc.bits.isMask(j)
          }
          when( io.molloc.bits.sew === "b01".U ){
            if( j < vParams.vlen/16 ){
              isWroteBack(i)(j) := io.molloc.bits.isMask(j)
              isMask(i)(j)      := io.molloc.bits.isMask(j)
            }
          }
          when( io.molloc.bits.sew === "b10".U ){
            if( j < vParams.vlen/32 ){
              isWroteBack(i)(j) := io.molloc.bits.isMask(j)
              isMask(i)(j)      := io.molloc.bits.isMask(j)
            }
          }
          when( io.molloc.bits.sew === "b11".U ){
            if( j < vParams.vlen/64 ){
              isWroteBack(i)(j) := io.molloc.bits.isMask(j)
              isMask(i)(j)      := io.molloc.bits.isMask(j)
            }
          }
        }
      }
    }
  }
}

trait VRegReadOp{ this: VRegFilesBase =>
  for( i <- 0 until 32 ){
    io.readOp(i).valid := ~isMolloced(i)
      // (~isMolloced(i)) |
      // (isMolloced(i) &
      //   Mux1H(Seq(
      //     (vsew(i) === "b00".U) -> (0 until vParams.vlen/8 ).map{ j => isWroteBack(i)(j) }.reduce(_&_),
      //     (vsew(i) === "b01".U) -> (0 until vParams.vlen/16).map{ j => isWroteBack(i)(j) }.reduce(_&_),
      //     (vsew(i) === "b10".U) -> (0 until vParams.vlen/32).map{ j => isWroteBack(i)(j) }.reduce(_&_),
      //     (vsew(i) === "b11".U) -> (0 until vParams.vlen/64).map{ j => isWroteBack(i)(j) }.reduce(_&_),
      //   ))
      // )

    io.readOp(i).bits := files(i)
      // Mux( isMolloced(i), Cat(wbFiles(i), files(i) )
  }
}

trait VRegWriteBack{ this: VRegFilesBase =>

  // for( chn <- 0 until wbc ) {


  when( io.writeBack.fire ){
    val i = io.writeBack.bits.rd0
    val j = io.writeBack.bits.eleIdx

    assert( isMolloced(i) )
    when( vsew(i) === "b00".U ) {assert( j.U < (vParams.vlen/8 ).U, "Assert Failed, invalid VWriteback" )}
    when( vsew(i) === "b01".U ) { assert( j.U < (vParams.vlen/16).U, "Assert Failed, invalid VWriteback" ) }
    when( vsew(i) === "b10".U ) { assert( j.U < (vParams.vlen/32).U, "Assert Failed, invalid VWriteback" ) }
    when( vsew(i) === "b11".U ) { assert( j.U < (vParams.vlen/64).U, "Assert Failed, invalid VWriteback" ) }

    wbFiles(i)(j) := io.writeBack.bits.res

    isException(i)(j) := io.writeBack.bits.isException
    isWroteBack(i)(j) := true.B

  }


  // }

}




/**
  * In order regfile commit donot need override
  */
trait VRegCommit{ this: VRegFilesBase =>
  for ( i <- 0 until cmChn ){ 
    val idx = io.commit(i).phy
    io.commit(i).isWroteback  := isWroteBack(idx).reduce(_&_)
    io.commit(i).isException  := isException(idx).reduce(_|_)
    io.commit(i).exceptionIdx := isException(idx).indexWhere( (x: Bool) => (x === true.B) )

    when( io.commit(i).isAbort ){
      isMolloced  := 0.U.asTypeOf(isMolloced)
      isWroteBack := 0.U.asTypeOf(isWroteBack)
      isException := 0.U.asTypeOf(isException)
    }
    .elsewhen( io.commit(i).isComfirm ){
      isMolloced(idx)  := false.B
      isWroteBack(idx) := 0.U.asTypeOf(isWroteBack(idx))
      isException(idx) := 0.U.asTypeOf(isException(idx))

      when( vsew(idx) === "b00".U ) {
        files(idx) := Cat(
          ( 0 until vParams.vlen/8 ).map{ j =>
            Mux( isMask(idx)(j), files(idx)( 8*j+7, 8*j ), wbFiles(idx)(j)(7,0) )
          }.reverse
        )
      } .elsewhen( vsew(idx) === "b01".U ) {
        files(idx) := Cat(
          ( 0 until vParams.vlen/16 ).map{ j =>
            Mux( isMask(idx)(j), files(idx)( 16*j+15, 16*j ), wbFiles(idx)(j)(15,0) )
          }.reverse
        )
      } .elsewhen( vsew(idx) === "b10".U ) {
        files(idx) := Cat(
          ( 0 until vParams.vlen/32 ).map{ j =>
            Mux( isMask(idx)(j), files(idx)( 32*j+31, 32*j ), wbFiles(idx)(j)(31,0) )
          }.reverse
        )
      } .elsewhen( vsew(idx) === "b11".U ) {
        files(idx) := Cat(
          ( 0 until vParams.vlen/64 ).map{ j =>
            Mux( isMask(idx)(j), files(idx)( 64*j+63, 64*j ), wbFiles(idx)(j)(63,0) )
          }.reverse
        )
      }

    }
  }
}

class VRegFiles()(implicit p: Parameters) extends VRegFilesBase
with VRegMolloc
with VRegReadOp
with VRegWriteBack
with VRegCommit{

}



class FakeVRegFiles()(implicit p: Parameters) extends VRegFilesBase{
  io.molloc.ready := true.B

  for( i <- 0 until 32 ) {
    io.readOp(i).valid := false.B
    io.readOp(i).bits  := 0.U
  }

  for( i<- 0 until cmChn ){
    io.commit(i).isWroteBack := true.B
    io.commit(i).isException := false.B
    io.commit(i).exceptionIdx := 0.U
  }

  io.diffReg := 0.U.asTypeOf(io.diffReg)
}
