
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
import base._

import rift2Core.privilege._
import rift2Chip._
import org.chipsalliance.cde.config._




abstract class VecPreIssueBase()(implicit p: Parameters) extends RiftModule{
  class VecPreIssueIO extends Bundle{
    val enq = Vec(rnChn, Flipped(new DecoupledIO(new Dpt_info)))
    val deq = Vec(rnChn, Flipped(new DecoupledIO(new Dpt_info)))

    val molloc = Decoupled(new UInt(5.W)) //molloc at read op
    val readOp = Flipped(Vec( 32, Valid(UInt( (vParams.vlen).W )) ) )

    val csrfiles = Input(new CSR_Bundle)
    val isPndVStore = Input(Bool())

    val flush = Input(Bool())
  }

  val io: VecPreIssueIO = IO(new VecPreIssueIO)

  val vecDptInfo = Wire( new Dpt_info )

  val vpuExeInfo = Wire( Vec( vParams.vlen/8, new Dpt_info ) )
  val vlsExeInfo = Wire( Vec( vParams.vlen/8, new Dpt_info ) )

  val vop = Wire( Vec(3, UInt((vParams.vlen).W)) )
  val vsew = vecDptInfo.vAttach.get.vsew

  val preIssueBufDnxt = for( i <- 0 until vParams.vlen/8 ) yield { Wire(new Dpt_info) }
  val isBufValidDnxt = for( i <- 0 until vParams.vlen/8 ) yield { Wire(Bool()) }

  val preIssueBuf = for( i <- 0 until vParams.vlen/8 ) yield { Reg(new Dpt_info) }
  val isBufValid = for( i <- 0 until vParams.vlen/8 ) yield  { RegInit( false.B ) }

  val isBusy = isBufValid.reduce(_|_)

  val lmul  = vecDptInfo.vAttach.get.vlmul
  val lmulSel  = vecDptInfo.vAttach.get.mulSel
  val nf     = vecDptInfo.vAttach.get.nf
  val nfSel  = vecDptInfo.vAttach.get.nfSel
  val vWidth = vecDptInfo.vAttach.get.vWidth
  val vstart = csrfiles.vstart
  val vl     = csrfiles.vl
}




trait VecPreIssueReadOp{ this: VecPreIssueBase =>
  val idx = Seq( vecDptInfo.phy.vm0, vecDptInfo.phy.rs1, vecDptInfo.phy.rs2, vecDptInfo.phy.rs3 )

  vop(0) := io.readOp(idx(0)).bits

  vop(1) := Mux( vecDptInfo.vecIsa.isVector, io.readOp(idx(1)).bits, io.readOp(idx(3)).bits )
  vop(2) := io.readOp(idx(2)).bits

}

trait VecPreIssueVlsSpliter{ this: VecPreIssueBase =>


  for( i <- 0 until vParams.vlen/8 ){
    vlsExeInfo(i).lsuIsa    := vecDptInfo.lsuIsa
    vlsExeInfo(i).aluIsa    := 0.U.asTypeOf(new Alu_isa)
    vlsExeInfo(i).bruIsa    := 0.U.asTypeOf(new Bru_isa)
    vlsExeInfo(i).csrIsa    := 0.U.asTypeOf(new Csr_isa)
    vlsExeInfo(i).mulIsa    := 0.U.asTypeOf(new Mul_isa)
    vlsExeInfo(i).privil_isa := 0.U.asTypeOf(new Privil_isa)
    vlsExeInfo(i).fpuIsa    := 0.U.asTypeOf(new Fpu_isa)
    vlsExeInfo(i).vecIsa  := 0.U.asTypeOf(new vecIsa)

    vlsExeInfo(i).param.is_rvc  := false.B
    vlsExeInfo(i).param.pc      := DontCare
    vlsExeInfo(i).param.imm     := vecDptInfo.param.imm
    vlsExeInfo(i).param.rm      := vecDptInfo.param.rm

    vlsExeInfo(i).vAttach.get.nf     := vecDptInfo.vAttach.get.nf
    vlsExeInfo(i).vAttach.get.vm     := vecDptInfo.vAttach.get.vm

    vlsExeInfo(i).vAttach.get.lmulSel  := vecDptInfo.vAttach.get.lmulSel
    vlsExeInfo(i).vAttach.get.nfSel    := vecDptInfo.vAttach.get.nfSel
    vlsExeInfo(i).vAttach.get.widenSel := vecDptInfo.vAttach.get.widenSel

    vlsExeInfo(i).vAttach.get.vtype   := vecDptInfo.vAttach.get.vtype

    vlsExeInfo(i).vAttach.get.voffset := 
      (Mux1H(Seq(
        (nf === 0.U) -> ( i*1 ).U, (nf === 1.U) -> ( i*2 ).U,
        (nf === 2.U) -> ( i*3 ).U, (nf === 3.U) -> ( i*4 ).U,
        (nf === 4.U) -> ( i*5 ).U, (nf === 5.U) -> ( i*6 ).U,
        (nf === 6.U) -> ( i*7 ).U, (nf === 7.U) -> ( i*8 ).U,
      )) + nfSel) >> vWidth

    when( ori.fun.vle | iss.fun.vlm | iss.fun.vleNff | iss.fun.vleNff | iss.fun.vlNreN ){

      vlsExeInfo(i).param.raw.vm0 := DontCare
      vlsExeInfo(i).param.raw.rs1 := vecDptInfo.param.raw.rs1
      vlsExeInfo(i).param.raw.rs2 := DontCare
      vlsExeInfo(i).param.raw.rs3 := DontCare

      vlsExeInfo(i).vAttach.get.eleIdx := i.U

      vlsExeInfo(i).vAttach.get.vop0 := vop(0)(i).asBool
      vlsExeInfo(i).vAttach.get.vop1 := DontCare
      vlsExeInfo(i).vAttach.get.vop2 := DontCare



    }.elsewhen( iss.fun.vse | iss.fun.vsm | iss.fun.vsNr ){

      vlsExeInfo(i).param.raw.vm0 := DontCare
      vlsExeInfo(i).param.raw.rs1 := vecDptInfo.param.raw.rs1
      vlsExeInfo(i).param.raw.rs2 := DontCare
      vlsExeInfo(i).param.raw.rs3 := DontCare

      vlsExeInfo(i).vAttach.get.eleIdx := i.U

      vlsExeInfo(i).vAttach.get.vop0 := vop(0)(i).asBool
      vlsExeInfo(i).vAttach.get.vop1 := 
        Mux1H(Seq(
          (vsew === "b000".U) -> (vop(1) >> (i >> 3).U ).apply(7,0),
          (vsew === "b001".U) -> (vop(1) >> (i >> 4).U ).apply(15,0),
          (vsew === "b010".U) -> (vop(1) >> (i >> 5).U ).apply(31,0),
          (vsew === "b011".U) -> (vop(1) >> (i >> 6).U ).apply(63,0),
        ))
      vlsExeInfo(i).vAttach.get.vop2 := DontCare


    }.elsewhen( iss.fun.vlse ){

      vlsExeInfo(i).param.raw.vm0 := DontCare
      vlsExeInfo(i).param.raw.rs1 := vecDptInfo.param.raw.rs1
      vlsExeInfo(i).param.raw.rs2 := vecDptInfo.param.raw.rs2
      vlsExeInfo(i).param.raw.rs3 := DontCare

      vlsExeInfo(i).vAttach.get.eleIdx := i.U

      vlsExeInfo(i).vAttach.get.vop0 := vop(0)(i).asBool
      vlsExeInfo(i).vAttach.get.vop1 := DontCare
      vlsExeInfo(i).vAttach.get.vop2 := DontCare


    }.elsewhen( iss.fun.vsse ){

      vlsExeInfo(i).param.raw.vm0 := DontCare
      vlsExeInfo(i).param.raw.rs1 := vecDptInfo.param.raw.rs1
      vlsExeInfo(i).param.raw.rs2 := vecDptInfo.param.raw.rs2
      vlsExeInfo(i).param.raw.rs3 := DontCare

      vlsExeInfo(i).vAttach.get.eleIdx := i.U

      vlsExeInfo(i).vAttach.get.vop0 := vop(0)(i).asBool
      vlsExeInfo(i).vAttach.get.vop1 := 
        Mux1H(Seq(
          (vsew === "b000".U) -> (vop(1) >> (i >> 3).U ).apply(7,0),
          (vsew === "b001".U) -> (vop(1) >> (i >> 4).U ).apply(15,0),
          (vsew === "b010".U) -> (vop(1) >> (i >> 5).U ).apply(31,0),
          (vsew === "b011".U) -> (vop(1) >> (i >> 6).U ).apply(63,0),
        ))
      vlsExeInfo(i).vAttach.get.vop2 := DontCare

    }.elsewhen( iss.fun.vloxei ){

      vlsExeInfo(i).param.raw.vm0 := DontCare
      vlsExeInfo(i).param.raw.rs1 := vecDptInfo.param.raw.rs1
      vlsExeInfo(i).param.raw.rs2 := DontCare
      vlsExeInfo(i).param.raw.rs3 := DontCare

      vlsExeInfo(i).vAttach.get.eleIdx := i.U

      vlsExeInfo(i).vAttach.get.vop0 := vop(0)(i).asBool
      vlsExeInfo(i).vAttach.get.vop1 := DontCare
      vlsExeInfo(i).vAttach.get.vop2 := 
        Mux1H(Seq(
          (vsew === "b000".U) -> (vop(2) >> (i >> 3).U ).apply(7,0),
          (vsew === "b001".U) -> (vop(2) >> (i >> 4).U ).apply(15,0),
          (vsew === "b010".U) -> (vop(2) >> (i >> 5).U ).apply(31,0),
          (vsew === "b011".U) -> (vop(2) >> (i >> 6).U ).apply(63,0),
        ))

    }.elsewhen( iss.fun.vsoxei ){

      vlsExeInfo(i).param.raw.vm0 := DontCare
      vlsExeInfo(i).param.raw.rs1 := vecDptInfo.param.raw.rs1
      vlsExeInfo(i).param.raw.rs2 := DontCare
      vlsExeInfo(i).param.raw.rs3 := DontCare

      vlsExeInfo(i).vAttach.get.eleIdx := i.U

      vlsExeInfo(i).vAttach.get.vop0 := vop(0)(i).asBool
      vlsExeInfo(i).vAttach.get.vop1 := 
        Mux1H(Seq(
          (vsew === "b000".U) -> (vop(1) >> (i >> 3).U ).apply(7,0),
          (vsew === "b001".U) -> (vop(1) >> (i >> 4).U ).apply(15,0),
          (vsew === "b010".U) -> (vop(1) >> (i >> 5).U ).apply(31,0),
          (vsew === "b011".U) -> (vop(1) >> (i >> 6).U ).apply(63,0),
        ))
      vlsExeInfo(i).vAttach.get.vop2 :=
        Mux1H(Seq(
          (vsew === "b000".U) -> (vop(2) >> (i >> 3).U ).apply(7,0),
          (vsew === "b001".U) -> (vop(2) >> (i >> 4).U ).apply(15,0),
          (vsew === "b010".U) -> (vop(2) >> (i >> 5).U ).apply(31,0),
          (vsew === "b011".U) -> (vop(2) >> (i >> 6).U ).apply(63,0),
        ))

      
      sbInfo.splitInfo(i).fun.sb := ori.vAttach.get.vWidth === "b000".U
      sbInfo.splitInfo(i).fun.sh := ori.vAttach.get.vWidth === "b001".U
      sbInfo.splitInfo(i).fun.sw := ori.vAttach.get.vWidth === "b010".U
      sbInfo.splitInfo(i).fun.sd := ori.vAttach.get.vWidth === "b011".U
        
      sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + adjAddr( i, ori.vAttach.get.nf, ori.vAttach.get.vWidth ) + adjVSEle( ori.param.dat.op2, i, "b000".U, ori.vAttach.get.vsew ) 
      sbInfo.splitInfo(i).param.dat.op2 := adjVSEle( ori.param.dat.op3, i, ori.vAttach.get.nf, ori.vAttach.get.vsew )

      sbInfo.isExeReady := false.B



    }.otherwise{
      vpuExeInfo(i) := 0.U.asTypeOf(new Dpt_info)
    }

  }

}

trait VecPreIssueVpuSpliter{ this: VecPreIssueBase =>
  for( i <- 0 until vParams.vlen/8 ){
    vpuExeInfo(i) := 0.U.asTypeOf(new Dpt_info)
  }
}

trait VecPreIssueMux{ this: VecPreIssueBase =>

  val validMask = Wire( Vec( vParams.vlen/8, Vec( vParams.vlen/8, Bool() ) ))
  val deqSel    = Wire( Vec( vParams.vlen/8, UInt( log2Ceil(vParams.vlen/8).W )) )

  validMask(0) := isBufValid
  for( i <- 1 until vParams.vlen/8 ){
    validMask(i) := validMask(i-1) & ~(1.U((vParams.vlen/8).W) << deqSel(i-1))
  }

  for( i <- 0 until vParams.vlen/8 ){
    deqSel(i) := validMask.indexWhere((x: Bool) => (x === true.B))
  }



  val isVSTorePnd = RegInit(false.B)
  val isVStprePre  = RegNext(io.isPndVStore, false.B)
  val isVStprePost = io.isPndVStore & ~isVStprePre
  when( io.flush ) {
    isVSTorePnd := false.B
  } .elsewhen( isVStprePost ) {
    isVSTorePnd := true.B
  } .elsewhen(isVSTorePnd & ~isBusy) {
    isVSTorePnd := false.B
  }




  for( i <- 0 until rnChn ){
    when( isBusy ){
      io.deq(i).valid := isBufValid(deqSel(i))
      io.deq(i).bits  := preIssueBuf(deqSel(i))

      when( io.deq(i).fire ){
        assert(isBufValid(deqSel(i)) === true.B)
        isBufValid(deqSel(i)) := false.B
            preIssueBuf(deqSel(i)) := 0.U.asTypeOf(new Dpt_info)
      }


      io.molloc.valid := false.B
      io.molloc.bits  := DontCare
      io.enq(i).ready := false.B
    } .otherwise{
      when( (0 until i).map{ j => io.enq(j).bits.lsuIsa.isVector | io.enq(j).bits.vecIsa.isVALU }.foldLeft(false.B)(_|_) ){
        io.deq(i).valid := false.B
        io.deq(i).bits  := 0.U.asTypeOf(new IF4_Bundle)

        io.molloc.valid := false.B
        io.molloc.bits  := DontCare

        io.enq(i).ready := false.B
      } .otherwise{
        when( ~(io.enq(i).bits.lsuIsa.isVector | io.enq(i).bits.vecIsa.isVALU) ){
          io.enq(i) <> io.deq(i)
        } .otherwise{
          vecDptInfo := io.enq(i).bits

          io.deq(i).valid := false.B
          io.deq(i).bits  := 0.U.asTypeOf(new IF4_Bundle)

// when( io.enq(i).fire ) {

          for( j <- 0 until vParams.vlen/8 ){

            preIssueBufDnxt(j) := 
              Mux1H(Seq(
                io.enq(i).bits.lsuIsa.isVector -> vlsExeInfo(j),
                io.enq(i).bits.vecIsa.isVALU -> vpuExeInfo(j),
              ))
              
            isBufValidDnxt(j) := 
              Mux1H(Seq(
                (vsew === "b000".U) -> ( (j.U <  8.U) & Mux( vlsExeInfo(j).vAttach.vm === true.B, true.B, vop(0)(j) === 1.U )),
                (vsew === "b001".U) -> ( (j.U < 16.U) & Mux( vlsExeInfo(j).vAttach.vm === true.B, true.B, vop(0)(j) === 1.U )),
                (vsew === "b010".U) -> ( (j.U < 32.U) & Mux( vlsExeInfo(j).vAttach.vm === true.B, true.B, vop(0)(j) === 1.U )),
                (vsew === "b011".U) -> ( (j.U < 64.U) & Mux( vlsExeInfo(j).vAttach.vm === true.B, true.B, vop(0)(j) === 1.U )),
              ))      

            io.molloc.bits.isMask(j) :=
              Mux1H(Seq(
                (vsew === "b000".U) -> ( (j.U <  8.U) & Mux( vlsExeInfo(j).vAttach.vm === true.B, false.B, vop(0)(j) === 0.U )),
                (vsew === "b001".U) -> ( (j.U < 16.U) & Mux( vlsExeInfo(j).vAttach.vm === true.B, false.B, vop(0)(j) === 0.U )),
                (vsew === "b010".U) -> ( (j.U < 32.U) & Mux( vlsExeInfo(j).vAttach.vm === true.B, false.B, vop(0)(j) === 0.U )),
                (vsew === "b011".U) -> ( (j.U < 64.U) & Mux( vlsExeInfo(j).vAttach.vm === true.B, false.B, vop(0)(j) === 0.U )),
              ))      
          }
// }

          io.molloc.valid := 
            io.enq(i).valid &
            Mux( io.enq(i).bits.lsuIsa.isVStore,
                isVSTorePnd & (vstartSel+j.U >= vstart) & (vstartSel+j.U <= vl),
                true.B) &
            io.enq(i).bits.isVwb &
            Mux(io.enq(i).bits.isVM0, io.readOp(idx(0)).valid, true.B) &
            Mux(io.enq(i).bits.isVS1, io.readOp(idx(1)).valid, true.B) &
            Mux(io.enq(i).bits.isVS2, io.readOp(idx(2)).valid, true.B) &
            Mux(io.enq(i).bits.isVS3, io.readOp(idx(3)).valid, true.B)

          io.molloc.bits.idx    := io.enq(i).bits.param.rd0
          io.molloc.bits.vsew   := vsew(1,0)

          io.enq(i).ready := io.molloc.ready

          assert( io.enq(i).fire === io.molloc.fire )

          when( io.flush ){
            isBufValid(j) := false.B
          } .elsewhen(  io.enq(i).fire ){
            for( j <- 0 until vParams.vlen/8 ){
              preIssueBuf(j) := preIssueBufDnxt(j)            
              isBufValid(j)  := isBufValidDnxt(j)
            }
          }
        }
      }
    }
  }
}





class VecPreIssue()(implicit p: Parameters) extends VecPreIssueBase
with VecPreIssueMux
with VecPreIssueReadOp
with VecPreIssueVlsSpliter
with VecPreIssueVpuSpliter



class FakeVecPreIssue()(implicit p: Parameters) extends VecPreIssueBase{
  io.enq <> io.deq

  io.molloc.valid := false.B
  io.molloc.bits  := 0.U
}


