
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


class VecPreIssueIO()(implicit p: Parameters) extends RiftBundle{
  val enq = Vec(rnChn, Flipped(new DecoupledIO(new Dpt_info)))
  val deq = Vec(rnChn, new DecoupledIO(new Dpt_info))

  val molloc = Decoupled(new Vector_Molloc_Bundle) //molloc at read op
  val readOp = Flipped(Vec( 32, Valid(UInt( (vParams.vlen).W )) ) )

  val csrfiles = Input(new CSR_Bundle)
  val isPndVStore = Input(Bool())

  val flush = Input(Bool())
}

abstract class VecPreIssueBase()(implicit p: Parameters) extends RiftModule{
  val io: VecPreIssueIO = IO(new VecPreIssueIO)

  val vecDptInfo = Wire( new Dpt_info )

  val vpuExeInfo = Wire( Vec( vParams.vlen/8, new Dpt_info ) )
  val vlsExeInfo = Wire( Vec( vParams.vlen/8, new Dpt_info ) )

  val vop = Wire( Vec(4, UInt((vParams.vlen).W)) )

  // val preIssueBufDnxt =  Wire( Vec( vParams.vlen/8, new Dpt_info))
  // val isBufValidDnxt  = Wire(Vec( vParams.vlen/8, Bool()) )

  val preIssueBuf = Reg( Vec( vParams.vlen/8, new Dpt_info) )
  val isBufValid  = RegInit( VecInit( Seq.fill(vParams.vlen/8 ){false.B} ))

  val isBusy = isBufValid.reduce(_|_)


  val vWidth = vecDptInfo.param.vWidth
  val vstart = io.csrfiles.vstart
  val vl     = io.csrfiles.vConfig.vl  //pervious vl should be committed

  val vsew     = (if(hasVector) {vecDptInfo.vAttach.get.vsew}     else {0.U})
  val lmul     = (if(hasVector) {vecDptInfo.vAttach.get.vlmul}    else {0.U})
  val lmulSel  = (if(hasVector) {vecDptInfo.vAttach.get.lmulSel}  else {0.U})
  val nf       = (if(hasVector) {vecDptInfo.vAttach.get.nf}       else {0.U})
  val nfSel    = (if(hasVector) {vecDptInfo.vAttach.get.nfSel}    else {0.U})
  val microIdx = (if(hasVector) {vecDptInfo.vAttach.get.microIdx} else {0.U})

  val psew     = WireDefault( 0.U(2.W) )
}




trait VecPreIssueReadOp{ this: VecPreIssueBase =>
  val idx = Seq( vecDptInfo.param.raw.vm0, vecDptInfo.param.raw.rs1, vecDptInfo.param.raw.rs2, vecDptInfo.param.raw.rs3 )

  vop(0) := io.readOp(idx(0)).bits >> ( microIdx << (psew + 3.U) )

  vop(1) := io.readOp(idx(1)).bits
  vop(2) := io.readOp(idx(2)).bits
  vop(3) := io.readOp(idx(3)).bits

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
    vlsExeInfo(i).vecIsa  := 0.U.asTypeOf(new VecIsa)


    vlsExeInfo(i).param   := vecDptInfo.param
    vlsExeInfo(i).phy     := vecDptInfo.phy
    vlsExeInfo(i).vAttach.get     := vecDptInfo.vAttach.get


    vlsExeInfo(i).vAttach.get.vlIdx :=  //override
      vecDptInfo.vAttach.get.vlIdx + i.U
          // (Mux1H(Seq(
          //   (nf === 0.U) -> ( i*1 ).U, (nf === 1.U) -> ( i*2 ).U,
          //   (nf === 2.U) -> ( i*3 ).U, (nf === 3.U) -> ( i*4 ).U,
          //   (nf === 4.U) -> ( i*5 ).U, (nf === 5.U) -> ( i*6 ).U,
          //   (nf === 6.U) -> ( i*7 ).U, (nf === 7.U) -> ( i*8 ).U,
          // )) + nfSel) >> vWidth







    when( vecDptInfo.lsuIsa.vle | vecDptInfo.lsuIsa.vlm | vecDptInfo.lsuIsa.vleNff | vecDptInfo.lsuIsa.vleNff | vecDptInfo.lsuIsa.vlre ){
      psew := Mux1H(Seq(
        (vecDptInfo.lsuIsa.isByte) -> "b00".U,
        (vecDptInfo.lsuIsa.isHalf) -> "b01".U,
        (vecDptInfo.lsuIsa.isWord) -> "b10".U,
        (vecDptInfo.lsuIsa.isDubl) -> "b11".U,
      ))

      vlsExeInfo(i).param.raw.vm0 := DontCare
      vlsExeInfo(i).param.raw.rs1 := vecDptInfo.param.raw.rs1 //override
      vlsExeInfo(i).param.raw.rs2 := DontCare
      vlsExeInfo(i).param.raw.rs3 := DontCare

      vlsExeInfo(i).vAttach.get.eleIdx := i.U

      vlsExeInfo(i).vAttach.get.vop0 := vop(0)(i).asBool
      vlsExeInfo(i).vAttach.get.vop1 := DontCare
      vlsExeInfo(i).vAttach.get.vop2 := DontCare



    }.elsewhen( vecDptInfo.lsuIsa.vse | vecDptInfo.lsuIsa.vsm | vecDptInfo.lsuIsa.vsr ){
      psew := Mux1H(Seq(
        (vecDptInfo.lsuIsa.isByte) -> "b00".U,
        (vecDptInfo.lsuIsa.isHalf) -> "b01".U,
        (vecDptInfo.lsuIsa.isWord) -> "b10".U,
        (vecDptInfo.lsuIsa.isDubl) -> "b11".U,
      ))

      vlsExeInfo(i).param.raw.vm0 := DontCare
      vlsExeInfo(i).param.raw.rs1 := vecDptInfo.param.raw.rs1
      vlsExeInfo(i).param.raw.rs2 := DontCare
      vlsExeInfo(i).param.raw.rs3 := DontCare

      vlsExeInfo(i).vAttach.get.eleIdx := i.U

      vlsExeInfo(i).vAttach.get.vop0 := vop(0)(i).asBool
      vlsExeInfo(i).vAttach.get.vop1 := 
        Mux1H(Seq(
          (psew === "b00".U) -> (vop(3) >> (i << 3).U ).apply(7,0),
          (psew === "b01".U) -> (vop(3) >> (i << 4).U ).apply(15,0),
          (psew === "b10".U) -> (vop(3) >> (i << 5).U ).apply(31,0),
          (psew === "b11".U) -> (vop(3) >> (i << 6).U ).apply(63,0),
        ))
      vlsExeInfo(i).vAttach.get.vop2 := DontCare


    }.elsewhen( vecDptInfo.lsuIsa.vlse ){
      psew := Mux1H(Seq(
        (vecDptInfo.lsuIsa.isByte) -> "b00".U,
        (vecDptInfo.lsuIsa.isHalf) -> "b01".U,
        (vecDptInfo.lsuIsa.isWord) -> "b10".U,
        (vecDptInfo.lsuIsa.isDubl) -> "b11".U,
      ))

      vlsExeInfo(i).param.raw.vm0 := DontCare
      vlsExeInfo(i).param.raw.rs1 := vecDptInfo.param.raw.rs1
      vlsExeInfo(i).param.raw.rs2 := vecDptInfo.param.raw.rs2
      vlsExeInfo(i).param.raw.rs3 := DontCare

      vlsExeInfo(i).vAttach.get.eleIdx := i.U

      vlsExeInfo(i).vAttach.get.vop0 := vop(0)(i).asBool
      vlsExeInfo(i).vAttach.get.vop1 := DontCare
      vlsExeInfo(i).vAttach.get.vop2 := DontCare


    }.elsewhen( vecDptInfo.lsuIsa.vsse ){
      psew := Mux1H(Seq(
        (vecDptInfo.lsuIsa.isByte) -> "b00".U,
        (vecDptInfo.lsuIsa.isHalf) -> "b01".U,
        (vecDptInfo.lsuIsa.isWord) -> "b10".U,
        (vecDptInfo.lsuIsa.isDubl) -> "b11".U,
      ))

      vlsExeInfo(i).param.raw.vm0 := DontCare
      vlsExeInfo(i).param.raw.rs1 := vecDptInfo.param.raw.rs1
      vlsExeInfo(i).param.raw.rs2 := vecDptInfo.param.raw.rs2
      vlsExeInfo(i).param.raw.rs3 := DontCare

      vlsExeInfo(i).vAttach.get.eleIdx := i.U

      vlsExeInfo(i).vAttach.get.vop0 := vop(0)(i).asBool
      vlsExeInfo(i).vAttach.get.vop1 := 
        Mux1H(Seq(
          (psew === "b00".U) -> (vop(3) >> (i << 3).U ).apply(7,0),
          (psew === "b01".U) -> (vop(3) >> (i << 4).U ).apply(15,0),
          (psew === "b10".U) -> (vop(3) >> (i << 5).U ).apply(31,0),
          (psew === "b11".U) -> (vop(3) >> (i << 6).U ).apply(63,0),
        ))
      vlsExeInfo(i).vAttach.get.vop2 := DontCare

    }.elsewhen( vecDptInfo.lsuIsa.vloxei ){
      psew := vsew

      vlsExeInfo(i).param.raw.vm0 := DontCare
      vlsExeInfo(i).param.raw.rs1 := vecDptInfo.param.raw.rs1
      vlsExeInfo(i).param.raw.rs2 := DontCare
      vlsExeInfo(i).param.raw.rs3 := DontCare

      vlsExeInfo(i).vAttach.get.eleIdx := i.U

      vlsExeInfo(i).vAttach.get.vop0 := vop(0)(i).asBool
      vlsExeInfo(i).vAttach.get.vop1 := DontCare
      vlsExeInfo(i).vAttach.get.vop2 := 
        Mux1H(Seq(
          (vecDptInfo.lsuIsa.vsoxei8 ) -> (vop(2) >> (i << 3).U ).apply(7,0),
          (vecDptInfo.lsuIsa.vsoxei16) -> (vop(2) >> (i << 4).U ).apply(15,0),
          (vecDptInfo.lsuIsa.vsoxei32) -> (vop(2) >> (i << 5).U ).apply(31,0),
          (vecDptInfo.lsuIsa.vsoxei64) -> (vop(2) >> (i << 6).U ).apply(63,0),
        ))

    }.elsewhen( vecDptInfo.lsuIsa.vsoxei ){
      psew := vsew

      vlsExeInfo(i).param.raw.vm0 := DontCare
      vlsExeInfo(i).param.raw.rs1 := vecDptInfo.param.raw.rs1
      vlsExeInfo(i).param.raw.rs2 := DontCare
      vlsExeInfo(i).param.raw.rs3 := DontCare

      vlsExeInfo(i).vAttach.get.eleIdx := i.U

      vlsExeInfo(i).vAttach.get.vop0 := vop(0)(i).asBool
      vlsExeInfo(i).vAttach.get.vop1 := 
        Mux1H(Seq(
          (psew === "b00".U) -> (vop(3) >> (i << 3).U ).apply(7,0),
          (psew === "b01".U) -> (vop(3) >> (i << 4).U ).apply(15,0),
          (psew === "b10".U) -> (vop(3) >> (i << 5).U ).apply(31,0),
          (psew === "b11".U) -> (vop(3) >> (i << 6).U ).apply(63,0),
        ))
      vlsExeInfo(i).vAttach.get.vop2 :=
        Mux1H(Seq(
          (vecDptInfo.lsuIsa.vsoxei8 ) -> (vop(2) >> (i << 3).U ).apply(7,0),
          (vecDptInfo.lsuIsa.vsoxei16) -> (vop(2) >> (i << 4).U ).apply(15,0),
          (vecDptInfo.lsuIsa.vsoxei32) -> (vop(2) >> (i << 5).U ).apply(31,0),
          (vecDptInfo.lsuIsa.vsoxei64) -> (vop(2) >> (i << 6).U ).apply(63,0),
        ))


    }.otherwise{
      vlsExeInfo(i) := 0.U.asTypeOf(new Dpt_info)
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
    validMask(i) := validMask(i-1)
    // validMask(i) := validMask(i-1) & ~(1.U((vParams.vlen/8).W) << deqSel(i-1))
    validMask(i)( deqSel(i-1) ) := false.B
  }

  for( i <- 0 until vParams.vlen/8 ){
    deqSel(i) := validMask(i).indexWhere((x: Bool) => (x === true.B))
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



  io.molloc.valid := false.B
  io.molloc.bits  := 0.U.asTypeOf(new Vector_Molloc_Bundle)
  vecDptInfo := 0.U.asTypeOf(new Dpt_info)

  for( i <- 0 until rnChn ){
    when( isBusy ){
      io.deq(i).valid := isBufValid(deqSel(i))
      io.deq(i).bits  := preIssueBuf(deqSel(i))

      when( io.deq(i).fire ){
        assert(isBufValid(deqSel(i)) === true.B)
        isBufValid(deqSel(i)) := false.B
        preIssueBuf(deqSel(i)) := 0.U.asTypeOf(new Dpt_info)
      }

      io.enq(i).ready := false.B

    } .otherwise{
      when( (0 until i).map{ j => io.enq(j).bits.lsuIsa.isVector | io.enq(j).bits.vecIsa.isVALU }.foldLeft(false.B)(_|_) ){
        io.deq(i).valid := false.B
        io.deq(i).bits  := 0.U.asTypeOf(new Dpt_info)

        io.enq(i).ready := false.B

      } .otherwise{
        when( ~(io.enq(i).bits.lsuIsa.isVector | io.enq(i).bits.vecIsa.isVALU) ){
          io.enq(i) <> io.deq(i)
        } .otherwise{
          val idx = Seq( vecDptInfo.phy.vm0, vecDptInfo.phy.rs1, vecDptInfo.phy.rs2, vecDptInfo.phy.rs3 )

          vecDptInfo := io.enq(i).bits

          io.deq(i).valid := false.B
          io.deq(i).bits  := 0.U.asTypeOf(new Dpt_info)

          when( io.enq(i).fire ) {

            for( j <- 0 until vParams.vlen/8 ){

              preIssueBuf(j) := 
                Mux1H(Seq(
                  io.enq(i).bits.lsuIsa.isVector -> vlsExeInfo(j),
                  io.enq(i).bits.vecIsa.isVALU -> vpuExeInfo(j),
                ))
                
              isBufValid(j) := 
                Mux1H(Seq(
                  (psew === "b00".U) -> ( j.U < (vParams.vlen/ 8).U ),
                  (psew === "b01".U) -> ( j.U < (vParams.vlen/16).U ),
                  (psew === "b10".U) -> ( j.U < (vParams.vlen/32).U ),
                  (psew === "b11".U) -> ( j.U < (vParams.vlen/64).U ),
                )) & 
                (io.enq(i).bits.vAttach.get.vlIdx + j.U) < vl &
                Mux( vlsExeInfo(j).vAttach.get.vm === true.B, true.B, vop(0)(j) === 1.U )
            }
          }



          /**
            * @note a no-body elements microInstruction will also molloc, but it will be marked as writeback in next cycle, and be committed then
            */
          io.molloc.valid := 
            io.enq(i).valid &
            Mux( io.enq(i).bits.lsuIsa.isVStore, isVSTorePnd, true.B) &
            io.enq(i).bits.isVwb //& //we should molloc a vector register
            // (io.enq(i).bits.vAttach.get.vlIdx >= vstart) & (io.enq(i).bits.vAttach.get.vlIdx < vl)

                      // Mux(io.enq(i).bits.isVM0, io.readOp(idx(0)).valid, true.B) &
                      // Mux(io.enq(i).bits.isVS1, io.readOp(idx(1)).valid, true.B) &
                      // Mux(io.enq(i).bits.isVS2, io.readOp(idx(2)).valid, true.B) &
                      // Mux(io.enq(i).bits.isVS3, io.readOp(idx(3)).valid, true.B)

          for( j <- 0 until vParams.vlen/8 ){
            io.molloc.bits.isMask(j) :=
              Mux1H(Seq(
                (psew === "b000".U) -> ( j.U >= (vParams.vlen/ 8).U ),
                (psew === "b001".U) -> ( j.U >= (vParams.vlen/16).U ),
                (psew === "b010".U) -> ( j.U >= (vParams.vlen/32).U ),
                (psew === "b011".U) -> ( j.U >= (vParams.vlen/64).U ),
              )) |
            Mux( vlsExeInfo(j).vAttach.get.vm === true.B, false.B, vop(0)(j) === 0.U ) |
            (io.enq(i).bits.vAttach.get.vlIdx + j.U) >= vl |
            (io.enq(i).bits.vAttach.get.vlIdx + j.U) < vstart
          }

          io.molloc.bits.idx    := io.enq(i).bits.phy.rd0
          io.molloc.bits.vsew   := psew(1,0)

          io.enq(i).ready :=
            io.molloc.ready &
            Mux( io.enq(i).bits.lsuIsa.isVStore, isVSTorePnd, true.B)// &
            // (io.enq(i).bits.vAttach.get.vlIdx >= vstart) & (io.enq(i).bits.vAttach.get.vlIdx < vl)


          when(io.enq(i).bits.isVwb) {
            assert( io.enq(i).fire === io.molloc.fire )
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



class FakeVecPreIssue()(implicit p: Parameters) extends RiftModule{
  val io: VecPreIssueIO = IO(new VecPreIssueIO)

  io.enq <> io.deq

  io.molloc.valid := false.B
  io.molloc.bits  := 0.U.asTypeOf(new Vector_Molloc_Bundle)
}


