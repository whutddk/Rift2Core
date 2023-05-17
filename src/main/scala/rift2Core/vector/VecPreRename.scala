
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
import base._
import rift2Core.define._
import rift2Chip._
import rift2Core.privilege._
import org.chipsalliance.cde.config._





















abstract class VecPreRenameBase()(implicit p: Parameters) extends RiftModule {

  class VecPreRenameIO extends Bundle{
    val enq = Vec(rnChn, Flipped(new DecoupledIO(new IF4_Bundle)))
    val deq = Vec(rnChn, new DecoupledIO(new IF4_Bundle))

    val vpuCsrMolloc    = Vec(rnChn, Decoupled(Bool()))
    val csrfiles   = Input(new CSR_Bundle)


    val flush = Input(Bool())
  }

  val io: VecPreRenameIO = IO(new VecPreRenameIO)

  val vecSplitFifo = Module(new MultiPortFifo( new Dpt_info, aw = 3, 8, 1 ) )

  val vecSplitReq = WireDefault(0.U.asTypeof(new IF4_Bundle))

  val vtype   = io.csrfiles.vtype
  val lmul    = vtype(2,0)
  val vsew    = vtype(5,3)
  val nf      = vSplitReq.vAttach.get.nf

  val isVALU  = vSplitReq.vecIsa.isVALU
  val isVLSU  = vSplitReq.lsuIsa.isVector

  val isWiden = vSplitReq.vecIsa.isVS2P | vSplitReq.vecIsa.is2Malloc

}


trait VecPreRenameMux{ this: VecPreRenameBase =>

  for( i <- 0 until rnChn ){

    when( vecSplitFifo.io.deq(0).valid ){
      io.deq(i) <> vecSplitFifo.io.deq(i)

      io.enq(i).ready := false.B
    } .otherwise{
      when( (0 until i).map{ j => io.enq(j).bits.lsuIsa.isVector | io.enq(j).bits.vecIsa.isVALU }.foldLeft(false.B)(_|_) ){
        io.deq(i).valid := false.B
        io.deq(i).bits  := 0.U.asTypeOf(new IF4_Bundle)
        io.enq(i).ready := false.B
      } .otherwise{
        when( ~(io.enq(i).bits.lsuIsa.isVector | io.enq(i).bits.vecIsa.isVALU) ){
          io.enq(i) <> io.deq(i)
        } .otherwise{
          vecSplitReq := io.enq(i).bits
          when( nf === 0.U & lmul(1,0) === 0.U & ~isWiden ){ //dont splitter
            io.enq(i) <> io.deq(i)
          }.otherwise{ //splitter
            io.deq(i).valid := false.B
            io.deq(i).bits  := 0.U.asTypeOf(new IF4_Bundle)
            io.enq(i).ready := true.B
          }
        }
      }
    }
  }
}






trait VecPreRenameMicroInstr{ this: VecPreRenameBase =>
      



  io.vSplieRsp.valid := false.B
  io.vSplieRsp.bits  := 0.U.asTypeOf(new Dpt_info)

  val microInstrCnt = 
    Mux1H(Seq(
      ( isVALU   ) -> (lmul(1,0)),
      ( isVLSU   ) -> ((nf+1.U) << lmul(1,0)),
      ( isWiden  ) -> ( 2.U     << lmul(1,0)),
    ))

  val microInstr = Wire( Vec( 8, new IF4_Bundle ) )
  val lmulSel    = Wire( Vec( 8, UInt(3.W)) )
  val widenSel   = Wire( Vec( 8, UInt(3.W)) )

  for( i <- 0 until 8 ) {

    when( isVLSU ){
      lmulSel(i) := Mux1H(Seq(
        (nf === "b000".U) -> (i/1).U, (nf === "b001".U) -> (i/2).U, //nf = 2
        (nf === "b010".U) -> (i/3).U, (nf === "b011".U) -> (i/4).U, //nf = 4
        (nf === "b100".U) -> 0.U,     (nf === "b101".U) -> 0.U,
        (nf === "b110".U) -> 0.U,     (nf === "b111".U) -> 0.U,
      ))
    }.elsewhen( isWiden ){
      lmulSel := (i/2).U
    }.otherwise{
      lmulSel := (i/1).U
    }

    widenSel(i) := Mux( isWiden, (i%2).U, 0.U )

    microInstr(i).aluIsa    := 0.U.asTypeOf( new Alu_isa )
    microInstr(i).bruIsa    := 0.U.asTypeOf( new Bru_isa )
    microInstr(i).lsuIsa    := vSplitReq.lsuIsa
    microInstr(i).csrIsa    := 0.U.asTypeOf( new Csr_isa )
    microInstr(i).mulIsa    := 0.U.asTypeOf( new Mul_isa )
    microInstr(i).privil_isa := 0.U.asTypeOf( new Privil_isa )
    microInstr(i).fpuIsa    := 0.U.asTypeOf( new Fpu_isa )
    microInstr(i).vecIsa  := vSplitReq.vecIsa

    microInstr(i).param.is_rvc := false.B
    microInstr(i).param.pc  := vSplitReq.param.pc
    microInstr(i).param.imm := 0.U
    microInstr(i).param.rm  := vSplitReq.param.rm

    microInstr(i).vAttach.get.vm  := vSplitReq.vAttach.get.vm
    microInstr(i).vAttach.get.nf  := nf

    microInstr(i).vAttach.get.lmulSel := lmulSel(i)

    microInstr(i).vAttach.get.nfSel := 
      Mux1H(Seq(
        (nf === "b000".U) -> (i%1).U, (nf === "b001".U) -> (i%2).U,
        (nf === "b010".U) -> (i%3).U, (nf === "b011".U) -> (i%4).U,
        (nf === "b100".U) -> i.U,     (nf === "b101".U) -> i.U,
        (nf === "b110".U) -> i.U,     (nf === "b111".U) -> i.U,
      ))


    microInstr(i).vAttach.get.widenSel := widenSel(i)

    microInstr(i).vAttach.get.microIdx := i.U

    microInstr(i).vAttach.get.vstartSel := 
      Mux1H(Seq(
        vsew === "b000".U -> (i*vParams.vlen / 8).U,
        vsew === "b001".U -> (i*vParams.vlen / 16).U,
        vsew === "b010".U -> (i*vParams.vlen / 32).U,
        vsew === "b011".U -> (i*vParams.vlen / 64).U,
      ))


  // def microIdx = isWiden * lmulSel + widenSel

    microInstr(i).param.raw.rs1 := vSplitReq.param.raw.rs1 + Mux( vSplitReq.vecIsa.isVS1, lmulSel(i), 0.U)

    microInstr(i).param.raw.rs2 :=
      Mux1H(Seq(
        ( vSplitReq.vecIsa.isVS2 &  vSplitReq.vecIsa.isVS2P ) -> ( vSplitReq.param.raw.rs2 + (lmulSel(i) << 1) + widenSel(i) ),
        ( vSplitReq.vecIsa.isVS2 & ~vSplitReq.vecIsa.isVS2P ) -> ( vSplitReq.param.raw.rs2 +  lmulSel(i) ),
        ( vSplitReq.lsuIsa.isVS2                                 ) -> ( vSplitReq.param.raw.rs2 +  lmulSel(i) ), //nf didn't effect vs2 in lsu
        ( vSplitReq.isRS2                                         ) -> ( vSplitReq.param.raw.rs2 ),
      ))

    microInstr(i).param.raw.rs3 := 
      Mux1H( Seq(
        ( vSplitReq.vecIsa.isVwb &  vSplitReq.vecIsa.is2Malloc ) -> (vSplitReq.param.raw.rd0 + (lmulSel(i) << 1) + widenSel(i)),
        ( vSplitReq.vecIsa.isVwb & ~vSplitReq.vecIsa.is2Malloc ) -> (vSplitReq.param.raw.rd0 +  lmulSel(i) ),
        ( vSplitReq.lsuIsa.isVwb                                    ) -> (vSplitReq.param.raw.rd0 +  lmulSel(i) ),
        ( vSplitReq.lsuIsa.isVS3                                    ) -> (vSplitReq.param.raw.rs3), //VSTORE
      ))

    microInstr(i).param.raw.rd0 := 
      Mux1H( Seq(
        ( vSplitReq.vecIsa.isVwb &  vSplitReq.vecIsa.is2Malloc ) -> (vSplitReq.param.raw.rd0 + (lmulSel(i) << 1) + widenSel(i) ),
        ( vSplitReq.vecIsa.isVwb & ~vSplitReq.vecIsa.is2Malloc ) -> (vSplitReq.param.raw.rd0 +  lmulSel(i) ),
        ( vSplitReq.lsuIsa.isVLoad                                  ) -> (vSplitReq.param.raw.rd0 +  lmulSel(i) ),
        ( vSplitReq.lsuIsa.isVStore                                 ) -> 0.U,
      ))

    microInstr(i).vAttach.get.isLast    := (i+1).U === Cal_MicroInstr_Cnt
    microInstr(i).vAttach.get.vstart    := DontCare
    microInstr(i).vAttach.get.vl        := DontCare
    microInstr(i).vAttach.get.vtype     := vtype
    microInstr(i).vAttach.get.vstartSel := DontCare
  }

}



class VecPreRename()(implicit p: Parameters) extends VecPreRenameBase with VecPreRenameMux with VecPreRenameMicroInstr{

}

class FakeVecPreRename()(implicit p: Parameters) extends VecPreRenameBase{
  io.enq <> io.deq

  io.vpuCsrMolloc.valid := false.B
  io.vpuCsrMolloc.bits  := false.B
}
