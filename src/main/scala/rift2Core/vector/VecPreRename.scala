
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















class VecPreRenameIO()(implicit p: Parameters) extends RiftBundle{
  val enq = Vec(rnChn, Flipped(new DecoupledIO(new IF4_Bundle)))
  val deq = Vec(rnChn, new DecoupledIO(new IF4_Bundle))

  val vpuCsrMolloc    = Vec(rnChn, Decoupled(Bool()))
  val csrfiles   = Input(new CSR_Bundle)


  val flush = Input(Bool())
}





abstract class VecPreRenameBase()(implicit p: Parameters) extends RiftModule {
  val io: VecPreRenameIO = IO(new VecPreRenameIO)

  val vecSplitFifo = Module(new MultiPortFifo( new IF4_Bundle, aw = 3, 8, rnChn ) )

  val vSplitReq = WireDefault(0.U.asTypeOf(new IF4_Bundle))

  val vtype   = io.csrfiles.vConfig.vtype
  val lmul    = vtype(2,0)
  val vsew    = vtype(5,3)
  val nf      = (if( hasVector ) {vSplitReq.vAttach.get.nf} else {0.U})

  val isVALU  = vSplitReq.vecIsa.isVALU
  val isVLSU  = vSplitReq.lsuIsa.isVector

  val isWiden = vSplitReq.vecIsa.isVS2P | vSplitReq.vecIsa.is2Malloc

}


trait VecPreRenameMux{ this: VecPreRenameBase =>

  for( i <- 0 until rnChn ){

    when( vecSplitFifo.io.deq(0).valid ){
      io.deq(i) <> vecSplitFifo.io.deq(i)
      io.enq(i).ready := false.B
      vSplitReq := 0.U.asTypeOf(new IF4_Bundle)
    } .otherwise{
      vecSplitFifo.io.deq(i).ready := false.B

      when( (0 until i).map{ j => io.enq(j).bits.lsuIsa.isVector | io.enq(j).bits.vecIsa.isVALU }.foldLeft(false.B)(_|_) ){
        io.deq(i).valid := false.B
        io.deq(i).bits  := 0.U.asTypeOf(new IF4_Bundle)
        io.enq(i).ready := false.B
        vSplitReq := 0.U.asTypeOf(new IF4_Bundle)
      } .otherwise{
        when( ~(io.enq(i).bits.lsuIsa.isVector | io.enq(i).bits.vecIsa.isVALU) ){
          io.enq(i) <> io.deq(i)
        } .otherwise{
          vSplitReq := io.enq(i).bits
          when( nf === 0.U & lmul(1,0) === 0.U & ~isWiden ){ //dont splitter
            io.enq(i) <> io.deq(i)
          }.otherwise{ //splitter
            io.deq(i).valid := false.B
            io.deq(i).bits  := 0.U.asTypeOf(new IF4_Bundle)
            io.enq(i).ready := true.B

            assert( io.enq(i).fire === vecSplitFifo.io.enq(0).fire )
          }
        }
      }
    }
  }
}






trait VecPreRenameMicroInstr{ this: VecPreRenameBase =>
      


  val microInstrCnt = 
    Mux1H(Seq(
      ( isVALU   ) -> Mux(isWiden, 2.U << lmul(1,0), (lmul(1,0))),
      ( isVLSU   ) -> ((nf+1.U) << lmul(1,0)),
    ))

  val microInstr = Wire( Vec( 8, new IF4_Bundle ) )
  val lmulSel    = Wire( Vec( 8, UInt(3.W)) )
  val widenSel   = Wire( Vec( 8, UInt(3.W)) )

  val fifoReq = 
    (0 until rnChn).map{ i => io.enq(i).valid & (io.enq(i).bits.lsuIsa.isVector | io.enq(i).bits.vecIsa.isVALU) & ~(nf === 0.U & lmul(1,0) === 0.U & ~isWiden) }.foldLeft(false.B)(_|_)


  for( i <- 0 until 8 ) {
    vecSplitFifo.io.enq(i).valid := fifoReq & (i.U <= microInstrCnt)
    vecSplitFifo.io.enq(i).bits  := microInstr(i)
    assert( ~(vecSplitFifo.io.enq(i).valid & ~vecSplitFifo.io.enq(i).ready) )

    when( isVLSU ){
      lmulSel(i) := Mux1H(Seq(
        (nf === "b000".U) -> (i/1).U, (nf === "b001".U) -> (i/2).U, //nf = 2
        (nf === "b010".U) -> (i/3).U, (nf === "b011".U) -> (i/4).U, //nf = 4
        (nf === "b100".U) -> 0.U,     (nf === "b101".U) -> 0.U,
        (nf === "b110".U) -> 0.U,     (nf === "b111".U) -> 0.U,
      ))
    }.elsewhen( isWiden ){
      lmulSel(i) := (i/2).U
    }.otherwise{
      lmulSel(i) := (i/1).U
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

    


  // def microIdx = isWiden * lmulSel + widenSel

    microInstr(i).param.raw.vm0 := vSplitReq.param.raw.vm0
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

    microInstr(i).vAttach.get.vm        := vSplitReq.vAttach.get.vm
    microInstr(i).vAttach.get.nf        := nf
    microInstr(i).vAttach.get.vtype     := vtype
    microInstr(i).vAttach.get.vstartSel := 
      Mux1H(Seq(
        ( vsew === "b000".U ) -> (i*vParams.vlen /  8).U,
        ( vsew === "b001".U ) -> (i*vParams.vlen / 16).U,
        ( vsew === "b010".U ) -> (i*vParams.vlen / 32).U,
        ( vsew === "b011".U ) -> (i*vParams.vlen / 64).U,
      ))
    microInstr(i).vAttach.get.lmulSel   := lmulSel(i)
    microInstr(i).vAttach.get.nfSel     := 
      Mux1H(Seq(
        (nf === "b000".U) -> (i%1).U, (nf === "b001".U) -> (i%2).U,
        (nf === "b010".U) -> (i%3).U, (nf === "b011".U) -> (i%4).U,
        (nf === "b100".U) -> i.U,     (nf === "b101".U) -> i.U,
        (nf === "b110".U) -> i.U,     (nf === "b111".U) -> i.U,
      ))
    microInstr(i).vAttach.get.widenSel  := widenSel(i)
    microInstr(i).vAttach.get.microIdx  := i.U
    microInstr(i).vAttach.get.vlCnt     := ((vParams.vlen/8).U) >> lmulSel(i)(1,0); assert( lmulSel(i).extract(2) === 0.U )
    microInstr(i).vAttach.get.eleIdx    := 0.U
    microInstr(i).vAttach.get.vop0      := false.B
    microInstr(i).vAttach.get.vop1      := 0.U
    microInstr(i).vAttach.get.vop2      := 0.U
    microInstr(i).vAttach.get.voffset   := 0.U 


  }

}



class VecPreRename()(implicit p: Parameters) extends VecPreRenameBase with VecPreRenameMux with VecPreRenameMicroInstr{
  for( i <- 0 until rnChn ) {
    io.vpuCsrMolloc(i).valid := false.B
    io.vpuCsrMolloc(i).bits  := false.B    
  }

  vecSplitFifo.io.flush := io.flush
}

class FakeVecPreRename()(implicit p: Parameters) extends RiftModule{
  val io: VecPreRenameIO = IO(new VecPreRenameIO)
  io.enq <> io.deq

  for( i <- 0 until rnChn ) {
    io.vpuCsrMolloc(i).valid := false.B
    io.vpuCsrMolloc(i).bits  := false.B    
  }

}
