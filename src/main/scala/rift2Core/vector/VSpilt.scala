
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





















abstract class VecSplitterBase()(implicit p: Parameters) extends RiftModule {

  class VecSplitterIO extends Bundle{
    val enq = Vec(rnChn, Flipped(new DecoupledIO(new IF4_Bundle)))
    val deq = Vec(rnChn, new DecoupledIO(new IF4_Bundle))

    val vpuCsrMolloc    = Vec(rnChn, Decoupled(Bool()))
    val csrfiles   = Input(new CSR_Bundle)


    val flush = Input(Bool())
  }

  val io: VecSplitterIO = IO(new VecSplitterIO)

  val vecSplitFifo = Module(new MultiPortFifo( new Dpt_info, aw = 3, 8, 1 ) )

  val vecSplitReq = WireDefault(0.U.asTypeof(new IF4_Bundle))

  val vtype   = io.csrfiles.vtype
  val lmul    = vtype(2,0)
  val nf      = vSplitReq.vAttach.get.nf

  val isVALU  = vSplitReq.vectorIsa.isVALU
  val isVLSU  = vSplitReq.lsu_isa.isVector

  val isWiden = vSplitReq.vectorIsa.isVS2P | vSplitReq.vectorIsa.is2Malloc

}


trait VecSplitterMux{ this: VecSplitterBase =>

  for( i <- 0 until rnChn ){

    when( vecSplitFifo.io.deq(0).valid ){
      io.deq(i).valid := false.B
      io.deq(i).bits  := 0.U.asTypeOf(new IF4_Bundle)
      io.enq(i).ready := false.B
    } .otherwise{
      when( (0 until i).map{ j => io.enq(j).bits.lsu_isa.isVector | io.enq(j).bits.vectorIsa.isVALU }.foldLeft(false.B)(_|_) ){
        io.deq(i).valid := false.B
        io.deq(i).bits  := 0.U.asTypeOf(new IF4_Bundle)
        io.enq(i).ready := false.B
      } .otherwise{
        when( ~(io.enq(i).bits.lsu_isa.isVector | io.enq(i).bits.vectorIsa.isVALU) ){
          io.enq(i) <> io.deq(i)
        } .otherwise{
          vecSplitReq := io.enq(i).bits
          when( nf === 0.U & lmul(1,0) === 0.U & ~isWiden ){ //dont splitter
            io.enq(i) <> io.deq(i)
          }.otherwise{ //splitter
            io.deq(i).valid := false.B
            io.deq(i).bits  := 0.U.asTypeOf(new IF4_Bundle)
            io.enq(i).ready := false.B
          }
        }
      }
    }

  }

}






trait VecSplitterMicroInstr{ this: VecSplitterBase =>
      



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

    microInstr(i).alu_isa    := 0.U.asTypeOf( new Alu_isa )
    microInstr(i).bru_isa    := 0.U.asTypeOf( new Bru_isa )
    microInstr(i).lsu_isa    := vSplitReq.lsu_isa
    microInstr(i).csr_isa    := 0.U.asTypeOf( new Csr_isa )
    microInstr(i).mul_isa    := 0.U.asTypeOf( new Mul_isa )
    microInstr(i).privil_isa := 0.U.asTypeOf( new Privil_isa )
    microInstr(i).fpu_isa    := 0.U.asTypeOf( new Fpu_isa )
    microInstr(i).vectorIsa  := vSplitReq.vectorIsa

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

    microInstr(i).param.raw.rs1 := vSplitReq.param.raw.rs1 + Mux( vSplitReq.vectorIsa.isVS1, lmulSel(i), 0.U)

    microInstr(i).param.raw.rs2 :=
      Mux1H(Seq(
        ( vSplitReq.vectorIsa.isVS2 &  vSplitReq.vectorIsa.isVS2P ) -> ( vSplitReq.param.raw.rs2 + (lmulSel(i) << 1) + widenSel(i) ),
        ( vSplitReq.vectorIsa.isVS2 & ~vSplitReq.vectorIsa.isVS2P ) -> ( vSplitReq.param.raw.rs2 +  lmulSel(i) ),
        ( vSplitReq.lsu_isa.isVS2                                 ) -> ( vSplitReq.param.raw.rs2 +  lmulSel(i) ), //nf didn't effect vs2 in lsu
        ( vSplitReq.isRS2                                         ) -> ( vSplitReq.param.raw.rs2 ),
      ))

    microInstr(i).param.raw.rs3 := 
      Mux1H( Seq(
        ( vSplitReq.vectorIsa.isVwb &  vSplitReq.vectorIsa.is2Malloc ) -> (vSplitReq.param.raw.rd0 + (lmulSel(i) << 1) + widenSel(i)),
        ( vSplitReq.vectorIsa.isVwb & ~vSplitReq.vectorIsa.is2Malloc ) -> (vSplitReq.param.raw.rd0 +  lmulSel(i) ),
        ( vSplitReq.lsu_isa.isVwb                                    ) -> (vSplitReq.param.raw.rd0 +  lmulSel(i) ),
        ( vSplitReq.lsu_isa.isVS3                                    ) -> (vSplitReq.param.raw.rs3), //VSTORE
      ))

    microInstr(i).param.raw.rd0 := 
      Mux1H( Seq(
        ( vSplitReq.vectorIsa.isVwb &  vSplitReq.vectorIsa.is2Malloc ) -> (vSplitReq.param.raw.rd0 + (lmulSel(i) << 1) + widenSel(i) ),
        ( vSplitReq.vectorIsa.isVwb & ~vSplitReq.vectorIsa.is2Malloc ) -> (vSplitReq.param.raw.rd0 +  lmulSel(i) ),
        ( vSplitReq.lsu_isa.isVLoad                                  ) -> (vSplitReq.param.raw.rd0 +  lmulSel(i) ),
        ( vSplitReq.lsu_isa.isVStore                                 ) -> 0.U,
      ))

    microInstr(i).vAttach.get.isLast    := (i+1).U === Cal_MicroInstr_Cnt
    microInstr(i).vAttach.get.vstart    := DontCare
    microInstr(i).vAttach.get.vl        := DontCare
    microInstr(i).vAttach.get.vtype     := vtype
    microInstr(i).vAttach.get.vstartSel := DontCare
  }

}



class VecSplitter()(implicit p: Parameters) extends VecSplitterBase with VecSplitterMux with VecSplitterMicroInstr{

}


