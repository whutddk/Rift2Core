
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




abstract class VRenameFilterBase()(implicit p: Parameters) extends RiftModule {

  class VRenameFilterIO extends Bundle{
    val enq = Vec(rnChn, Flipped(new DecoupledIO(new IF4_Bundle)))
    val deq = Vec(rnChn, new DecoupledIO(new IF4_Bundle))

    val csrfiles   = Input(new CSR_Bundle)
    val csrIsReady = Input(new CSR_LOG_Bundle(cRegNum))

    val flush = Input(Bool())    
  }

  val io: VRenameFilterIO = IO(new VRenameFilterIO)

  val vRenameScoreBoardFifo = Module(new MultiPortFifo( new IF4_Bundle, aw = 3, in = 8, out = rnChn ))
}


abstract class VRenameFilterVCSRProtect()(implicit p: Parameters) extends VRenameFilterBase{
  val isRestart = RegInit(true.B)
  println("Warning, As restart check for vstart, vl, an exception or fof will cause a flush!")
  when( io.flush ){
    isRestart := true.B
  } .elsewhen( vRenameScoreBoardFifo.io.enq(0).fire ){
    isRestart := false.B
  }

  val isPendingVStart = RegInit(false.B)
  val isPendingVConfig = RegInit(false.B)

  val isMollocVStart: Seq[Bool] = for( i <- 0 until rnChn ) yield {
    io.enq(i).bits.csr_isa.is_csr & io.enq(i).bits.param.imm === "h008".U
  }

  val isMollocVConfig: Seq[Bool] = for( i <- 0 until rnChn ) yield {
    io.enq(i).bits.vectorIsa.isVConfig
  }

  when( io.flush ){
    isPendingVStart := false.B
  }.elsewhen( (0 until rnChn).map{ i => ( io.enq(i).fire & isMollocVStart(i) ) }.reduce(_|_) ){
    isPendingVStart := true.B
  }.elsewhen( io.csrIsReady.vstart.isFree ){
    isPendingVStart := false.B
  }

  when( io.flush ){
    isPendingVConfig := false.B
  }.elsewhen( (0 until rnChn).map{ i => ( io.enq(i).fire & isMollocVConfig(i) ) }.reduce(_|_) ){
    isPendingVConfig := true.B
  }.elsewhen( io.csrIsReady.vConfig.isFree ){
    isPendingVConfig := false.B
  }

  val isVStartFree = for( i <- 0 until rnChn ) yield {
    ~isRestart & ~isPendingVStart &
    ( 0 until i ).map{j => ~isMollocVStart(j)}.fold(true.B)(_&_)
  }

  val isVConfigFree = for( i <- 0 until rnChn ) yield {
    ~isPendingVConfig &
    ( 0 until i ).map{j => ~isMollocVConfig(j)}.fold(true.B)(_&_)
  }



  val isVcsrInfoReady: Seq[Bool] = for( i <- 0 until rnChn ) yield {
    isVStartFree(i) & isVConfigFree(i)
  }

  val vtype  = io.csrfiles.vConfig.vtype
  val vstart = Mux(isRestart, io.csrfiles.vstart, 0.U)
  val vl     = io.csrfiles.vConfig.vl


  val lmul: Seq[UInt] = for( i <- 0 until rnChn ) yield {
    assert( vtype(2) === 0.U, "Assert Failed, do not support fractional now") 
    vtype(2,0)
  }

  val vsew: Seq[UInt] = for( i <- 0 until rnChn ) yield {
    vtype(4,3)
  }

  val nf: Seq[UInt] = for( i <- 0 until rnChn ) yield {
    Mux(io.enq(i).bits.lsu_isa.isAcquireFifo, io.enq(i).bits.vAttach.get.nf, 0.U)
  }

  val isAcquireFifo: Seq[Bool] = for( i <- 0 until rnChn ) yield {
    ( io.enq(i).bits.lsu_isa.isAcquireFifo   & ((lmul(i).apply(2) === 0.U & lmul(i).apply(1, 0) =/= 0.U) | nf(i) =/= 0.U)) |
    ( io.enq(i).bits.vectorIsa.isAcquireFifo & ( lmul(i).apply(2) === 0.U & lmul(i).apply(1, 0) =/= 0.U)                 )
  }

  def Cal_MicroInstr_Cnt: Seq[UInt] = for( i <- 0 until rnChn ) yield {
    MuxCase(lmul(i).apply(1,0), Seq(
      ( io.enq(i).bits.lsu_isa.isVector                                      ) -> ((nf(i)+1.U) << lmul(i).apply(1,0)),
      ( io.enq(i).bits.vectorIsa.isVS2P | io.enq(i).bits.vectorIsa.is2Malloc ) -> ( 2.U        << lmul(i).apply(1,0)),
    ))
  }

  def Pkg_MicroInstr_Bundle(mic: Int): Seq[IF4_Bundle] = for( i <- 0 until rnChn ) yield {
    val deq = Wire(new IF4_Bundle)

    deq.alu_isa    := 0.U.asTypeOf( new Alu_isa )
    deq.bru_isa    := 0.U.asTypeOf( new Bru_isa )
    deq.lsu_isa    := io.enq(i).bits.lsu_isa
    deq.csr_isa    := 0.U.asTypeOf( new Csr_isa )
    deq.mul_isa    := 0.U.asTypeOf( new Mul_isa )
    deq.privil_isa := 0.U.asTypeOf( new Privil_isa )
    deq.fpu_isa    := 0.U.asTypeOf( new Fpu_isa )
    deq.vectorIsa  := io.enq(i).bits.vectorIsa

    deq.param.is_rvc := false.B
    deq.param.pc  := io.enq(i).bits.param.pc
    deq.param.imm := 0.U
    deq.param.rm  := io.enq(i).bits.param.rm

    deq.vAttach.get.vm  := io.enq(i).bits.vAttach.get.vm
    deq.vAttach.get.nf  := nf(i)

    val lmulSel = Wire(UInt(3.W))
    deq.vAttach.get.lmulSel := lmulSel

    when( io.enq(i).bits.lsu_isa.isVector ){
      lmulSel := Mux1H(Seq(
        (nf(i) === "b000".U) -> (mic/1).U, (nf(i) === "b001".U) -> (mic/2).U, //nf = 2
        (nf(i) === "b010".U) -> (mic/3).U, (nf(i) === "b011".U) -> (mic/4).U, //nf = 4
        (nf(i) === "b100".U) -> 0.U,       (nf(i) === "b101".U) -> 0.U,
        (nf(i) === "b110".U) -> 0.U,       (nf(i) === "b111".U) -> 0.U,
      ))
    }.elsewhen( io.enq(i).bits.vectorIsa.isVS2P | io.enq(i).bits.vectorIsa.is2Malloc ){
      lmulSel := (mic/2).U
    }.otherwise{
      lmulSel := (mic/1).U
    }



    deq.vAttach.get.nfSel := 
      Mux1H(Seq(
        (nf(i) === "b000".U) -> (mic%1).U,
        (nf(i) === "b001".U) -> (mic%2).U,
        (nf(i) === "b010".U) -> (mic%3).U,
        (nf(i) === "b011".U) -> (mic%4).U,
        (nf(i) === "b100".U) -> mic.U,
        (nf(i) === "b101".U) -> mic.U,
        (nf(i) === "b110".U) -> mic.U,
        (nf(i) === "b111".U) -> mic.U,
      ))


    val widenSel = Mux( io.enq(i).bits.vectorIsa.isVS2P | io.enq(i).bits.vectorIsa.is2Malloc, (mic%2).U, 0.U )

    deq.vAttach.get.widenSel := widenSel

    deq.param.raw.rs1 := io.enq(i).bits.param.raw.rs1 + Mux( io.enq(i).bits.vectorIsa.isVS1, lmulSel, 0.U)

    deq.param.raw.rs2 :=
      Mux1H(Seq(
        ( io.enq(i).bits.vectorIsa.isVS2 &  io.enq(i).bits.vectorIsa.isVS2P ) -> ( io.enq(i).bits.param.raw.rs2 + (lmulSel << 1) + widenSel ),
        ( io.enq(i).bits.vectorIsa.isVS2 & ~io.enq(i).bits.vectorIsa.isVS2P ) -> ( io.enq(i).bits.param.raw.rs2 + lmulSel ),
        ( io.enq(i).bits.lsu_isa.isVS2                                      ) -> ( io.enq(i).bits.param.raw.rs2 + lmulSel ), //nf didn't effect vs2 in lsu
        ( io.enq(i).bits.isRS2                                              ) -> ( io.enq(i).bits.param.raw.rs2 ),
      ))

      // io.enq(i).bits.param.raw.rs2 +
      //   Mux(
      //     io.enq(i).bits.vectorIsa.isVS2 | io.enq(i).bits.lsu_isa.isVS2,
      //     lmulSel << Mux(io.enq(i).bits.vectorIsa.isVS2P, 1.U, 0.U) + widenSel,
      //     0.U
      //   )

    deq.param.raw.rs3 := 
      Mux1H( Seq(
        ( io.enq(i).bits.vectorIsa.isVwb &  io.enq(i).bits.vectorIsa.is2Malloc ) -> (io.enq(i).bits.param.raw.rd0 + (lmulSel << 1) + widenSel),
        ( io.enq(i).bits.vectorIsa.isVwb & ~io.enq(i).bits.vectorIsa.is2Malloc ) -> (io.enq(i).bits.param.raw.rd0 + lmulSel ),
        ( io.enq(i).bits.lsu_isa.isVwb                                         ) -> (io.enq(i).bits.param.raw.rd0 + lmulSel ),
        ( io.enq(i).bits.lsu_isa.isVS3                                         ) -> (io.enq(i).bits.param.raw.rs3), //VSTORE
      ))

    deq.param.raw.rd0 := 
      Mux1H( Seq(
        ( io.enq(i).bits.vectorIsa.isVwb &  io.enq(i).bits.vectorIsa.is2Malloc ) -> (io.enq(i).bits.param.raw.rd0 + (lmulSel << 1) + widenSel ),
        ( io.enq(i).bits.vectorIsa.isVwb & ~io.enq(i).bits.vectorIsa.is2Malloc ) -> (io.enq(i).bits.param.raw.rd0 + lmulSel ),
        ( io.enq(i).bits.lsu_isa.isVwb                                         ) -> (io.enq(i).bits.param.raw.rd0 + lmulSel ),
        ( io.enq(i).bits.lsu_isa.isVStore                                      ) -> 0.U,
      ))

    deq.vAttach.get.isLast    := (mic+1).U === Cal_MicroInstr_Cnt(i)
    deq.vAttach.get.vstart    := vstart
    deq.vAttach.get.vl        := io.csrfiles.vConfig.vl
    deq.vAttach.get.vtype     := vtype
    deq.vAttach.get.vstartSel := (((vParams.vlen * mic).U >> 3) >> vsew(i)) 

    deq
  }

}

abstract class VRenameFilterMux()(implicit p: Parameters) extends VRenameFilterVCSRProtect{
  when(vRenameScoreBoardFifo.io.deq(0).valid) { //when fifo is not empty

    (0 until 8).map{ i =>
      vRenameScoreBoardFifo.io.enq(i).valid := false.B
    }

    (0 until rnChn).map{ i => io.enq(i).ready := false.B}

    io.deq <> vRenameScoreBoardFifo.io.deq //when fifo is not empty, connect all input from fifo

  }.otherwise{ //when fifo is empty
    for( i <- 0 until rnChn ){
      when( ( 0 to i ).map{ j => ~isVcsrInfoReady(j)}.fold(false.B)(_|_) ){ // csr not ready
        io.enq(i).ready := false.B
        io.deq(i).valid := false.B
        io.deq(i).bits  := 0.U.asTypeOf(new IF4_Bundle)
      }.otherwise{
        when( ( ( 0 until i ).map{ j => isAcquireFifo(j)}).fold(false.B)(_|_) ){ //the pervious instr has requested fifo
          io.enq(i).ready := false.B

          io.deq(i).valid := false.B
          io.deq(i).bits  := 0.U.asTypeOf(new IF4_Bundle)
        }.elsewhen( isAcquireFifo(i) ){ //the this instr will request fifo

          for( eleIdx <- 0 until 8 ){
            vRenameScoreBoardFifo.io.enq(eleIdx).valid := eleIdx.U < Cal_MicroInstr_Cnt(i)
            vRenameScoreBoardFifo.io.enq(eleIdx).bits  := Pkg_MicroInstr_Bundle(eleIdx)(i)
          }

          io.enq(i).ready := vRenameScoreBoardFifo.io.enq(0).fire

          io.deq(i).valid := false.B
          io.deq(i).bits  := 0.U.asTypeOf(new IF4_Bundle)

          when(vRenameScoreBoardFifo.io.enq(0).fire) {
            assert( Cal_MicroInstr_Cnt(i) <=  8.U )
            assert( Cal_MicroInstr_Cnt(i) =/= 0.U )
          }

        }.otherwise{ //no instr has requested fifo yield //xinstr, finstr, vinstr
          io.enq(i) <> io.deq(i)
        }
      }
      
    }
  }
}

class VRenameFilter()(implicit p: Parameters) extends VRenameFilterMux{

}


