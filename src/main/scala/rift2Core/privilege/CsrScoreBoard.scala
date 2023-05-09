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

package rift2Core.privilege


import chisel3._
import chisel3.util._
import base._
import rift2Core.backend._

import rift2Chip._
import org.chipsalliance.cde.config._


// class VPU_Commit_Bundle(implicit p: Parameters) extends RiftBundle{
//   val vstart = UInt(log2Ceil(vParams.vlen).W )
//   val vl     = UInt(log2Ceil(vParams.vlen).W )
//   val vtype  = UInt(9.W)

//   val isVstart =Bool()
//   val isVl =Bool()
//   val isVtype =Bool()
// }


/**
  * An interface for XPUCsrIO signals.
  * @param p The parameters of the system.
  */
class XPUCsrIO(implicit p: Parameters) extends RiftBundle{
  /** The csr allocation signal, used at rename stage */
  val molloc    = Vec(rnChn, Flipped(Decoupled(UInt(12.W))))
  /** The write-back signal, used at execute stage */
  val writeBack = Flipped(Valid(new Exe_Port))
  /** The commit signal, used at commit stage */
  val commit    = Decoupled(new Exe_Port)
}

/**
  * An interface for FPUCsrIO signals.
  * @param p The parameters of the system.
  */
class FPUCsrIO(implicit p: Parameters) extends RiftBundle{
  /** The csr allocation signal, used at rename stage while a FAlu instruction is renaming */
  val molloc    = Vec(rnChn, Flipped(Decoupled(Bool())))
  /** The write-back signal, used at execute stage */
  val writeBack = Flipped(Valid(new Exe_Port))
  /** The commit signal, used at commit stage */
  val commit    = Decoupled(new Exe_Port)
}

/**
  * An interface for VPUCsrIO signals.
  * @param p The parameters of the system.
  */
class VPUCsrIO(implicit p: Parameters) extends FPUCsrIO

/**
  * An interface for the CSR scoreboard signals.
  * @param p The parameters of the system.
  */
class CsrScoreBoardIO(implicit p: Parameters) extends RiftBundle{
  val xpu: XPUCsrIO      = new XPUCsrIO
  val fpu: FPUCsrIO      = new FPUCsrIO
  val vpu: VPUCsrIO      = new VPUCsrIO
  val isAbort: Bool = Input(Bool())
}

/**
  * An abstract class that extends RiftModule and defines a CSR scoreboard base module.
  * @param p Represents the Parameters
  */
abstract class CsrScoreBoardBase(implicit p: Parameters) extends RiftModule{
  /** An instance of CsrScoreBoardIO that has two sub-interfaces of XPUCsrIO and FPUCsrIO and a Boolean input called isAbort */
  val io: CsrScoreBoardIO = IO(new CsrScoreBoardIO)

  /** A class to create CSR which Read & Write in order with given name and address.
    * @param name The name of the CSR
    * @param address The memory address of the CSR
    */
  class CSRInOrderReadWrite(val name: String, val address: Int) {
    /** interface */
    val isFlush  = Wire(Bool())
    val isClear  = Wire(Bool())
    val isMolloc = Wire(Bool())

    val isReady4Molloc = Wire(Bool()) //used at renmae

    /** Read-State and Write-State as binary values */
    def RDSTATE = "b0".U
    def WRSTATE = "b1".U

    /** Current state of the CSR */
    val curState = RegInit(RDSTATE)

    /** Flushes the current state if isFlush flag is high */
    when(isFlush){
      curState := RDSTATE
    } .elsewhen(isMolloc){
      curState := ~curState //WRSTATE
      assert(curState === RDSTATE, cf"Assert Failed at ${name}($address%X), Write requesting when not ready" )
    }.elsewhen( (curState === WRSTATE) & isClear){ //the same csr cannot molloc again until the counter is committed to empty once
      curState := ~curState //RDSTATE
    }
    isReady4Molloc := curState === RDSTATE
  }

  
  val XPUStatus: Seq[CSRInOrderReadWrite] = CSRInfoTable.XPUCSRGroup.map{ x => new CSRInOrderReadWrite( x.name, x.address ) }
  val FPUStatus: Seq[CSRInOrderReadWrite] = CSRInfoTable.FPUCSRGroup.map{ x => new CSRInOrderReadWrite( x.name, x.address ) }
  val VPUStatus: Seq[CSRInOrderReadWrite] = CSRInfoTable.VPUCSRGroup.map{ x => new CSRInOrderReadWrite( x.name, x.address ) }

  /** create all implemented CSR register from the preset Array CSRInfoTable */
  val CSRStatus = XPUStatus ++ FPUStatus ++ VPUStatus

  /** Maximum inflight number of CSR Instruction */
  def xpuInfly = 4
  val xpuInflyCounter  = Module( new Queue(new Bool(),   entries = xpuInfly, pipe = false, flow = false) )
  val xpuWriteBackFifo = Module( new Queue(new Exe_Port, entries = xpuInfly, pipe = false, flow = false) )
  val isXpuInfly = xpuInflyCounter.io.deq.valid

  /** Maximum inflight number of FALU */
  def fpuInfly = 4
  val fpuInflyCounter  = Module( new Queue(new Bool(),   entries = fpuInfly, pipe = false, flow = false) )
  val fpuWriteBackFifo = Module( new Queue(new Exe_Port, entries = fpuInfly, pipe = false, flow = false) )
  val isFpuInfly = fpuInflyCounter.io.deq.valid

  /** Maximum inflight number of VALU, Works in Ping-ping Mode */
  def vpuInfly = 2
  val vpuInflyCounter  = Module( new Queue(new Bool(),   entries = vpuInfly, pipe = true, flow = false) )
  val vpuWriteBackFifo = Module( new Queue(new Exe_Port, entries = vpuInfly, pipe = true, flow = false) )
  val isVpuInfly = vpuInflyCounter.io.deq.valid
  val isVtypeLock = RegInit(false.B)

}

/** A trait that extends CsrScoreBoardBase and defines a scoreboard module for the CSR Instruction. */
trait CsrScoreBoardXPU{ this: CsrScoreBoardBase =>

  for( i <- 0 until rnChn ){
    io.xpu.molloc(i).ready := 
      Mux1H(CSRStatus.map{ x => ((x.address).U === io.xpu.molloc(i).bits) -> (
        x.isReady4Molloc &                                                            //the requesting csr is ready to molloc
        xpuInflyCounter.io.enq.ready &                                                //the inflight instructions are not overflow 
        ( (0 until i).map{ j => ~(io.xpu.molloc(j).valid) }.foldLeft(true.B)(_&_) ) & //single issue
        (
          if( (x.address == 0x001) || (x.address == 0x002) || (x.address == 0x003) ){ //no fpu request at fpu csr
            ~isFpuInfly & 
            (0 until i).map{ j => ~(io.fpu.molloc(j).valid) }.foldLeft(true.B)(_&_)
          } else {
            true.B
          }
        ))
      })
    
    xpuInflyCounter.io.enq.valid := io.xpu.molloc.map{ x => x.fire }.foldLeft(false.B)(_|_)
    xpuInflyCounter.io.enq.bits  := DontCare

    assert(PopCount( io.xpu.molloc.map{ x => x.fire } ) <= 1.U, "Assert Failed, only one chn will fire")
  }

  CSRStatus.map{ x =>
    x.isMolloc :=
      ( 0 until rnChn ).map{ i => io.xpu.molloc(i).fire & ((x.address).U === io.xpu.molloc(i).bits) }.reduce(_|_)

    /** the same csr cannot molloc again until the counter is empty once */
    x.isClear := ~isXpuInfly
  }

  xpuWriteBackFifo.io.enq.valid := io.xpu.writeBack.fire
  xpuWriteBackFifo.io.enq.bits  := io.xpu.writeBack.bits
  assert(~(xpuWriteBackFifo.io.enq.valid & ~xpuWriteBackFifo.io.enq.ready), "Assert Failed at xpuWriteBackFifo, Overflow!\n")

  for( i <- 0 until cmChn ) {
    xpuInflyCounter.io.deq.ready := io.xpu.commit.fire
    assert( ~(xpuInflyCounter.io.deq.ready & ~xpuInflyCounter.io.deq.valid) )
    
    io.xpu.commit <> xpuWriteBackFifo.io.deq
    assert( ~(xpuWriteBackFifo.io.deq.ready & ~xpuWriteBackFifo.io.deq.valid) )
  }

}

/** A trait that extends CsrScoreBoardBase and defines a scoreboard module for the FALU instruction. */
trait CsrScoreBoardFPU{ this: CsrScoreBoardBase =>

  val fflags: CSRInOrderReadWrite = FPUStatus.apply(0); require(fflags.address == 0x001)
  val frm   : CSRInOrderReadWrite = FPUStatus.apply(1); require(frm.address    == 0x002)
  val fcsr  : CSRInOrderReadWrite = FPUStatus.apply(2); require(fcsr.address   == 0x003)

  for( i <- 0 until rnChn ){
    io.fpu.molloc(i).ready := 
      fflags.isReady4Molloc &
      frm.isReady4Molloc &
      fcsr.isReady4Molloc &   // no csr instruciton out-standing 
      fpuInflyCounter.io.enq.ready & //the falu out-standing instructions are not overflow 
      (0 until i).map{ j => ~(io.fpu.molloc(j).valid) }.foldLeft(true.B)(_&_) & //single issue
      (0 until i).map{ j => ~(io.xpu.molloc(j).valid & (io.xpu.molloc(j).bits === ("h001").U | io.xpu.molloc(j).bits === ("h002").U | io.xpu.molloc(j).bits === ("h003").U) ) }.foldLeft(true.B)(_&_) //no csr instruction molloc
 
    fpuInflyCounter.io.enq.valid := io.fpu.molloc.map{ x => x.fire }.foldLeft(false.B)(_|_)
    fpuInflyCounter.io.enq.bits  := DontCare

    assert(PopCount( io.fpu.molloc.map{ x => x.fire } ) <= 1.U, "Assert Failed, only one chn will fire")
  }

  fpuWriteBackFifo.io.enq.valid := io.fpu.writeBack.fire
  fpuWriteBackFifo.io.enq.bits  := io.fpu.writeBack.bits
  assert( ~(fpuWriteBackFifo.io.enq.valid & ~fpuWriteBackFifo.io.enq.ready), "Assert Failed at fpuWriteBackFifo, Overflow!\n")


  fpuInflyCounter.io.deq.ready := io.fpu.commit.fire
  assert( ~(fpuInflyCounter.io.deq.ready & ~fpuInflyCounter.io.deq.valid) )
  
  io.fpu.commit <> fpuWriteBackFifo.io.deq
  assert( ~(fpuWriteBackFifo.io.deq.ready & ~fpuWriteBackFifo.io.deq.valid) )

}


trait CsrScoreBoardVPU{ this: CsrScoreBoardBase =>
  println("Warning, Dont care about fcsr and vcsr now!\n")


  val vstart : CSRInOrderReadWrite = VPUStatus.apply(0); require( vstart.address == 0x008)
  val vxsat  : CSRInOrderReadWrite = VPUStatus.apply(1); require( vxsat.address == 0x009)
  val vxrm   : CSRInOrderReadWrite = VPUStatus.apply(2); require( vxrm.address == 0x00A)
  val vcsr   : CSRInOrderReadWrite = VPUStatus.apply(3); require( vcsr.address == 0x00F)
  val vl     : CSRInOrderReadWrite = VPUStatus.apply(4); require( vl.address == 0xC20)
  val vtype  : CSRInOrderReadWrite = VPUStatus.apply(5); require( vtype.address == 0xC21)
  val vlenb  : CSRInOrderReadWrite = VPUStatus.apply(7); require( vlenb.address == 0xC22)

  for( i <- 0 until rnChn ){

    io.vpu.molloc(i).ready := 
      ~isVtypeLock &
      ~vpuInflyCounter.io.enq.ready &
      (0 until i).map{ j => ~(io.vpu.molloc(j).valid) }.foldLeft(true.B)(_&_) //single issue

    vpuInflyCounter.io.enq.valid := io.vpu.molloc.map{ x => x.fire }.foldLeft(false.B)(_|_)
    vpuInflyCounter.io.enq.bits := DontCare

    assert(PopCount( io.vpu.molloc.map{ x => x.fire } ) <= 1.U, "Assert Failed, only one chn will fire")
  }

  when( io.isAbort ){
    isVtypeLock := false.B
  }
  .elsewhen( io.vpu.molloc.map{ x => (x.fire & x.bits === false.B) }.foldLeft(false.B)(_|_) ){
    assert( isVtypeLock === false.B )
    isVtypeLock := true.B
  }
  .elsewhen( io.vpu.commit.fire & io.vpu.commit.bits.addr === "hFFE".U ){
    isVtypeLock := false.B
  }


  vpuWriteBackFifo.io.enq.valid := io.vpu.writeBack.fire
  vpuWriteBackFifo.io.enq.bits  := io.vpu.writeBack.bits
  assert( ~(vpuWriteBackFifo.io.enq.valid & ~vpuWriteBackFifo.io.enq.ready), "Assert Failed at vpuWriteBackFifo, Overflow!\n")


  vpuInflyCounter.io.deq.ready := io.vpu.commit.fire
  assert( ~(vpuInflyCounter.io.deq.ready & ~vpuInflyCounter.io.deq.valid) )
                      
  io.vpu.commit <> vpuWriteBackFifo.io.deq
  assert( ~(vpuWriteBackFifo.io.deq.ready & ~vpuWriteBackFifo.io.deq.valid) )

}


class CsrScoreBoard(implicit p: Parameters) extends CsrScoreBoardBase
with CsrScoreBoardXPU
with CsrScoreBoardFPU{

  CSRStatus.map{ x => 
    x.isFlush := io.isAbort
  }


  xpuInflyCounter.reset  := io.isAbort | reset.asBool
  xpuWriteBackFifo.reset := io.isAbort | reset.asBool
  fpuInflyCounter.reset  := io.isAbort | reset.asBool
  fpuWriteBackFifo.reset := io.isAbort | reset.asBool
  vpuInflyCounter.reset  := io.isAbort | reset.asBool
  vpuWriteBackFifo.reset := io.isAbort | reset.asBool
}

