

/*
  Copyright (c) 2020 - 2022 Wuhan University of Technology <295054118@whut.edu.cn>

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
import chisel3.experimental.dataview._
import rift2Core.define._
import rift2Chip._
import chipsalliance.rocketchip.config.Parameters

abstract class BruBase()(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val bru_iss_exe = Flipped(new DecoupledIO(new Bru_iss_info))
    val bru_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64))

    val bftq = Flipped(Decoupled(new Branch_FTarget_Bundle))
    val jftq = Flipped(Decoupled(new Jump_FTarget_Bundle))

    val bctq = Decoupled(new Branch_CTarget_Bundle)
    val jctq = Decoupled(new Jump_CTarget_Bundle)

    val bcmm_update = Valid(new Branch_CTarget_Bundle)
    val jcmm_update = Valid(new Jump_CTarget_Bundle)

    val flush = Input(Bool())
  })

  val bru_exe_iwb_fifo = Module( new Queue( new WriteBack_info(dw=64), 1, true, false ) )
  val bctq = Module(new Queue( new Branch_CTarget_Bundle, (if(!isMinArea) 4 else 1) ) )
  val jctq = Module(new Queue( new Jump_CTarget_Bundle,   (if(!isMinArea) 4 else 1) ) )

  val misPredict_locker = RegInit(false.B)

  val op1 = io.bru_iss_exe.bits.param.dat.op1
  val op2 = io.bru_iss_exe.bits.param.dat.op2
  val imm = io.bru_iss_exe.bits.param.imm
  val pc  = io.bru_iss_exe.bits.param.pc
  val isRVC = io.bru_iss_exe.bits.param.is_rvc
}

trait BranchExe { this: BruBase => 

  val is_branchTaken = Mux1H(Seq(
    io.bru_iss_exe.bits.fun.beq  -> (op1 === op2),
    io.bru_iss_exe.bits.fun.bne  -> (op1 =/= op2),
    io.bru_iss_exe.bits.fun.blt  -> (op1.asSInt <  op2.asSInt),
    io.bru_iss_exe.bits.fun.bge  -> (op1.asSInt >= op2.asSInt),
    io.bru_iss_exe.bits.fun.bltu -> (op1 <  op2),
    io.bru_iss_exe.bits.fun.bgeu -> (op1 >= op2),
  ))

  bctq.io.enq.bits.viewAsSupertype(new Branch_FTarget_Bundle) := io.bftq.bits
  bctq.io.enq.bits.isFinalTaken := is_branchTaken
  bctq.io.enq.bits.finalTarget  := Mux( is_branchTaken, (pc + imm), (pc + Mux(isRVC, 2.U, 4.U)) )


}

trait JumpExe { this: BruBase => 
  jctq.io.enq.bits.viewAsSupertype(new Jump_FTarget_Bundle) := io.jftq.bits
  jctq.io.enq.bits.finalTarget := (op1 + imm) >> 1 << 1
}

/** Update IF when the branch / jalr is commited */
trait PredictorUpdate { this: BruBase => 

  io.bcmm_update.bits  := Mux(io.bctq.fire, io.bctq.bits, 0.U.asTypeOf(new Branch_CTarget_Bundle))
  io.bcmm_update.valid := io.bctq.fire
  io.jcmm_update.bits  := Mux(io.jctq.fire, io.jctq.bits, 0.U.asTypeOf(new Jump_CTarget_Bundle))
  io.jcmm_update.valid := io.jctq.fire
}



class Bru()(implicit p: Parameters) extends BruBase with BranchExe with JumpExe with PredictorUpdate {

  when( io.flush ) {
    misPredict_locker := false.B
  } .elsewhen( (bctq.io.enq.fire & bctq.io.enq.bits.isMisPredict) | (jctq.io.enq.fire & jctq.io.enq.bits.isMisPredict) ) {
    misPredict_locker := true.B
  }

  io.bru_exe_iwb <> bru_exe_iwb_fifo.io.deq
  io.bctq <> bctq.io.deq
  io.jctq <> jctq.io.deq

  bru_exe_iwb_fifo.reset := reset.asBool | io.flush
  bctq.reset := reset.asBool | io.flush
  jctq.reset := reset.asBool | io.flush


  io.bru_iss_exe.ready := bru_exe_iwb_fifo.io.enq.ready & bctq.io.enq.ready & jctq.io.enq.ready & ~misPredict_locker

  io.bftq.ready := io.bru_iss_exe.fire & io.bru_iss_exe.bits.fun.is_branch
  io.jftq.ready := io.bru_iss_exe.fire & io.bru_iss_exe.bits.fun.jalr

  bru_exe_iwb_fifo.io.enq.valid := io.bru_iss_exe.fire
  bctq.io.enq.valid := io.bru_iss_exe.fire & io.bru_iss_exe.bits.fun.is_branch
  jctq.io.enq.valid := io.bru_iss_exe.fire & io.bru_iss_exe.bits.fun.jalr

  bru_exe_iwb_fifo.io.enq.bits.res := pc + Mux( isRVC, 2.U, 4.U)
  bru_exe_iwb_fifo.io.enq.bits.rd0 := io.bru_iss_exe.bits.param.rd0
  bru_exe_iwb_fifo.io.enq.bits.rd1 := 0.U



  assert( jctq.io.enq.fire === (io.bru_iss_exe.fire & io.bru_iss_exe.bits.fun.jalr) )
  assert( io.jftq.fire === jctq.io.enq.fire )
  assert( bctq.io.enq.fire === (io.bru_iss_exe.fire & io.bru_iss_exe.bits.fun.is_branch)  )
  assert( io.bftq.fire === bctq.io.enq.fire )
  assert( io.bru_iss_exe.fire === bru_exe_iwb_fifo.io.enq.fire )


}
