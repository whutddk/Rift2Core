

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
import rift2Core.define._



class Bru extends Module {
  val io = IO(new Bundle{
    val bru_iss_exe = Flipped(new DecoupledIO(new Bru_iss_info))
    val bru_exe_iwb = new DecoupledIO(new WriteBack_info)

    val cmm_bru_ilp = Input(Bool())

    val bru_pd_b = new ValidIO( Bool() )
    val bru_pd_j = new ValidIO( UInt(64.W) )

    val flush = Input(Bool())
  })

  val bru_exe_iwb_fifo = Module( new Queue( new WriteBack_info, 1, false, false ) ) // to block back-to back branch
  io.bru_exe_iwb <> bru_exe_iwb_fifo.io.deq
  bru_exe_iwb_fifo.reset := reset.asBool | io.flush

  def iss_ack = io.bru_iss_exe.valid
  def iwb_ack = bru_exe_iwb_fifo.io.enq.valid & bru_exe_iwb_fifo.io.enq.ready


  def op1 = io.bru_iss_exe.bits.param.op1
  def op2 = io.bru_iss_exe.bits.param.op2
  
  def is_branchTaken = MuxCase(DontCare, Array(
    io.bru_iss_exe.bits.fun.beq  -> (op1 === op2),
    io.bru_iss_exe.bits.fun.bne  -> (op1 =/= op2),
    io.bru_iss_exe.bits.fun.blt  -> (op1.asSInt < op2.asSInt),
    io.bru_iss_exe.bits.fun.bge  -> (op1.asSInt >= op2.asSInt),
    io.bru_iss_exe.bits.fun.bltu -> (op1 <  op2),
    io.bru_iss_exe.bits.fun.bgeu -> (op1 >= op2),
  ))

  // two back to back branch&jalr may be executed together, the 2nd one is executed by mistake, by its mispredict will not affert the perivous one
  def is_clear_ilp = Mux(io.bru_iss_exe.bits.fun.is_branch, io.cmm_bru_ilp, true.B)


  io.bru_pd_b.bits  := is_branchTaken
  io.bru_pd_b.valid := iwb_ack & io.bru_iss_exe.bits.fun.is_branch
  io.bru_pd_j.bits  := (io.bru_iss_exe.bits.param.op1 + io.bru_iss_exe.bits.param.imm) & ~("b1".U(64.W))
  io.bru_pd_j.valid := iwb_ack & io.bru_iss_exe.bits.fun.jalr



  io.bru_iss_exe.ready := iwb_ack

  bru_exe_iwb_fifo.io.enq.valid := is_clear_ilp & io.bru_iss_exe.valid
  bru_exe_iwb_fifo.io.enq.bits.res := io.bru_iss_exe.bits.param.pc + Mux( io.bru_iss_exe.bits.param.is_rvc, 2.U, 4.U)
  bru_exe_iwb_fifo.io.enq.bits.rd0 := io.bru_iss_exe.bits.param.rd0
  bru_exe_iwb_fifo.io.enq.bits.is_iwb := true.B
  bru_exe_iwb_fifo.io.enq.bits.is_fwb := false.B


}

