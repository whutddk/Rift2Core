

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
import chipsalliance.rocketchip.config.Parameters


class Alu(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val alu_iss_exe = Flipped(new DecoupledIO(new Alu_iss_info))
    val alu_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64))

    val flush = Input(Bool())
  })

  val alu_exe_iwb_fifo = Module( new Queue( new WriteBack_info(dw=64), 1, true, false ) )
  io.alu_exe_iwb <> alu_exe_iwb_fifo.io.deq
  alu_exe_iwb_fifo.reset := reset.asBool | io.flush


  val is_32w = io.alu_iss_exe.bits.param.is_32w
  val is_usi = io.alu_iss_exe.bits.param.is_usi

  val op1 = io.alu_iss_exe.bits.param.dat.op1
  val op2 = io.alu_iss_exe.bits.param.dat.op2

  def cutTo32(in: UInt) = Cat(Fill( 32, in(31) ), in(31, 0))


  val adder_res = op1 + op2
  val adder_res_32w = cutTo32(adder_res)
  val alu_add_res = Mux(is_32w, adder_res_32w, adder_res)

  val slt_sign_res = Mux( op1.asSInt < op2.asSInt, 1.U, 0.U )
  val slt_unsi_res = Mux( op1 < op2, 1.U, 0.U )

  val alu_slt_res = Mux(is_usi, slt_unsi_res, slt_sign_res)

  val alu_xor_res = op1 ^ op2;
  val alu_or_res  = op1 | op2;
  val alu_and_res = op1 & op2;



  val shift_op2 = Mux(is_32w, op2(4,0), op2(5,0))

  val alu_sll_res  = Mux( is_32w, cutTo32( op1 << shift_op2), op1 << shift_op2 )

  val alu_srl_res  = Mux( is_32w, cutTo32(op1(31,0) >> shift_op2), op1 >> shift_op2)

  val sra_op1_128w = Mux( is_32w, Cat( Fill(96, op1(31)), op1(31,0) ), Cat( Fill(64, op1(63)), op1(63,0) ) )
  val alu_sra_res  = Mux( is_32w, cutTo32(sra_op1_128w >> shift_op2), sra_op1_128w >> shift_op2 )


  val res = MuxCase(DontCare, Array(
    io.alu_iss_exe.bits.fun.add -> alu_add_res,
    io.alu_iss_exe.bits.fun.slt -> alu_slt_res,
    io.alu_iss_exe.bits.fun.xor -> alu_xor_res,
    io.alu_iss_exe.bits.fun.or  -> alu_or_res,
    io.alu_iss_exe.bits.fun.and -> alu_and_res,
    io.alu_iss_exe.bits.fun.sll -> alu_sll_res,
    io.alu_iss_exe.bits.fun.srl -> alu_srl_res,
    io.alu_iss_exe.bits.fun.sra -> alu_sra_res
  ))

  io.alu_iss_exe.ready := alu_exe_iwb_fifo.io.enq.fire

  alu_exe_iwb_fifo.io.enq.valid := io.alu_iss_exe.valid 
  alu_exe_iwb_fifo.io.enq.bits.res := res
  alu_exe_iwb_fifo.io.enq.bits.rd0 := io.alu_iss_exe.bits.param.rd0

}
