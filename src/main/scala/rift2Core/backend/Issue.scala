
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



class Out_of_Order_Issue(rn_chn: Int = 2, rop_chn: Int=2) extends Module {

  val io = IO(new Bundle{
    val alu_dpt_iss = Vec(rn_chn, Flipped(new DecoupledIO(new Dpt_info)))
    val lsu_dpt_iss = Vec(rn_chn, Flipped(new DecoupledIO(new Dpt_info)))
    val mul_dpt_iss = Vec(rn_chn, Flipped(new DecoupledIO(new Dpt_info)))

    val alu_iss_exe = Vec(rop_chn, new DecoupledIO(new Alu_iss_info))
    val lsu_iss_exe = Vec(rop_chn, new DecoupledIO(new Lsu_iss_info))
    val mul_iss_exe = Vec(rop_chn, new DecoupledIO(new Mul_iss_info))

    val ooo_readOp  = Vec(3, Flipped( new iss_readOp_info))

  })

  val alu_dpt_fifo = {
    val mdl = MultiPortFifo( new Dpt_info, aw = 4, in = rn_chn, out = 1 )
    mdl.io.enq <> io.alu_dpt_iss
    mdl
  }

  val lsu_dpt_fifo = {
    val mdl = MultiPortFifo( new Dpt_info, aw = 4, in = rn_chn, out = 1 )
    mdl.io.enq <> io.lsu_dpt_iss
    mdl
  }

  val mul_dpt_fifo = {
    val mdl = MultiPortFifo( new Dpt_info, aw = 4, in = rn_chn, out = 1 )
    mdl.io.enq <> io.mul_dpt_iss
    mdl
  }


  ooo_readOp(0).reg.valid := alu_dpt_fifo.deq.io.valid
  ooo_readOp(0).reg.bits  := alu_dpt_fifo.deq.io.bits.phy

  io.alu_iss_exe.valid := ooo_readOp(0).reg.fire

  when( io.alu_iss_exe.valid ) {
    val src1 = ooo_readOp(0).reg.dat.op1
    val src2 = ooo_readOp(0).reg.dat.op2
    io.alu_iss_exe.bits.fun.add := alu_dpt_fifo.deq.io.bits.isa.is_fun_add
    io.alu_iss_exe.bits.fun.slt := alu_dpt_fifo.deq.io.bits.isa.is_fun_slt
    io.alu_iss_exe.bits.fun.xor := alu_dpt_fifo.deq.io.bits.isa.is_fun_xor
    io.alu_iss_exe.bits.fun.or  := alu_dpt_fifo.deq.io.bits.isa.is_fun_or
    io.alu_iss_exe.bits.fun.and := alu_dpt_fifo.deq.io.bits.isa.is_fun_and
    io.alu_iss_exe.bits.fun.sll := alu_dpt_fifo.deq.io.bits.isa.is_fun_sll
    io.alu_iss_exe.bits.fun.srl := alu_dpt_fifo.deq.io.bits.isa.is_fun_srl
    io.alu_iss_exe.bits.fun.sra := alu_dpt_fifo.deq.io.bits.isa.is_fun_sra

    io.alu_iss_exe.bits.param.is_32w  := alu_dpt_fifo.deq.io.bits.isa.is_32w
    io.alu_iss_exe.bits.param.is_usi  := alu_dpt_fifo.deq.io.bits.isa.is_usi


    io.alu_iss_info.param.dat.op1 :=
      Mux1H(Seq(
        alu_dpt_fifo.deq.io.bits.isa.lui    -> 0.U,  alu_dpt_fifo.deq.io.bits.isa.auipc  -> alu_dpt_fifo.deq.io.bits.param.pc,
        alu_dpt_fifo.deq.io.bits.isa.addi   -> src1, alu_dpt_fifo.deq.io.bits.isa.addiw  -> src1,
        alu_dpt_fifo.deq.io.bits.isa.slti   -> src1, alu_dpt_fifo.deq.io.bits.isa.sltiu  -> src1,
        alu_dpt_fifo.deq.io.bits.isa.xori   -> src1, alu_dpt_fifo.deq.io.bits.isa.ori    -> src1,
        alu_dpt_fifo.deq.io.bits.isa.andi   -> src1, alu_dpt_fifo.deq.io.bits.isa.slli   -> src1,
        alu_dpt_fifo.deq.io.bits.isa.slliw  -> src1, alu_dpt_fifo.deq.io.bits.isa.srli   -> src1,
        alu_dpt_fifo.deq.io.bits.isa.srliw  -> src1, alu_dpt_fifo.deq.io.bits.isa.srai   -> src1,
        alu_dpt_fifo.deq.io.bits.isa.sraiw  -> src1, alu_dpt_fifo.deq.io.bits.isa.add    -> src1,
        alu_dpt_fifo.deq.io.bits.isa.addw   -> src1, alu_dpt_fifo.deq.io.bits.isa.sub    -> src1,
        alu_dpt_fifo.deq.io.bits.isa.subw   -> src1, alu_dpt_fifo.deq.io.bits.isa.sll    -> src1,
        alu_dpt_fifo.deq.io.bits.isa.sllw   -> src1, alu_dpt_fifo.deq.io.bits.isa.slt    -> src1,
        alu_dpt_fifo.deq.io.bits.isa.sltu   -> src1, alu_dpt_fifo.deq.io.bits.isa.xor    -> src1,
        alu_dpt_fifo.deq.io.bits.isa.srl    -> src1, alu_dpt_fifo.deq.io.bits.isa.srlw   -> src1,
        alu_dpt_fifo.deq.io.bits.isa.sra    -> src1, alu_dpt_fifo.deq.io.bits.isa.sraw   -> src1,
        alu_dpt_fifo.deq.io.bits.isa.or     -> src1, alu_dpt_fifo.deq.io.bits.isa.and    -> src1,

        alu_dpt_fifo.deq.io.bits.isa.wfi    -> 0.U,

    ))

    io.alu_iss_info.param.dat.op2 :=
      Mux1H(Seq(
        alu_dpt_fifo.deq.io.bits.isa.lui    -> alu_dpt_fifo.deq.io.bits.param.imm,      alu_dpt_fifo.deq.io.bits.isa.auipc  -> alu_dpt_fifo.deq.io.bits.param.imm,
        alu_dpt_fifo.deq.io.bits.isa.addi   -> alu_dpt_fifo.deq.io.bits.param.imm,      alu_dpt_fifo.deq.io.bits.isa.addiw  -> alu_dpt_fifo.deq.io.bits.param.imm,
        alu_dpt_fifo.deq.io.bits.isa.slti   -> alu_dpt_fifo.deq.io.bits.param.imm,      alu_dpt_fifo.deq.io.bits.isa.sltiu  -> alu_dpt_fifo.deq.io.bits.param.imm,
        alu_dpt_fifo.deq.io.bits.isa.xori   -> alu_dpt_fifo.deq.io.bits.param.imm,      alu_dpt_fifo.deq.io.bits.isa.ori    -> alu_dpt_fifo.deq.io.bits.param.imm,
        alu_dpt_fifo.deq.io.bits.isa.andi   -> alu_dpt_fifo.deq.io.bits.param.imm,      alu_dpt_fifo.deq.io.bits.isa.slli   -> alu_dpt_fifo.deq.io.bits.param.imm(5,0),
        alu_dpt_fifo.deq.io.bits.isa.slliw  -> alu_dpt_fifo.deq.io.bits.param.imm(5,0), alu_dpt_fifo.deq.io.bits.isa.srli   -> alu_dpt_fifo.deq.io.bits.param.imm(5,0),
        alu_dpt_fifo.deq.io.bits.isa.srliw  -> alu_dpt_fifo.deq.io.bits.param.imm(5,0), alu_dpt_fifo.deq.io.bits.isa.srai   -> alu_dpt_fifo.deq.io.bits.param.imm(5,0),
        alu_dpt_fifo.deq.io.bits.isa.sraiw  -> alu_dpt_fifo.deq.io.bits.param.imm(5,0), alu_dpt_fifo.deq.io.bits.isa.add    -> src2,
        alu_dpt_fifo.deq.io.bits.isa.addw   -> src2,                                    alu_dpt_fifo.deq.io.bits.isa.sub    -> (~src2 + 1.U),
        alu_dpt_fifo.deq.io.bits.isa.subw   -> (~src2 + 1.U),                           alu_dpt_fifo.deq.io.bits.isa.sll    -> src2,
        alu_dpt_fifo.deq.io.bits.isa.sllw   -> src2,                                    alu_dpt_fifo.deq.io.bits.isa.slt    -> src2,
        alu_dpt_fifo.deq.io.bits.isa.sltu   -> src2,                                    alu_dpt_fifo.deq.io.bits.isa.xor    -> src2,
        alu_dpt_fifo.deq.io.bits.isa.srl    -> src2,                                    alu_dpt_fifo.deq.io.bits.isa.srlw   -> src2,
        alu_dpt_fifo.deq.io.bits.isa.sra    -> src2,                                    alu_dpt_fifo.deq.io.bits.isa.sraw   -> src2,
        alu_dpt_fifo.deq.io.bits.isa.or     -> src2,                                    alu_dpt_fifo.deq.io.bits.isa.and    -> src2,

        alu_dpt_fifo.deq.io.bits.isa.wfi    -> 0.U
    ))

    io.alu_iss_info.param.phy := alu_dpt_fifo.deq.io.bits.phy
  }
 


class Alu_iss_info extends Bundle {
  val fun = new Alu_function
  val param = new Alu_param
}


  alu_dpt_fifo.deq.io.ready := io.alu_iss_exe.fire




  ooo_readOp(1).valid := lsu_dpt_fifo.deq.io.valid

  ooo_readOp(2).valid := mul_dpt_fifo.deq.io.valid
  

}

