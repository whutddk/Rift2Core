
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
import base._



class Out_of_Order_Issue() extends Module {

  val io = IO(new Bundle{
    // val alu_dpt_iss = Vec(rn_chn, Flipped(new DecoupledIO(new Dpt_info)))
    // val lsu_dpt_iss = Vec(rn_chn, Flipped(new DecoupledIO(new Dpt_info)))
    // val mul_dpt_iss = Vec(rn_chn, Flipped(new DecoupledIO(new Dpt_info)))

    // val alu_iss_exe = Vec(rop_chn, new DecoupledIO(new Alu_iss_info))
    // val lsu_iss_exe = Vec(rop_chn, new DecoupledIO(new Lsu_iss_info))
    // val mul_iss_exe = Vec(rop_chn, new DecoupledIO(new Mul_iss_info))

    val ooo_dpt_iss = Vec(4, Flipped(new DecoupledIO(new Dpt_info)))
    val alu_iss_exe = new DecoupledIO(new Alu_iss_info)
    val lsu_iss_exe = new DecoupledIO(new Lsu_iss_info)
    val mul_iss_exe = new DecoupledIO(new Mul_iss_info)


    val ooo_readOp  = Vec(4,  new iss_readOp_info(64))

  })




  val alu_iss_rePort = {
    val mdl = Module(new RePort( new Alu_iss_info, port=4))
    mdl
  }

  val alu_iss_fifo = {
    val mdl = Module(new MultiPortFifo( new Alu_iss_info, aw = 4, in = 4, out = 1 ))
    mdl.io.enq <> alu_iss_rePort.io.deq
    mdl.io.deq(0) <> io.alu_iss_exe
    mdl 
  }

  val lsu_iss_rePort = {
    val mdl = Module(new RePort( new Lsu_iss_info, port=4))
    mdl
  }

  val lsu_iss_fifo = {
    val mdl = Module(new MultiPortFifo( new Lsu_iss_info, aw = 4, in = 4, out = 1 ))
    mdl.io.enq <> lsu_iss_rePort.io.deq
    mdl.io.deq(0) <> io.lsu_iss_exe
    mdl 
  }

  val mul_iss_rePort = {
    val mdl = Module(new RePort( new Mul_iss_info, port=4))
    mdl
  }

  val mul_iss_fifo = {
    val mdl = Module(new MultiPortFifo( new Mul_iss_info, aw = 4, in = 4, out = 1 ))
    mdl.io.enq <> mul_iss_rePort.io.deq
    mdl.io.deq(0) <> io.mul_iss_exe
    mdl 
  }


  for ( i <- 0 until 4 ) yield {
    io.ooo_readOp(i).reg.valid := io.ooo_dpt_iss(i).valid
    io.ooo_readOp(i).reg.bits  := io.ooo_dpt_iss(i).bits.phy

    alu_iss_rePort.io.enq(i).valid := io.ooo_readOp(i).reg.fire & io.ooo_dpt_iss(i).bits.alu_isa.is_alu
    lsu_iss_rePort.io.enq(i).valid := io.ooo_readOp(i).reg.fire & io.ooo_dpt_iss(i).bits.lsu_isa.is_lsu
    mul_iss_rePort.io.enq(i).valid := io.ooo_readOp(i).reg.fire & io.ooo_dpt_iss(i).bits.mul_isa.is_mul

    alu_iss_rePort.io.enq(i).bits := Mux( alu_iss_rePort.io.enq(i).valid, Pkg_alu_iss(io.ooo_readOp(i), io.ooo_dpt_iss(i).bits), DontCare )
    lsu_iss_rePort.io.enq(i).bits := Mux( lsu_iss_rePort.io.enq(i).valid, Pkg_lsu_iss(io.ooo_readOp(i), io.ooo_dpt_iss(i).bits), DontCare )
    mul_iss_rePort.io.enq(i).bits := Mux( mul_iss_rePort.io.enq(i).valid, Pkg_mul_iss(io.ooo_readOp(i), io.ooo_dpt_iss(i).bits), DontCare )

    io.ooo_dpt_iss(i).ready := io.ooo_readOp(i).reg.ready & alu_iss_rePort.io.enq(i).ready & lsu_iss_rePort.io.enq(i).ready & mul_iss_rePort.io.enq(i).ready
  }

  def Pkg_alu_iss(op: iss_readOp_info, dpt: Dpt_info): Alu_iss_info = {
    val res = Wire(new Alu_iss_info)
    val src1 = op.dat.op1
    val src2 = op.dat.op2
    res.fun.add := dpt.alu_isa.is_fun_add
    res.fun.slt := dpt.alu_isa.is_fun_slt
    res.fun.xor := dpt.alu_isa.is_fun_xor
    res.fun.or  := dpt.alu_isa.is_fun_or
    res.fun.and := dpt.alu_isa.is_fun_and
    res.fun.sll := dpt.alu_isa.is_fun_sll
    res.fun.srl := dpt.alu_isa.is_fun_srl
    res.fun.sra := dpt.alu_isa.is_fun_sra

    res.param.is_32w  := dpt.alu_isa.is_32w
    res.param.is_usi  := dpt.alu_isa.is_usi


    res.param.dat.op1 :=
      Mux1H(Seq(
        dpt.alu_isa.lui    -> 0.U,  dpt.alu_isa.auipc  -> dpt.param.pc,
        dpt.alu_isa.addi   -> src1, dpt.alu_isa.addiw  -> src1,
        dpt.alu_isa.slti   -> src1, dpt.alu_isa.sltiu  -> src1,
        dpt.alu_isa.xori   -> src1, dpt.alu_isa.ori    -> src1,
        dpt.alu_isa.andi   -> src1, dpt.alu_isa.slli   -> src1,
        dpt.alu_isa.slliw  -> src1, dpt.alu_isa.srli   -> src1,
        dpt.alu_isa.srliw  -> src1, dpt.alu_isa.srai   -> src1,
        dpt.alu_isa.sraiw  -> src1, dpt.alu_isa.add    -> src1,
        dpt.alu_isa.addw   -> src1, dpt.alu_isa.sub    -> src1,
        dpt.alu_isa.subw   -> src1, dpt.alu_isa.sll    -> src1,
        dpt.alu_isa.sllw   -> src1, dpt.alu_isa.slt    -> src1,
        dpt.alu_isa.sltu   -> src1, dpt.alu_isa.xor    -> src1,
        dpt.alu_isa.srl    -> src1, dpt.alu_isa.srlw   -> src1,
        dpt.alu_isa.sra    -> src1, dpt.alu_isa.sraw   -> src1,
        dpt.alu_isa.or     -> src1, dpt.alu_isa.and    -> src1,

        dpt.alu_isa.wfi    -> 0.U,

    ))

    res.param.dat.op2 :=
      Mux1H(Seq(
        dpt.alu_isa.lui    -> dpt.param.imm,      dpt.alu_isa.auipc  -> dpt.param.imm,
        dpt.alu_isa.addi   -> dpt.param.imm,      dpt.alu_isa.addiw  -> dpt.param.imm,
        dpt.alu_isa.slti   -> dpt.param.imm,      dpt.alu_isa.sltiu  -> dpt.param.imm,
        dpt.alu_isa.xori   -> dpt.param.imm,      dpt.alu_isa.ori    -> dpt.param.imm,
        dpt.alu_isa.andi   -> dpt.param.imm,      dpt.alu_isa.slli   -> dpt.param.imm(5,0),
        dpt.alu_isa.slliw  -> dpt.param.imm(5,0), dpt.alu_isa.srli   -> dpt.param.imm(5,0),
        dpt.alu_isa.srliw  -> dpt.param.imm(5,0), dpt.alu_isa.srai   -> dpt.param.imm(5,0),
        dpt.alu_isa.sraiw  -> dpt.param.imm(5,0), dpt.alu_isa.add    -> src2,
        dpt.alu_isa.addw   -> src2,               dpt.alu_isa.sub    -> (~src2 + 1.U),
        dpt.alu_isa.subw   -> (~src2 + 1.U),      dpt.alu_isa.sll    -> src2,
        dpt.alu_isa.sllw   -> src2,               dpt.alu_isa.slt    -> src2,
        dpt.alu_isa.sltu   -> src2,               dpt.alu_isa.xor    -> src2,
        dpt.alu_isa.srl    -> src2,               dpt.alu_isa.srlw   -> src2,
        dpt.alu_isa.sra    -> src2,               dpt.alu_isa.sraw   -> src2,
        dpt.alu_isa.or     -> src2,               dpt.alu_isa.and    -> src2,

        dpt.alu_isa.wfi    -> 0.U
    ))

    res.param.rd0 := dpt.phy.rd0
    res.param.is_iwb := true.B
    res.param.is_fwb := false.B

    return res
  }

  def Pkg_lsu_iss(op: iss_readOp_info, dpt: Dpt_info): Lsu_iss_info = {
    val res = Wire(new Lsu_iss_info)
    // res.fun := dpt.isa


    res.fun := dpt.lsu_isa


    res.param.dat := op.dat
    res.param.rd0 := dpt.phy.rd0
    res.param.is_iwb := true.B
    res.param.is_fwb := false.B
    return res
  }

  def Pkg_mul_iss(op: iss_readOp_info, dpt: Dpt_info): Mul_iss_info = {
    val res = Wire(new Mul_iss_info)
    res.fun := dpt.mul_isa
 
    res.param.dat    := op.dat
    res.param.rd0    := dpt.phy.rd0
    res.param.is_iwb := true.B
    res.param.is_fwb := false.B
    return res
  }

}


class In_Order_Issue extends Module {
    val io = IO(new Bundle{
      val ito_dpt_iss = Flipped(new DecoupledIO(new Dpt_info))
      val bru_iss_exe = new DecoupledIO(new Bru_iss_info)
      val csr_iss_exe = new DecoupledIO(new Csr_iss_info)

      val ito_readOp  = new iss_readOp_info(64)

  })



  val bru_iss_fifo = {
    val mdl = Module(new Queue( new Bru_iss_info, 4, pipe = true, false ))
    mdl.io.deq <> io.bru_iss_exe
    mdl 
  }

  val csr_iss_fifo = {
    val mdl = Module(new Queue( new Csr_iss_info, 4, pipe = true, false ))
    mdl.io.deq <> io.csr_iss_exe
    mdl 
  }


  io.ito_readOp.reg.valid := io.ito_dpt_iss.valid
  io.ito_readOp.reg.bits  := io.ito_dpt_iss.bits.phy

  bru_iss_fifo.io.enq.valid := io.ito_readOp.reg.fire & io.ito_dpt_iss.bits.bru_isa.is_bru
  csr_iss_fifo.io.enq.valid := io.ito_readOp.reg.fire & io.ito_dpt_iss.bits.csr_isa.is_csr

  bru_iss_fifo.io.enq.bits := Mux( bru_iss_fifo.io.enq.valid, Pkg_bru_iss(io.ito_readOp, io.ito_dpt_iss.bits), DontCare )
  csr_iss_fifo.io.enq.bits := Mux( csr_iss_fifo.io.enq.valid, Pkg_csr_iss(io.ito_readOp, io.ito_dpt_iss.bits), DontCare )


  io.ito_dpt_iss.ready := io.ito_readOp.reg.ready & bru_iss_fifo.io.enq.ready & csr_iss_fifo.io.enq.ready



  def Pkg_bru_iss(op: iss_readOp_info, dpt: Dpt_info): Bru_iss_info = {
    val res = Wire(new Bru_iss_info)
    res.fun  := dpt.bru_isa
    res.param.is_rvc   := dpt.param.is_rvc
    res.param.pc   := dpt.param.pc
    res.param.imm   := dpt.param.imm
    res.param.dat   := op.dat
    res.param.rd0    := dpt.phy.rd0
    res.param.is_iwb := true.B
    res.param.is_fwb := false.B
    return res
  }

  def Pkg_csr_iss(op: iss_readOp_info, dpt: Dpt_info): Csr_iss_info = {
    val res = Wire(new Csr_iss_info)

    res.fun.rc  := dpt.csr_isa.rc | dpt.csr_isa.rci
    res.fun.rs  := dpt.csr_isa.rs | dpt.csr_isa.rsi
    res.fun.rw  := dpt.csr_isa.rw | dpt.csr_isa.rwi

    res.param.dat.op1 := 
      Mux1H(Seq(
        dpt.csr_isa.rw  -> op.dat.op1, dpt.csr_isa.rwi -> dpt.param.raw.rs1,
        dpt.csr_isa.rs  -> op.dat.op1, dpt.csr_isa.rsi -> dpt.param.raw.rs1,
        dpt.csr_isa.rc  -> op.dat.op1, dpt.csr_isa.rci -> dpt.param.raw.rs1
      ))

    res.param.dat.op2 := dpt.param.imm
    res.param.rd0    := dpt.phy.rd0
    res.param.is_iwb := true.B
    res.param.is_fwb := false.B
    return res
  }

}

class Issue extends Module {
  val io = IO(new Bundle{
    val ooo_dpt_iss = Vec(4, Flipped(new DecoupledIO(new Dpt_info)))
    val ito_dpt_iss = Flipped(new DecoupledIO(new Dpt_info))   

    val ooo_readOp  = Vec(4,  new iss_readOp_info(64))
    val ito_readOp  = new iss_readOp_info(64)

    val alu_iss_exe = new DecoupledIO(new Alu_iss_info)
    val lsu_iss_exe = new DecoupledIO(new Lsu_iss_info)
    val mul_iss_exe = new DecoupledIO(new Mul_iss_info)
    val bru_iss_exe = new DecoupledIO(new Bru_iss_info)
    val csr_iss_exe = new DecoupledIO(new Csr_iss_info)

  })


  val ooo_issue = {
    val mdl = Module(new Out_of_Order_Issue)
    mdl.io.ooo_dpt_iss <> io.ooo_dpt_iss
    mdl.io.ooo_readOp  <> io.ooo_readOp
    mdl.io.alu_iss_exe <> io.alu_iss_exe
    mdl.io.lsu_iss_exe <> io.lsu_iss_exe
    mdl.io.mul_iss_exe <> io.mul_iss_exe
    mdl
  }

  val ito_issue = {
    val mdl = Module(new In_Order_Issue)
    mdl.io.ito_dpt_iss <> io.ito_dpt_iss
    mdl.io.ito_readOp  <> io.ito_readOp
    mdl.io.bru_iss_exe <> io.bru_iss_exe
    mdl.io.csr_iss_exe <> io.csr_iss_exe
    mdl
  }
}
