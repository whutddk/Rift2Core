
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


    val ooo_readOp  = Vec(4, Flipped( new iss_readOp_info))

  })




  val alu_iss_rePort = {
    val mdl = Module(new RePort( new Alu_iss_info, port=4))
    mdl
  }

  val alu_iss_fifo = {
    val mdl = Module(new MultiPortFifo( new Alu_iss_info, aw = 4, in = 4, out = 1 ))
    mdl.io.enq <> alu_iss_rePort.io.deq
    mdl.io.deq <> io.alu_iss_exe
    mdl 
  }

  val lsu_iss_rePort = {
    val mdl = Module(new RePort( new Lsu_iss_info, port=4))
    mdl
  }

  val lsu_iss_fifo = {
    val mdl = Module(new MultiPortFifo( new Lsu_iss_info, aw = 4, in = 4, out = 1 ))
    mdl.io.enq <> lsu_iss_rePort.io.deq
    mdl.io.deq <> io.lsu_iss_exe
    mdl 
  }

  val mul_iss_rePort = {
    val mdl = Module(new RePort( new Mul_iss_info, port=4))
    mdl
  }

  val mul_iss_fifo = {
    val mdl = Module(new MultiPortFifo( new Mul_iss_info, aw = 4, in = 4, out = 1 ))
    mdl.io.enq <> mul_iss_rePort.io.deq
    mdl.io.deq <> io.mul_iss_exe
    mdl 
  }


  for ( i <- 0 until 4 ) yield {
    io.ooo_readOp(i).reg.valid := io.ooo_dpt_iss(i).valid
    io.ooo_readOp(i).reg.bits  := io.ooo_dpt_iss(i).bits.phy

    alu_iss_rePort.io.enq(i).valid := io.ooo_readOp(i).reg.fire & io.ooo_dpt_iss(i).bits.isa.alu_isa.is_alu
    lsu_iss_rePort.io.enq(i).valid := io.ooo_readOp(i).reg.fire & io.ooo_dpt_iss(i).bits.isa.lsu_isa.is_lsu
    mul_iss_rePort.io.enq(i).valid := io.ooo_readOp(i).reg.fire & io.ooo_dpt_iss(i).bits.isa.mul_isa.is_mul

    alu_iss_rePort.io.enq(i).bits := Mux( alu_iss_rePort.io.enq(i).valid, Pkg_alu_iss(io.ooo_readOp(i), io.ooo_dpt_iss(i).bits), DontCare )
    lsu_iss_rePort.io.enq(i).bits := Mux( lsu_iss_rePort.io.enq(i).valid, Pkg_lsu_iss(io.ooo_readOp(i), io.ooo_dpt_iss(i).bits), DontCare )
    mul_iss_rePort.io.enq(i).bits := Mux( mul_iss_rePort.io.enq(i).valid, Pkg_mul_iss(io.ooo_readOp(i), io.ooo_dpt_iss(i).bits), DontCare )

    io.ooo_dpt_iss(i).ready := io.ooo_readOp(i).reg.ready & alu_iss_rePort.io.enq(i).ready & lsu_iss_rePort.io.enq(i).ready & mul_iss_rePort.io.enq(i).ready
  }

  def Pkg_alu_iss(op: iss_readOp_info, dpt: Dpt_info): Alu_iss_info = {
    val res = Wire(new Alu_iss_info)
    val src1 = op.reg.dat.op1
    val src2 = op.reg.dat.op2
    res.fun.add := dpt.isa.is_fun_add
    res.fun.slt := dpt.isa.is_fun_slt
    res.fun.xor := dpt.isa.is_fun_xor
    res.fun.or  := dpt.isa.is_fun_or
    res.fun.and := dpt.isa.is_fun_and
    res.fun.sll := dpt.isa.is_fun_sll
    res.fun.srl := dpt.isa.is_fun_srl
    res.fun.sra := dpt.isa.is_fun_sra

    res.param.is_32w  := dpt.isa.is_32w
    res.param.is_usi  := dpt.isa.is_usi


    res.param.dat.op1 :=
      Mux1H(Seq(
        dpt.isa.lui    -> 0.U,  dpt.isa.auipc  -> dpt.param.pc,
        dpt.isa.addi   -> src1, dpt.isa.addiw  -> src1,
        dpt.isa.slti   -> src1, dpt.isa.sltiu  -> src1,
        dpt.isa.xori   -> src1, dpt.isa.ori    -> src1,
        dpt.isa.andi   -> src1, dpt.isa.slli   -> src1,
        dpt.isa.slliw  -> src1, dpt.isa.srli   -> src1,
        dpt.isa.srliw  -> src1, dpt.isa.srai   -> src1,
        dpt.isa.sraiw  -> src1, dpt.isa.add    -> src1,
        dpt.isa.addw   -> src1, dpt.isa.sub    -> src1,
        dpt.isa.subw   -> src1, dpt.isa.sll    -> src1,
        dpt.isa.sllw   -> src1, dpt.isa.slt    -> src1,
        dpt.isa.sltu   -> src1, dpt.isa.xor    -> src1,
        dpt.isa.srl    -> src1, dpt.isa.srlw   -> src1,
        dpt.isa.sra    -> src1, dpt.isa.sraw   -> src1,
        dpt.isa.or     -> src1, dpt.isa.and    -> src1,

        dpt.isa.wfi    -> 0.U,

    ))

    res.param.dat.op2 :=
      Mux1H(Seq(
        dpt.isa.lui    -> dpt.param.imm,      dpt.isa.auipc  -> dpt.param.imm,
        dpt.isa.addi   -> dpt.param.imm,      dpt.isa.addiw  -> dpt.param.imm,
        dpt.isa.slti   -> dpt.param.imm,      dpt.isa.sltiu  -> dpt.param.imm,
        dpt.isa.xori   -> dpt.param.imm,      dpt.isa.ori    -> dpt.param.imm,
        dpt.isa.andi   -> dpt.param.imm,      dpt.isa.slli   -> dpt.param.imm(5,0),
        dpt.isa.slliw  -> dpt.param.imm(5,0), dpt.isa.srli   -> dpt.param.imm(5,0),
        dpt.isa.srliw  -> dpt.param.imm(5,0), dpt.isa.srai   -> dpt.param.imm(5,0),
        dpt.isa.sraiw  -> dpt.param.imm(5,0), dpt.isa.add    -> src2,
        dpt.isa.addw   -> src2,               dpt.isa.sub    -> (~src2 + 1.U),
        dpt.isa.subw   -> (~src2 + 1.U),      dpt.isa.sll    -> src2,
        dpt.isa.sllw   -> src2,               dpt.isa.slt    -> src2,
        dpt.isa.sltu   -> src2,               dpt.isa.xor    -> src2,
        dpt.isa.srl    -> src2,               dpt.isa.srlw   -> src2,
        dpt.isa.sra    -> src2,               dpt.isa.sraw   -> src2,
        dpt.isa.or     -> src2,               dpt.isa.and    -> src2,

        dpt.isa.wfi    -> 0.U
    ))

    res.param.rd0 := alu_dpt_fifo.deq.io.bits.phy.rd0
    res.param.is_iwb := true.B
    res.param.is_fwb := false.B

    return res
  }

  def Pkg_lsu_iss(op: iss_readOp_info, dpt: Dpt_info): Lsu_iss_info = {
    val res = Wire(new Lsu_iss_info)
    res.fun := dpt.isa
    res.param.dat := op.dat
    res.param.rd0 := dpt.phy.rd0
    res.param.is_iwb := true.B
    res.param.is_fwb := false.B
    return res
  }

  def Pkg_mul_iss(op: iss_readOp_info, dpt: Dpt_info): Mul_iss_info = {
    val res = Wire(new Mul_iss_info)
    res.fun          := dpt.isa
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

      val ito_readOp  = Flipped( new iss_readOp_info)

  })



  val bru_iss_fifo = {
    val mdl = Queue( new Bru_iss_info, 4, pipe = true )
    mdl.io.deq <> io.bru_iss_exe
    mdl 
  }

  val csr_iss_fifo = {
    val mdl = Queue( new Csr_iss_info, 4, pipe = true )
    mdl.io.deq <> io.csr_iss_exe
    mdl 
  }


  io.ito_readOp.reg.valid := io.ito_dpt_iss.valid
  io.ito_readOp.reg.bits  := io.ito_dpt_iss.bits.phy

  bru_iss_fifo.io.enq.valid := io.ito_readOp.reg.fire & io.ito_dpt_iss.bits.isa.bru_isa.is_bru
  csr_iss_fifo.io.enq.valid := io.ito_readOp.reg.fire & io.ito_dpt_iss.bits.isa.csr_isa.is_csr

  bru_iss_fifo.io.enq.bits := Mux( bru_iss_fifo.io.enq.valid, Pkg_bru_iss(io.ito_readOp, io.ito_dpt_iss.bits), DontCare )
  csr_iss_fifo.io.enq.bits := Mux( csr_iss_fifo.io.enq.valid, Pkg_csr_iss(io.ito_readOp, io.ito_dpt_iss.bits), DontCare )


  io.ito_dpt_iss.ready := io.ito_readOp.reg.ready & bru_iss_fifo.io.enq.ready & csr_iss_fifo.io.enq.ready



  def Pkg_bru_iss(op: iss_readOp_info, dpt: Dpt_info): Bru_iss_info = {
    val res = Wire(new Bru_iss_info)
    res.fun  := dpt.isa
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

    res.fun.rc  := dpt.isa.rc | dpt.isa.rci
    res.fun.rs  := dpt.isa.rs | dpt.isa.rsi
    res.fun.rw  := dpt.isa.rw | dpt.isa.rwi

    res.param.dat.op1 := 
      Mux1H(Seq(
        dpt.isa.rw  -> src1, dpt.isa.rwi -> dpt.param.raw_rs1,
        dpt.isa.rs  -> src1, dpt.isa.rsi -> dpt.param.raw_rs1,
        dpt.isa.rc  -> src1, dpt.isa.rci -> dpt.param.raw_rs1
      ))

    res.param.dat.op2 := dpt.param.imm
    res.param.rd0    := dpt.phy.rd0
    res.param.is_iwb := true.B
    res.param.is_fwb := false.B
    return res
  }

}

