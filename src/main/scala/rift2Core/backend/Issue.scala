
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
import rift._
import chipsalliance.rocketchip.config._



class Out_of_Order_Issue()(implicit p: Parameters) extends RiftModule {

  val io = IO(new Bundle{
    val ooo_dpt_iss = Vec(2, Flipped(new DecoupledIO(new Dpt_info)))
    val alu_iss_exe = new DecoupledIO(new Alu_iss_info)
    val mul_iss_exe = new DecoupledIO(new Mul_iss_info)


    val ooo_readOp  = Vec(2,  new iss_readOp_info(dw=64,dp=64))

  })




  val alu_iss_rePort = {
    val mdl = Module(new RePort( new Alu_iss_info, port=2))
    mdl
  }

  val alu_iss_fifo = {
    val mdl = Module(new MultiPortFifo( new Alu_iss_info, aw = (if(!isMinArea) 4 else 2), in = 2, out = 1 ))
    mdl.io.enq <> alu_iss_rePort.io.deq
    mdl.io.deq(0) <> io.alu_iss_exe
    mdl.io.flush := false.B
    mdl 
  }



  val mul_iss_rePort = {
    val mdl = Module(new RePort( new Mul_iss_info, port=2))
    mdl
  }

  val mul_iss_fifo = {
    val mdl = Module(new MultiPortFifo( new Mul_iss_info, aw = (if(!isMinArea) 4 else 2), in = 2, out = 1 ))
    mdl.io.enq <> mul_iss_rePort.io.deq
    mdl.io.deq(0) <> io.mul_iss_exe
    mdl.io.flush := false.B
    mdl 
  }


  for ( i <- 0 until 2 ) yield {
    io.ooo_readOp(i).reg.valid := io.ooo_dpt_iss(i).valid
    io.ooo_readOp(i).reg.bits  := io.ooo_dpt_iss(i).bits.phy

    alu_iss_rePort.io.enq(i).valid := io.ooo_readOp(i).reg.fire & io.ooo_dpt_iss(i).bits.alu_isa.is_alu
    mul_iss_rePort.io.enq(i).valid := io.ooo_readOp(i).reg.fire & io.ooo_dpt_iss(i).bits.mul_isa.is_mulDiv

    alu_iss_rePort.io.enq(i).bits := Mux( alu_iss_rePort.io.enq(i).valid, Pkg_alu_iss(io.ooo_readOp(i), io.ooo_dpt_iss(i).bits), 0.U.asTypeOf(new Alu_iss_info) )
    mul_iss_rePort.io.enq(i).bits := Mux( mul_iss_rePort.io.enq(i).valid, Pkg_mul_iss(io.ooo_readOp(i), io.ooo_dpt_iss(i).bits), 0.U.asTypeOf(new Mul_iss_info) )

    io.ooo_dpt_iss(i).ready :=
      io.ooo_readOp(i).reg.ready & 
      Mux1H(Seq(
        io.ooo_dpt_iss(i).bits.alu_isa.is_alu -> alu_iss_rePort.io.enq(i).ready,
        io.ooo_dpt_iss(i).bits.mul_isa.is_mulDiv -> mul_iss_rePort.io.enq(i).ready,
      ))


    when( io.ooo_dpt_iss(i).fire ) {
      assert( io.ooo_readOp(i).reg.fire )
      assert( PopCount( Seq(alu_iss_rePort.io.enq(i).fire, mul_iss_rePort.io.enq(i).fire) ) === 1.U )    
    }

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
    res.param.dat.op3 := 0.U
    res.param.rd0 := dpt.phy.rd0
    return res
  }

  def Pkg_mul_iss(op: iss_readOp_info, dpt: Dpt_info): Mul_iss_info = {
    val res = Wire(new Mul_iss_info)
    res.fun := dpt.mul_isa
 
    res.param.dat    := op.dat
    res.param.rd0    := dpt.phy.rd0
    return res
  }

}


class In_Order_Issue(implicit p: Parameters) extends RiftModule with HasFPUParameters{
    val io = IO(new Bundle{
      val bru_dpt_iss = Flipped(new DecoupledIO(new Dpt_info))
      val csr_dpt_iss = Flipped(new DecoupledIO(new Dpt_info))
      val lsu_dpt_iss = Flipped(new DecoupledIO(new Dpt_info))
      val fpu_dpt_iss = Flipped(new DecoupledIO(new Dpt_info))

      val bru_iss_exe = new DecoupledIO(new Bru_iss_info)
      val csr_iss_exe = new DecoupledIO(new Csr_iss_info)
      val lsu_iss_exe = new DecoupledIO(new Lsu_iss_info)
      val fpu_iss_exe = new DecoupledIO(new Fpu_iss_info)

      val bru_readOp  = new iss_readOp_info(dw=64,dp=64)
      val csr_readOp  = new iss_readOp_info(dw=64,dp=64)
      val lsu_readXOp  = new iss_readOp_info(dw=64,dp=64)
      val lsu_readFOp  = new iss_readOp_info(dw=65,dp=64)
      val fpu_readXOp  = new iss_readOp_info(dw=64,dp=64)
      val fpu_readFOp  = new iss_readOp_info(dw=65,dp=64)

  })



  val bru_iss_fifo = {
    val mdl = Module(new Queue( new Bru_iss_info, (if(!isMinArea) 4 else 2), pipe = false, false ))
    mdl.io.deq <> io.bru_iss_exe
    mdl 
  }

  val csr_iss_fifo = {
    val mdl = Module(new Queue( new Csr_iss_info, (if(!isMinArea) 4 else 2), pipe = false, false ))
    mdl.io.deq <> io.csr_iss_exe
    mdl 
  }

  val lsu_iss_fifo = {
    val mdl = Module(new Queue( new Lsu_iss_info, (if(!isMinArea) 4 else 2), pipe = false, false ))
    mdl.io.deq <> io.lsu_iss_exe
    mdl 
  }

  val fpu_iss_fifo = {
    val mdl = Module(new Queue( new Fpu_iss_info, (if(!isMinArea) 4 else 2), pipe = false, false ))
    mdl.io.deq <> io.fpu_iss_exe
    mdl 
  }

  io.bru_readOp.reg.valid := io.bru_dpt_iss.valid
  io.bru_readOp.reg.bits  := io.bru_dpt_iss.bits.phy

  bru_iss_fifo.io.enq.valid := io.bru_readOp.reg.fire
  bru_iss_fifo.io.enq.bits := Mux( bru_iss_fifo.io.enq.valid, Pkg_bru_iss(io.bru_readOp, io.bru_dpt_iss.bits), 0.U.asTypeOf(new Bru_iss_info) )

  io.bru_dpt_iss.ready := io.bru_readOp.reg.ready & bru_iss_fifo.io.enq.ready

  when( io.bru_dpt_iss.fire ) {
    assert( io.bru_readOp.reg.fire )
    assert( bru_iss_fifo.io.enq.fire )
  }




  io.csr_readOp.reg.valid := io.csr_dpt_iss.valid
  io.csr_readOp.reg.bits  := io.csr_dpt_iss.bits.phy

  csr_iss_fifo.io.enq.valid := io.csr_readOp.reg.fire
  csr_iss_fifo.io.enq.bits := Mux( csr_iss_fifo.io.enq.valid, Pkg_csr_iss(io.csr_readOp, io.csr_dpt_iss.bits), 0.U.asTypeOf(new Csr_iss_info) )

  io.csr_dpt_iss.ready := io.csr_readOp.reg.ready & csr_iss_fifo.io.enq.ready

  when( io.csr_dpt_iss.fire ) {
    assert( io.csr_readOp.reg.fire )
    assert( csr_iss_fifo.io.enq.fire )
  }


  io.lsu_readXOp.reg.valid := io.lsu_dpt_iss.valid
  io.lsu_readXOp.reg.bits.rs1  := io.lsu_dpt_iss.bits.phy.rs1
  io.lsu_readXOp.reg.bits.rs2  := Mux( io.lsu_dpt_iss.bits.lsu_isa.is_fst, 63.U, io.lsu_dpt_iss.bits.phy.rs2 )
  io.lsu_readXOp.reg.bits.rs3  := 63.U

  if( hasFpu ) {
    io.lsu_readFOp.reg.valid := io.lsu_dpt_iss.valid
    io.lsu_readFOp.reg.bits.rs1  := 63.U
    io.lsu_readFOp.reg.bits.rs2  := Mux( io.lsu_dpt_iss.bits.lsu_isa.is_fst, io.lsu_dpt_iss.bits.phy.rs2, 63.U )
    io.lsu_readFOp.reg.bits.rs3  := 63.U    
  } else {
    io.lsu_readFOp.reg.valid     := false.B
    io.lsu_readFOp.reg.bits.rs1  := 63.U
    io.lsu_readFOp.reg.bits.rs2  := 63.U
    io.lsu_readFOp.reg.bits.rs3  := 63.U
  }



  lsu_iss_fifo.io.enq.valid :=
    io.lsu_readXOp.reg.fire &
    ( if(hasFpu) {io.lsu_readFOp.reg.fire} else {true.B})

  lsu_iss_fifo.io.enq.bits :=
    Mux(
      lsu_iss_fifo.io.enq.valid,
      Pkg_lsu_iss(io.lsu_readXOp, io.lsu_readFOp, io.lsu_dpt_iss.bits),
      0.U.asTypeOf(new Lsu_iss_info)
    )

  io.lsu_dpt_iss.ready :=
    io.lsu_readXOp.reg.ready & io.lsu_readFOp.reg.ready &
    lsu_iss_fifo.io.enq.ready

  when( io.lsu_dpt_iss.fire ) {
    assert(
      io.lsu_readXOp.reg.fire &
      (if(hasFpu) {io.lsu_readFOp.reg.fire} else {true.B})
    )
    assert( lsu_iss_fifo.io.enq.fire )
  }

  if(hasFpu) {
    io.fpu_readXOp.reg.valid     := io.fpu_dpt_iss.valid
    io.fpu_readXOp.reg.bits.rs1  := Mux( io.fpu_dpt_iss.bits.fpu_isa.is_fop, 63.U, io.fpu_dpt_iss.bits.phy.rs1 )
    io.fpu_readXOp.reg.bits.rs2  := Mux( io.fpu_dpt_iss.bits.fpu_isa.is_fop, 63.U, io.fpu_dpt_iss.bits.phy.rs2 )
    io.fpu_readXOp.reg.bits.rs3  := 63.U
    io.fpu_readFOp.reg.valid     := io.fpu_dpt_iss.valid
    io.fpu_readFOp.reg.bits.rs1  := Mux( io.fpu_dpt_iss.bits.fpu_isa.is_fop, io.fpu_dpt_iss.bits.phy.rs1, 63.U )
    io.fpu_readFOp.reg.bits.rs2  := Mux( io.fpu_dpt_iss.bits.fpu_isa.is_fop, io.fpu_dpt_iss.bits.phy.rs2, 63.U )
    io.fpu_readFOp.reg.bits.rs3  := Mux( io.fpu_dpt_iss.bits.fpu_isa.is_fop, io.fpu_dpt_iss.bits.phy.rs3, 63.U )

    fpu_iss_fifo.io.enq.valid := io.fpu_readXOp.reg.fire & io.fpu_readFOp.reg.fire
    fpu_iss_fifo.io.enq.bits := Mux( fpu_iss_fifo.io.enq.valid, Pkg_fpu_iss(io.fpu_readXOp, io.fpu_readFOp, io.fpu_dpt_iss.bits), 0.U.asTypeOf(new Fpu_iss_info) )

    io.fpu_dpt_iss.ready :=
      io.fpu_readXOp.reg.ready & io.fpu_readFOp.reg.ready & fpu_iss_fifo.io.enq.ready

    when( io.fpu_dpt_iss.fire ) {
      assert( io.fpu_readXOp.reg.fire & io.fpu_readFOp.reg.fire )
      assert( fpu_iss_fifo.io.enq.fire )
    }
  } else {
    io.fpu_readXOp.reg.valid := false.B
    io.fpu_readXOp.reg.bits.rs1  := 63.U
    io.fpu_readXOp.reg.bits.rs2  := 63.U
    io.fpu_readXOp.reg.bits.rs3  := 63.U
    io.fpu_readFOp.reg.valid := false.B
    io.fpu_readFOp.reg.bits.rs1  := 63.U
    io.fpu_readFOp.reg.bits.rs2  := 63.U
    io.fpu_readFOp.reg.bits.rs3  := 63.U

    fpu_iss_fifo.io.enq.valid := false.B
    fpu_iss_fifo.io.enq.bits  := 0.U.asTypeOf(new Fpu_iss_info)

    io.fpu_dpt_iss.ready := true.B

  }


  def Pkg_bru_iss(op: iss_readOp_info, dpt: Dpt_info): Bru_iss_info = {
    val res = Wire(new Bru_iss_info)
    res.fun  := dpt.bru_isa
    res.param.is_rvc   := dpt.param.is_rvc
    res.param.pc   := dpt.param.pc
    res.param.imm   := dpt.param.imm
    res.param.dat   := op.dat
    res.param.rd0    := dpt.phy.rd0

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
    res.param.dat.op3 := 0.U
    res.param.rd0    := dpt.phy.rd0

    return res
  }


  def Pkg_lsu_iss(Xop: iss_readOp_info, Fop: iss_readOp_info, dpt: Dpt_info): Lsu_iss_info = {
    val res = Wire(new Lsu_iss_info)

    res.fun := dpt.lsu_isa

    res.param.dat.op1 := (Xop.dat.op1.asSInt + dpt.param.imm.asSInt()).asUInt()
    res.param.dat.op2 :=
      Mux(
        dpt.lsu_isa.is_fst,
        ieee(unbox(Fop.dat.op2, 1.U, None), t = FType.D),
        Xop.dat.op2
        
      )
    res.param.dat.op3 := 0.U

    res.param.rd0 := dpt.phy.rd0

    return res
  }

  def Pkg_fpu_iss(Xop: iss_readOp_info, Fop: iss_readOp_info, dpt: Dpt_info): Fpu_iss_info = {
    val res = Wire(new Fpu_iss_info)

    res.fun := dpt.fpu_isa

    res.param.dat.op1 :=
      Mux(
        dpt.fpu_isa.is_fop,
        Fop.dat.op1,
        Mux( dpt.fpu_isa.is_fun_fcsri, dpt.param.raw.rs1, Xop.dat.op1 )
      )

    res.param.dat.op2 :=
      Mux(
        dpt.fpu_isa.is_fop,
        Fop.dat.op2,
        Mux( dpt.fpu_isa.is_fun_fcsr, dpt.param.imm, Xop.dat.op2)
      )
    res.param.dat.op3 := Fop.dat.op3

    res.param.rd0 := dpt.phy.rd0

    res.param.rm := dpt.param.rm

    return res
  }
}




class Issue(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val ooo_dpt_iss = Vec(2, Flipped(new DecoupledIO(new Dpt_info)))
    val bru_dpt_iss = Flipped(new DecoupledIO(new Dpt_info))
    val csr_dpt_iss = Flipped(new DecoupledIO(new Dpt_info))
    val lsu_dpt_iss = Flipped(new DecoupledIO(new Dpt_info))
    val fpu_dpt_iss = Flipped(new DecoupledIO(new Dpt_info))

    val ooo_readOp  = Vec(2,  new iss_readOp_info(dw=64,dp=64))
    val bru_readOp  = new iss_readOp_info(dw=64,dp=64)
    val csr_readOp  = new iss_readOp_info(dw=64,dp=64)
    val lsu_readXOp  = new iss_readOp_info(dw=64,dp=64)
    val lsu_readFOp  = new iss_readOp_info(dw=65,dp=64)
    val fpu_readXOp  = new iss_readOp_info(dw=64,dp=64)
    val fpu_readFOp  = new iss_readOp_info(dw=65,dp=64)

    val alu_iss_exe = new DecoupledIO(new Alu_iss_info)
    val lsu_iss_exe = new DecoupledIO(new Lsu_iss_info)
    val mul_iss_exe = new DecoupledIO(new Mul_iss_info)
    val bru_iss_exe = new DecoupledIO(new Bru_iss_info)
    val csr_iss_exe = new DecoupledIO(new Csr_iss_info)
    val fpu_iss_exe = new DecoupledIO(new Fpu_iss_info)

  })


  val ooo_issue = {
    val mdl = Module(new Out_of_Order_Issue)
    mdl.io.ooo_dpt_iss <> io.ooo_dpt_iss
    mdl.io.ooo_readOp  <> io.ooo_readOp
    mdl.io.alu_iss_exe <> io.alu_iss_exe
    mdl.io.mul_iss_exe <> io.mul_iss_exe
    mdl
  }

  val ito_issue = {
    val mdl = Module(new In_Order_Issue)
    mdl.io.bru_dpt_iss <> io.bru_dpt_iss
    mdl.io.csr_dpt_iss <> io.csr_dpt_iss
    mdl.io.lsu_dpt_iss <> io.lsu_dpt_iss
    mdl.io.fpu_dpt_iss <> io.fpu_dpt_iss
    mdl.io.bru_readOp  <> io.bru_readOp
    mdl.io.csr_readOp  <> io.csr_readOp
    mdl.io.lsu_readXOp  <> io.lsu_readXOp
    mdl.io.lsu_readFOp  <> io.lsu_readFOp
    mdl.io.fpu_readXOp  <> io.fpu_readXOp
    mdl.io.fpu_readFOp  <> io.fpu_readFOp
    mdl.io.bru_iss_exe <> io.bru_iss_exe
    mdl.io.csr_iss_exe <> io.csr_iss_exe
    mdl.io.lsu_iss_exe <> io.lsu_iss_exe
    mdl.io.fpu_iss_exe <> io.fpu_iss_exe
    mdl
  }

  if( hasFpu ) {

  } else {
    assert( ~io.fpu_dpt_iss.valid )
    assert( ~io.fpu_iss_exe.valid )
  }
}
