
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
import rift2Chip._
import chipsalliance.rocketchip.config._


abstract class IssueBase()(implicit p: Parameters) extends RiftModule with HasFPUParameters{
  val io = IO(new Bundle{
    val sig_dpt_iss = (if( opChn == 1 ) {Some(Vec(1, Flipped(new DecoupledIO(new Dpt_info))))} else {None})
    val sig_readOp  = (if( opChn == 1 ) {Some(Vec(1,  new iss_readOp_info(dw=64)))}            else {None})

    val ooo_dpt_iss = (if( opChn > 1 ) {Some(Vec(opChn/2, Flipped(new DecoupledIO(new Dpt_info))))} else {None})
    val ooo_readOp  = (if( opChn > 1 ) {Some(Vec(opChn/2,  new iss_readOp_info(dw=64)))}            else {None})

    val alu_iss_exe = new DecoupledIO(new Alu_iss_info)
    val mul_iss_exe = new DecoupledIO(new Mul_iss_info)

    val ito_dpt_iss = (if( opChn > 1 ) {Some(Vec(opChn/2, Flipped(new DecoupledIO(new Dpt_info))))} else {None})
    val ito_readOp  = (if( opChn > 1 ) {Some(Vec(opChn/2,  new iss_readOp_info(dw=64)))}            else {None})

    val bru_iss_exe = new DecoupledIO(new Bru_iss_info)
    val csr_iss_exe = new DecoupledIO(new Csr_iss_info)
    val lsu_iss_exe = new DecoupledIO(new Lsu_iss_info)
    val fpu_iss_exe = new DecoupledIO(new Fpu_iss_info)


    val frg_readOp  = Vec((if( opChn > 1 ) {opChn/2} else {1}),  new iss_readOp_info(dw=65))


    val flush = Input(Bool())
  })

  val alu_iss_rePort = Module(new RePort( new Alu_iss_info, port=(if ( opChn > 1 ) {opChn/2} else {1}) ))
  val mul_iss_rePort = Module(new RePort( new Mul_iss_info, port=(if ( opChn > 1 ) {opChn/2} else {1}) ))

  val alu_iss_fifo = {
    val mdl = Module(new MultiPortFifo( new Alu_iss_info, aw = (if(!isMinArea) 4 else 1), in =(if ( opChn > 1 ) {opChn/2} else {1}), out = 1 ))
    mdl.io.enq <> alu_iss_rePort.io.deq
    mdl.io.deq(0) <> io.alu_iss_exe
    mdl.io.flush := io.flush
    mdl 
  }

  val mul_iss_fifo = {
    val mdl = Module(new MultiPortFifo( new Mul_iss_info, aw = (if(!isMinArea) 4 else 1), in =(if ( opChn > 1 ) {opChn/2} else {1}), out = 1 ))
    if( hasMulDiv ) {
      mdl.io.enq <> mul_iss_rePort.io.deq
      mdl.io.deq(0) <> io.mul_iss_exe
      mdl.io.flush := io.flush
      mdl       
    } else {
      mdl.io.enq.map{ _.valid := false.B }
      mdl.io.enq.map{ _.bits  := DontCare }
      mul_iss_rePort.io.deq.map{_.ready := true.B}
      mdl.io.flush := io.flush
      mdl.io.deq(0).ready := true.B
      io.mul_iss_exe.valid := false.B
      io.mul_iss_exe.bits := DontCare
    }

  }

  val bru_iss_rePort = Module(new RePort( new Bru_iss_info, port=(if ( opChn > 1 ) {opChn/2} else {1})))
  val csr_iss_rePort = Module(new RePort( new Csr_iss_info, port=(if ( opChn > 1 ) {opChn/2} else {1})))
  val lsu_iss_rePort = Module(new RePort( new Lsu_iss_info, port=(if ( opChn > 1 ) {opChn/2} else {1})))
  val fpu_iss_rePort = Module(new RePort( new Fpu_iss_info, port=(if ( opChn > 1 ) {opChn/2} else {1})))

  val bru_iss_fifo = {
    val mdl = Module(new MultiPortFifo( new Bru_iss_info, (if(!isMinArea) 4 else 1), in = (if ( opChn > 1 ) {opChn/2} else {1}), out = 1 ))
    mdl.io.enq <> bru_iss_rePort.io.deq
    mdl.io.deq(0) <> io.bru_iss_exe
    mdl.io.flush := io.flush
    mdl 
  }

  val csr_iss_fifo = {
    val mdl = Module(new MultiPortFifo( new Csr_iss_info, (if(!isMinArea) 4 else 1), in =(if ( opChn > 1 ) {opChn/2} else {1}), out = 1 ))
    mdl.io.enq <> csr_iss_rePort.io.deq
    mdl.io.deq(0) <> io.csr_iss_exe
    mdl.io.flush := io.flush
    mdl 
  }

  val lsu_iss_fifo = {
    val mdl = Module(new MultiPortFifo( new Lsu_iss_info, (if(!isMinArea) 4 else 1), in =(if ( opChn > 1 ) {opChn/2} else {1}), out = 1 ))
    mdl.io.enq <> lsu_iss_rePort.io.deq
    mdl.io.deq(0) <> io.lsu_iss_exe
    mdl.io.flush := io.flush
    mdl 
  }

  val fpu_iss_fifo = {
    val mdl = Module(new MultiPortFifo( new Fpu_iss_info, (if(!isMinArea) 4 else 1), in =(if ( opChn > 1 ) {opChn/2} else {1}), out = 1 ))
    mdl.io.enq <> fpu_iss_rePort.io.deq
    mdl.io.deq(0) <> io.fpu_iss_exe
    mdl.io.flush := io.flush
    mdl 
  }


  def Pkg_bru_iss(op: iss_readOp_info, dpt: Dpt_info): Bru_iss_info = {
    val res = Wire(new Bru_iss_info)
    res.fun  := dpt.bru_isa
    res.param.is_rvc   := dpt.param.is_rvc
    res.param.pc    := extVaddr(dpt.param.pc, vlen)
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
        dpt.alu_isa.lui    -> 0.U,  dpt.alu_isa.auipc  -> extVaddr(dpt.param.pc, vlen),
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

  def Pkg_lsu_iss(Xop: iss_readOp_info, Fop: iss_readOp_info, dpt: Dpt_info): Lsu_iss_info = {
    val res = Wire(new Lsu_iss_info)

    res.fun := dpt.lsu_isa

    res.param.dat.op1 := 
      Mux( (dpt.lsu_isa.is_lrsc | dpt.lsu_isa.is_amo), Xop.dat.op1,  (Xop.dat.op1.asSInt + dpt.param.imm.asSInt()).asUInt() )
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



trait IssueOoo{ this: IssueBase =>
    require( opChn > 1 )
  for ( i <- 0 until opChn/2 ) yield {
    io.ooo_readOp.get(i).reg.valid := io.ooo_dpt_iss.get(i).valid
    io.ooo_readOp.get(i).reg.bits  := io.ooo_dpt_iss.get(i).bits.phy

    alu_iss_rePort.io.enq(i).valid := io.ooo_readOp.get(i).reg.fire & io.ooo_dpt_iss.get(i).bits.alu_isa.is_alu
    mul_iss_rePort.io.enq(i).valid := io.ooo_readOp.get(i).reg.fire & io.ooo_dpt_iss.get(i).bits.mul_isa.is_mulDiv

    alu_iss_rePort.io.enq(i).bits := Pkg_alu_iss(io.ooo_readOp.get(i), io.ooo_dpt_iss.get(i).bits)
    mul_iss_rePort.io.enq(i).bits := Pkg_mul_iss(io.ooo_readOp.get(i), io.ooo_dpt_iss.get(i).bits)

    io.ooo_dpt_iss.get(i).ready :=
      io.ooo_readOp.get(i).reg.ready & 
      Mux1H(Seq(
        io.ooo_dpt_iss.get(i).bits.alu_isa.is_alu    -> alu_iss_rePort.io.enq(i).ready,
        io.ooo_dpt_iss.get(i).bits.mul_isa.is_mulDiv -> mul_iss_rePort.io.enq(i).ready,
      ))


    when( io.ooo_dpt_iss.get(i).fire ) {
      assert( io.ooo_readOp.get(i).reg.fire )
      assert( PopCount( Seq(alu_iss_rePort.io.enq(i).fire, mul_iss_rePort.io.enq(i).fire) ) === 1.U )    
    }

  }

}


trait IssueIto{ this: IssueBase => 
  require( opChn > 1 )
  val isItoBlock = WireDefault( VecInit( Seq.fill(opChn/2)(false.B) ) )

  for( i <- 1 until opChn/2 ) {
    for ( j <- 0 until i ) {
      when( (io.ito_dpt_iss.get(i).bits.dptRegion === io.ito_dpt_iss.get(j).bits.dptRegion) &
             ~io.ito_dpt_iss.get(j).fire
      ) {
        isItoBlock(i) := true.B
      }
    }
  }


  for ( i <- 0 until opChn/2 ) yield {
    io.ito_readOp.get(i).reg.valid := io.ito_dpt_iss.get(i).valid
    io.ito_readOp.get(i).reg.bits  := io.ito_dpt_iss.get(i).bits.phy

    io.frg_readOp(i).reg.valid := false.B
    io.frg_readOp(i).reg.bits.rs1  := (regNum-1).U
    io.frg_readOp(i).reg.bits.rs2  := (regNum-1).U
    io.frg_readOp(i).reg.bits.rs3  := (regNum-1).U
  }


  for ( i <- opChn/2-1 to 0 by -1 ) yield {
    when( io.ito_dpt_iss.get(i).bits.lsu_isa.is_lsu ) {//override
      io.ito_readOp.get(i).reg.bits.rs1  := io.ito_dpt_iss.get(i).bits.phy.rs1
      io.ito_readOp.get(i).reg.bits.rs2  := Mux( io.ito_dpt_iss.get(i).bits.lsu_isa.is_fst, (regNum-1).U, io.ito_dpt_iss.get(i).bits.phy.rs2 )
      io.ito_readOp.get(i).reg.bits.rs3  := (regNum-1).U
      if( hasFpu ) { //the first chn will be mux in
        io.frg_readOp(i).reg.valid     := io.ito_dpt_iss.get(i).valid
        io.frg_readOp(i).reg.bits.rs1  := (regNum-1).U
        io.frg_readOp(i).reg.bits.rs2  := Mux( io.ito_dpt_iss.get(i).bits.lsu_isa.is_fst, io.ito_dpt_iss.get(i).bits.phy.rs2, (regNum-1).U )
        io.frg_readOp(i).reg.bits.rs3  := (regNum-1).U
      }
    }

    if( hasFpu ) {
      when( io.ito_dpt_iss.get(i).bits.fpu_isa.is_fpu ) {//override
      
        io.ito_readOp.get(i).reg.bits.rs1  := Mux( io.ito_dpt_iss.get(i).bits.fpu_isa.is_fop, (regNum-1).U, io.ito_dpt_iss.get(i).bits.phy.rs1 )
        io.ito_readOp.get(i).reg.bits.rs2  := Mux( io.ito_dpt_iss.get(i).bits.fpu_isa.is_fop, (regNum-1).U, io.ito_dpt_iss.get(i).bits.phy.rs2 )
        io.ito_readOp.get(i).reg.bits.rs3  := (regNum-1).U

        //the first chn will be mux in
        io.frg_readOp(i).reg.valid     := io.ito_dpt_iss.get(i).valid
        io.frg_readOp(i).reg.bits.rs1  := Mux( io.ito_dpt_iss.get(i).bits.fpu_isa.is_fop, io.ito_dpt_iss.get(i).bits.phy.rs1, (regNum-1).U )
        io.frg_readOp(i).reg.bits.rs2  := Mux( io.ito_dpt_iss.get(i).bits.fpu_isa.is_fop, io.ito_dpt_iss.get(i).bits.phy.rs2, (regNum-1).U )
        io.frg_readOp(i).reg.bits.rs3  := Mux( io.ito_dpt_iss.get(i).bits.fpu_isa.is_fop, io.ito_dpt_iss.get(i).bits.phy.rs3, (regNum-1).U )
      }
    }
  }




  for ( i <- 0 until opChn/2 ) yield {

    bru_iss_rePort.io.enq(i).valid := ~isItoBlock(i) & io.ito_readOp.get(i).reg.fire & io.ito_dpt_iss.get(i).bits.bru_isa.is_bru
    csr_iss_rePort.io.enq(i).valid := ~isItoBlock(i) & io.ito_readOp.get(i).reg.fire & io.ito_dpt_iss.get(i).bits.csr_isa.is_csr
    lsu_iss_rePort.io.enq(i).valid := ~isItoBlock(i) & io.ito_readOp.get(i).reg.fire & ( if(hasFpu) {io.frg_readOp(i).reg.fire} else {true.B}) & io.ito_dpt_iss.get(i).bits.lsu_isa.is_lsu
    fpu_iss_rePort.io.enq(i).valid :=  (
      if (hasFpu) { ~isItoBlock(i) & io.ito_readOp.get(i).reg.fire & io.frg_readOp(i).reg.fire & io.ito_dpt_iss.get(i).bits.fpu_isa.is_fpu }
      else { false.B }      
    )


    bru_iss_rePort.io.enq(i).bits := Pkg_bru_iss(io.ito_readOp.get(i), io.ito_dpt_iss.get(i).bits)
    csr_iss_rePort.io.enq(i).bits := Pkg_csr_iss(io.ito_readOp.get(i), io.ito_dpt_iss.get(i).bits)
    lsu_iss_rePort.io.enq(i).bits := Pkg_lsu_iss(io.ito_readOp.get(i), io.frg_readOp(i), io.ito_dpt_iss.get(i).bits)
    fpu_iss_rePort.io.enq(i).bits := (
      if (hasFpu) { Pkg_fpu_iss(io.ito_readOp.get(i), io.frg_readOp(i), io.ito_dpt_iss.get(i).bits) }
      else { 0.U.asTypeOf(new Fpu_iss_info) }
    )




    io.ito_dpt_iss.get(i).ready :=
      ~isItoBlock(i) & io.ito_readOp.get(i).reg.ready & 
      Mux1H(Seq(
        (io.ito_dpt_iss.get(i).bits.bru_isa.is_bru) -> ( bru_iss_rePort.io.enq(i).ready ),
        (io.ito_dpt_iss.get(i).bits.csr_isa.is_csr) -> ( csr_iss_rePort.io.enq(i).ready ),
        (io.ito_dpt_iss.get(i).bits.lsu_isa.is_lsu) -> ( lsu_iss_rePort.io.enq(i).ready & io.frg_readOp(i).reg.ready ),
        (io.ito_dpt_iss.get(i).bits.fpu_isa.is_fpu) -> ( (if ( hasFpu ) {fpu_iss_rePort.io.enq(i).ready & io.frg_readOp(i).reg.ready} else {true.B}) ),
      ))


    when( io.ito_dpt_iss.get(i).fire ) {
      assert( io.ito_readOp.get(i).reg.fire )
      assert( PopCount( Seq(
        bru_iss_rePort.io.enq(i).fire,
        csr_iss_rePort.io.enq(i).fire,
        lsu_iss_rePort.io.enq(i).fire,
        fpu_iss_rePort.io.enq(i).fire) ) === 1.U )   

      if(hasFpu) {
        when( io.ito_dpt_iss.get(i).bits.lsu_isa.is_lsu ) { assert( io.frg_readOp(i).reg.fire ) }
        when( io.ito_dpt_iss.get(i).bits.fpu_isa.is_fpu ) { assert( io.frg_readOp(i).reg.fire ) }
      }
    }
  }

}

trait IssueSig{ this: IssueBase => 
  require( opChn == 1 )
  require( hasFpu == false )

  io.sig_readOp.get(0).reg.valid := io.sig_dpt_iss.get(0).valid
  io.sig_readOp.get(0).reg.bits  := io.sig_dpt_iss.get(0).bits.phy

  io.frg_readOp(0).reg.valid := false.B
  io.frg_readOp(0).reg.bits.rs1  := (regNum-1).U
  io.frg_readOp(0).reg.bits.rs2  := (regNum-1).U
  io.frg_readOp(0).reg.bits.rs3  := (regNum-1).U




  when( io.sig_dpt_iss.get(0).bits.lsu_isa.is_lsu ) {//override
    io.sig_readOp.get(0).reg.bits.rs1  := io.sig_dpt_iss.get(0).bits.phy.rs1
    io.sig_readOp.get(0).reg.bits.rs2  := io.sig_dpt_iss.get(0).bits.phy.rs2
    io.sig_readOp.get(0).reg.bits.rs3  := (regNum-1).U
  }

  alu_iss_rePort.io.enq(0).valid := io.sig_readOp.get(0).reg.fire & io.sig_dpt_iss.get(0).bits.alu_isa.is_alu
  mul_iss_rePort.io.enq(0).valid := io.sig_readOp.get(0).reg.fire & io.sig_dpt_iss.get(0).bits.mul_isa.is_mulDiv
  bru_iss_rePort.io.enq(0).valid := io.sig_readOp.get(0).reg.fire & io.sig_dpt_iss.get(0).bits.bru_isa.is_bru
  csr_iss_rePort.io.enq(0).valid := io.sig_readOp.get(0).reg.fire & io.sig_dpt_iss.get(0).bits.csr_isa.is_csr
  lsu_iss_rePort.io.enq(0).valid := io.sig_readOp.get(0).reg.fire & io.sig_dpt_iss.get(0).bits.lsu_isa.is_lsu
  fpu_iss_rePort.io.enq(0).valid := false.B    

  alu_iss_rePort.io.enq(0).bits := Pkg_alu_iss(io.sig_readOp.get(0), io.sig_dpt_iss.get(0).bits)
  mul_iss_rePort.io.enq(0).bits := Pkg_mul_iss(io.sig_readOp.get(0), io.sig_dpt_iss.get(0).bits)
  bru_iss_rePort.io.enq(0).bits := Pkg_bru_iss(io.sig_readOp.get(0), io.sig_dpt_iss.get(0).bits)
  csr_iss_rePort.io.enq(0).bits := Pkg_csr_iss(io.sig_readOp.get(0), io.sig_dpt_iss.get(0).bits)
  lsu_iss_rePort.io.enq(0).bits := Pkg_lsu_iss(io.sig_readOp.get(0), io.frg_readOp(0), io.sig_dpt_iss.get(0).bits)
  fpu_iss_rePort.io.enq(0).bits := 0.U.asTypeOf(new Fpu_iss_info)



  io.sig_dpt_iss.get(0).ready :=
    io.sig_readOp.get(0).reg.ready & 
    Mux1H(Seq(
      (io.sig_dpt_iss.get(0).bits.alu_isa.is_alu   ) -> ( alu_iss_rePort.io.enq(0).ready ),
      (io.sig_dpt_iss.get(0).bits.mul_isa.is_mulDiv) -> ( mul_iss_rePort.io.enq(0).ready ),
      (io.sig_dpt_iss.get(0).bits.bru_isa.is_bru   ) -> ( bru_iss_rePort.io.enq(0).ready ),
      (io.sig_dpt_iss.get(0).bits.csr_isa.is_csr   ) -> ( csr_iss_rePort.io.enq(0).ready ),
      (io.sig_dpt_iss.get(0).bits.lsu_isa.is_lsu   ) -> ( lsu_iss_rePort.io.enq(0).ready ),
    ))


  when( io.sig_dpt_iss.get(0).fire ) {
    assert( io.sig_readOp.get(0).reg.fire )
    assert( PopCount( Seq(
      alu_iss_rePort.io.enq(0).fire,
      mul_iss_rePort.io.enq(0).fire,
      bru_iss_rePort.io.enq(0).fire,
      csr_iss_rePort.io.enq(0).fire,
      lsu_iss_rePort.io.enq(0).fire) ) === 1.U )
  }


}


class Issue(implicit p: Parameters) extends IssueBase {

  if( hasFpu ) {

  } else {
    if( opChn > 1 ) {
      for ( i <- 0 until opChn/2 ) {assert( ~(io.ito_dpt_iss.get(i).valid & io.ito_dpt_iss.get(i).bits.fpu_isa.is_fpu) )}
      assert( ~io.fpu_iss_exe.valid )      
    } else {
      assert( ~(io.sig_dpt_iss.get(0).valid & io.sig_dpt_iss.get(0).bits.fpu_isa.is_fpu) )
      assert( ~io.fpu_iss_exe.valid )     
    }
  }
  if( hasMulDiv ){}
  else {
    if( opChn > 1 ) {
      for ( i <- 0 until opChn/2 ) {assert( ~(io.ito_dpt_iss.get(i).valid & io.ito_dpt_iss.get(i).bits.mul_isa.is_mulDiv) )}   
    } else {
      assert( ~(io.sig_dpt_iss.get(0).valid & io.sig_dpt_iss.get(0).bits.mul_isa.is_mulDiv) )  
    }   
  }
}
