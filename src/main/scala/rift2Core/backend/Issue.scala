
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
    // val alu_dpt_iss = Vec(rn_chn, Flipped(new DecoupledIO(new Dpt_info)))
    // val lsu_dpt_iss = Vec(rn_chn, Flipped(new DecoupledIO(new Dpt_info)))
    // val mul_dpt_iss = Vec(rn_chn, Flipped(new DecoupledIO(new Dpt_info)))

    // val alu_iss_exe = Vec(rop_chn, new DecoupledIO(new Alu_iss_info))
    // val lsu_iss_exe = Vec(rop_chn, new DecoupledIO(new Lsu_iss_info))
    // val mul_iss_exe = Vec(rop_chn, new DecoupledIO(new Mul_iss_info))

    val ooo_dpt_iss = Vec(rn_chn, Flipped(new DecoupledIO(new Dpt_info)))
    val alu_iss_exe = new DecoupledIO(new Alu_iss_info)
    val lsu_iss_exe = new DecoupledIO(new Lsu_iss_info)
    val mul_iss_exe = new DecoupledIO(new Mul_iss_info)


    val ooo_readOp  = Vec(4, Flipped( new iss_readOp_info))

  })

  // val alu_dpt_fifo = {
  //   val mdl = MultiPortFifo( new Dpt_info, aw = 4, in = rn_chn, out = 1 )
  //   mdl.io.enq <> io.alu_dpt_iss
  //   mdl
  // }

  // val alu_iss_fifo = {
  //   val mdl = MultiPortFifo( new Alu_iss_info, aw = 4, in = 1, out = rop_chn )
  //   mdl.io.deq <> io.alu_iss_exe
  //   mdl 
  // }

  // val lsu_dpt_fifo = {
  //   val mdl = MultiPortFifo( new Dpt_info, aw = 4, in = rn_chn, out = 1 )
  //   mdl.io.enq <> io.lsu_dpt_iss
  //   mdl
  // }

  // val lsu_iss_fifo = {
  //   val mdl = MultiPortFifo( new Lsu_iss_info, aw = 4, in = 1, out = rop_chn )
  //   mdl.io.deq <> io.lsu_iss_exe
  //   mdl 
  // }

  // val mul_dpt_fifo = {
  //   val mdl = MultiPortFifo( new Dpt_info, aw = 4, in = rn_chn, out = 1 )
  //   mdl.io.enq <> io.mul_dpt_iss
  //   mdl
  // }

  // val mul_iss_fifo = {
  //   val mdl = MultiPortFifo( new Mul_iss_info, aw = 4, in = 1, out = rop_chn )
  //   mdl.io.deq <> io.mul_iss_exe
  //   mdl 
  // }

  val zipQueue = {
    val mdl = Module(new ZipQueue( new Dpt_info, 4, in = rn_chn, out = 4 ))
    mdl.io.enq <> io.ooo_dpt_iss
    mdl
  }


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
    ooo_readOp(i).reg.valid := zipQueue.io.deq(i).valid
    ooo_readOp(i).reg.bits  := zipQueue.io.deq(i).bits.phy

    alu_iss_rePort.io.enq(i).valid := ooo_readOp(i).reg.fire & zipQueue.io.deq(i).bits.isa.alu_isa.is_alu
    lsu_iss_rePort.io.enq(i).valid := ooo_readOp(i).reg.fire & zipQueue.io.deq(i).bits.isa.lsu_isa.is_lsu
    mul_iss_rePort.io.enq(i).valid := ooo_readOp(i).reg.fire & zipQueue.io.deq(i).bits.isa.mul_isa.is_mul



  }

  def Pkg_alu_iss(ooo_readOp,):



  when( alu_iss_fifo.io.enq.valid ) {
    val src1 = ooo_readOp(0).reg.dat.op1
    val src2 = ooo_readOp(0).reg.dat.op2
    alu_iss_fifo.io.enq.bits.fun.add := alu_dpt_fifo.deq.io.bits.isa.is_fun_add
    alu_iss_fifo.io.enq.bits.fun.slt := alu_dpt_fifo.deq.io.bits.isa.is_fun_slt
    alu_iss_fifo.io.enq.bits.fun.xor := alu_dpt_fifo.deq.io.bits.isa.is_fun_xor
    alu_iss_fifo.io.enq.bits.fun.or  := alu_dpt_fifo.deq.io.bits.isa.is_fun_or
    alu_iss_fifo.io.enq.bits.fun.and := alu_dpt_fifo.deq.io.bits.isa.is_fun_and
    alu_iss_fifo.io.enq.bits.fun.sll := alu_dpt_fifo.deq.io.bits.isa.is_fun_sll
    alu_iss_fifo.io.enq.bits.fun.srl := alu_dpt_fifo.deq.io.bits.isa.is_fun_srl
    alu_iss_fifo.io.enq.bits.fun.sra := alu_dpt_fifo.deq.io.bits.isa.is_fun_sra

    alu_iss_fifo.io.enq.bits.param.is_32w  := alu_dpt_fifo.deq.io.bits.isa.is_32w
    alu_iss_fifo.io.enq.bits.param.is_usi  := alu_dpt_fifo.deq.io.bits.isa.is_usi


    alu_iss_fifo.io.enq.param.dat.op1 :=
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

    alu_iss_fifo.io.enq.param.dat.op2 :=
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

    alu_iss_fifo.io.enq.param.idx := alu_dpt_fifo.deq.io.bits.phy.rd
    alu_iss_fifo.io.enq.param.is_iwb := true.B
    alu_iss_fifo.io.enq.param.is_fwb := false.B
  }.otherwise {
    alu_iss_fifo.io.enq.bits := 0.U.asTypeof(new Alu_iss_info)
  }
 
  // zipQueue.io.deq(0).ready := alu_iss_fifo.io.enq.fire




  lsu_iss_fifo.io.enq.valid := ooo_readOp(1).reg.fire

  when( lsu_iss_fifo.io.enq.valid ) {
    lsu_iss_fifo.io.enq.bits.fun := lsu_dpt_fifo.deq.io.bits.isa
    lsu_iss_fifo.io.enq.bits.param.dat := ooo_readOp(1).dat
    io.lsu_iss_info.param.idx := lsu_dpt_fifo.deq.io.bits.phy.rd
    io.lsu_iss_info.param.is_iwb := true.B
    io.lsu_iss_info.param.is_fwb := false.B
  } .otherwise {
    lsu_iss_fifo.io.enq.bits := 0.U.asTypeof(new Lsu_iss_info)
  }
  lsu_dpt_fifo.deq.io.ready := lsu_iss_fifo.io.enq.fire




  mul_iss_fifo.io.enq.valid := ooo_readOp(2).reg.fire 

  when( mul_iss_fifo.io.enq.valid ) {
    mul_iss_fifo.io.enq.bits.fun          := mul_dpt_fifo.deq.io.bits.isa
    mul_iss_fifo.io.enq.bits.param.dat    := mul_dpt_fifo.deq.io.bits.dat
    mul_iss_fifo.io.enq.bits.param.idx    := mul_dpt_fifo.deq.io.bits.phy.rd
    mul_iss_fifo.io.enq.bits.param.is_iwb := true.B
    mul_iss_fifo.io.enq.bits.param.is_fwb := false.B
  } .otherwise {
    mul_iss_fifo.io.enq.bits := 0.U.asTypeof(new Mul_iss_info)
  }
  mul_dpt_fifo.deq.io.ready := mul_iss_fifo.io.enq.fire

}


class In_Order_Issue extends Module {
    val io = IO(new Bundle{
    val bru_dpt_iss = Vec(rn_chn, Flipped(new DecoupledIO(new Dpt_info)))
    val csr_dpt_iss = Vec(rn_chn, Flipped(new DecoupledIO(new Dpt_info)))

    val bru_iss_exe = new DecoupledIO(new Bru_iss_info)
    val csr_iss_exe = new DecoupledIO(new Csr_iss_info)

    val ito_readOp  = Flipped( new iss_readOp_info)

  })

  val bru_dpt_fifo = {
    val mdl = MultiPortFifo( new Dpt_info, aw = 4, in = rn_chn, out = 1 )
    mdl.io.enq <> io.bru_dpt_iss
    mdl
  }

  val bru_iss_fifo = {
    val mdl = Queue( new Bru_iss_info, 4, pipe = true )
    mdl.io.deq <> io.bru_iss_exe
    mdl 
  }



}

