
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

package rift2Core.backend.fpu

import chisel3._
import chisel3.util._
import base._
import rift2Core.define._
import rift2Core.backend._
import rift2Core.privilege._
import rift2Chip._
import org.chipsalliance.cde.config._


class Exc_Info(implicit p: Parameters) extends Fpu_iss_info { val exc = UInt(5.W) }
class Fres_Info(implicit p: Parameters) extends Exc_Info { val toFloat = UInt(65.W) }
class Xres_Info(implicit p: Parameters) extends Exc_Info { val toInt = UInt(64.W) }

class FAluIO(implicit p: Parameters) extends RiftBundle{
  val fpu_iss_exe = Flipped(DecoupledIO(new Fpu_iss_info))
  val fpu_exe_iwb = DecoupledIO(new WriteBack_info(dw=65))
  val fpu_exe_fwb = DecoupledIO(new WriteBack_info(dw=65))
  val fpu_cWriteBack = Valid(new SeqReg_WriteBack_Bundle(64, cRegNum))
  val flush = Input(Bool())
}


class FAlu(latency: Int = 5, infly: Int = 8)(implicit p: Parameters) extends RiftModule with HasFPUParameters{
  val io: FAluIO = IO(new FAluIO)

  val fpu_exe_iwb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info(dw=65), infly ) )
    mdl.io.deq <> io.fpu_exe_iwb
    mdl.reset := io.flush | reset.asBool
    mdl
  }
  val fpu_exe_fwb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info(dw=65), infly ) )
    mdl.io.deq <> io.fpu_exe_fwb
    mdl.reset := io.flush | reset.asBool
    mdl
  }

  val fpu_exe_csr_fifo = {
    val mdl = Module( new Queue( new SeqReg_WriteBack_Bundle(64, cRegNum), infly ) )
    io.fpu_cWriteBack.valid := mdl.io.deq.valid
    io.fpu_cWriteBack.bits  := mdl.io.deq.bits
    mdl.io.deq.ready        := true.B
    mdl.reset := io.flush | reset.asBool
    mdl
  }

  // val exc = Wire(UInt(5.W))

  val divSqrt = {
    val mdl = Module(new FDivSqrt())
    mdl.io.in.valid := io.fpu_iss_exe.valid
    mdl.io.in.bits := io.fpu_iss_exe.bits

    mdl
  }


  val f2i = {
    val mdl = Module(new FPToInt(latency))
    mdl.io.in.valid := io.fpu_iss_exe.valid
    mdl.io.in.bits := io.fpu_iss_exe.bits
    mdl.reset := io.flush | reset.asBool
    mdl
  }

  val i2f = {
    val mdl = Module(new IntToFP(latency))
    mdl.io.in.valid := io.fpu_iss_exe.valid
    mdl.io.in.bits := io.fpu_iss_exe.bits
    mdl.reset := io.flush | reset.asBool
    mdl
  }

  val f2f = {
    val mdl = Module(new FPToFP(latency))
    mdl.io.in.valid := io.fpu_iss_exe.valid
    mdl.io.in.bits := io.fpu_iss_exe.bits
    mdl.reset := io.flush | reset.asBool
    mdl
  }

  val sfma = {
    val mdl = Module(new FPUFMAPipe(latency, FType.S))
    mdl.io.in.valid := io.fpu_iss_exe.valid & io.fpu_iss_exe.bits.fun.FtypeTagIn === 0.U
    mdl.io.in.bits := io.fpu_iss_exe.bits
    mdl.reset := io.flush | reset.asBool
    mdl
  }


  val dfma = {
    val mdl = Module(new FPUFMAPipe(latency, FType.D))
    mdl.io.in.valid := io.fpu_iss_exe.valid & io.fpu_iss_exe.bits.fun.FtypeTagIn === 1.U
    mdl.io.in.bits := io.fpu_iss_exe.bits
    mdl.reset := io.flush | reset.asBool
    mdl
  }

  // exc := 


  fpu_exe_iwb_fifo.io.enq.valid := f2i.io.out.valid

  fpu_exe_iwb_fifo.io.enq.bits.res :=
    Mux1H(Seq(
      f2i.io.out.valid -> f2i.io.out.bits.toInt,
    )
    )
  fpu_exe_iwb_fifo.io.enq.bits.rd0 :=
    Mux1H(Seq(
      f2i.io.out.valid -> f2i.io.out.bits.param.rd0,
    ))

  fpu_exe_fwb_fifo.io.enq.valid := i2f.io.out.valid | f2f.io.out.valid | sfma.io.out.valid | dfma.io.out.valid | divSqrt.io.out.valid


  fpu_exe_fwb_fifo.io.enq.bits.res := 
    Mux1H(Seq(
      i2f.io.out.valid  -> i2f.io.out.bits.toFloat,
      f2f.io.out.valid  -> f2f.io.out.bits.toFloat,
      sfma.io.out.valid -> sfma.io.out.bits.toFloat,
      dfma.io.out.valid -> dfma.io.out.bits.toFloat,
      divSqrt.io.out.valid -> divSqrt.io.out.bits.toFloat,

    ))
  fpu_exe_fwb_fifo.io.enq.bits.rd0 :=
    Mux1H(Seq(
      i2f.io.out.valid -> i2f.io.out.bits.param.rd0,
      f2f.io.out.valid -> f2f.io.out.bits.param.rd0,
      sfma.io.out.valid -> sfma.io.out.bits.param.rd0,
      dfma.io.out.valid -> dfma.io.out.bits.param.rd0,
      divSqrt.io.out.valid -> divSqrt.io.out.bits.param.rd0,
    ))

  io.fpu_iss_exe.ready := ~(io.fpu_iss_exe.bits.fun.is_fun_divSqrt & divSqrt.io.pending)


  fpu_exe_csr_fifo.io.enq.valid       := i2f.io.out.valid | f2f.io.out.valid | sfma.io.out.valid | dfma.io.out.valid | divSqrt.io.out.valid | f2i.io.out.valid
  fpu_exe_csr_fifo.io.enq.bits.addr  := "h001".U
  fpu_exe_csr_fifo.io.enq.bits.dati  := 
    Mux1H(Seq(
      f2i.io.out.valid      -> f2i.io.out.bits.exc,
      i2f.io.out.valid      -> i2f.io.out.bits.exc,
      f2f.io.out.valid      -> f2f.io.out.bits.exc,
      sfma.io.out.valid     -> sfma.io.out.bits.exc,
      dfma.io.out.valid     -> dfma.io.out.bits.exc,
      divSqrt.io.out.valid  -> divSqrt.io.out.bits.exc,
    ))

  fpu_exe_csr_fifo.io.enq.bits.op_rw := false.B
  fpu_exe_csr_fifo.io.enq.bits.op_rs := true.B
  fpu_exe_csr_fifo.io.enq.bits.op_rc := false.B
  fpu_exe_csr_fifo.io.enq.bits.idx   :=
    Mux1H(Seq(
      f2i.io.out.valid      -> f2i.io.out.bits.param.csrw(log2Ceil(cRegNum)-1, 0),
      i2f.io.out.valid      -> i2f.io.out.bits.param.csrw(log2Ceil(cRegNum)-1, 0),
      f2f.io.out.valid      -> f2f.io.out.bits.param.csrw(log2Ceil(cRegNum)-1, 0),
      sfma.io.out.valid     -> sfma.io.out.bits.param.csrw(log2Ceil(cRegNum)-1, 0),
      dfma.io.out.valid     -> dfma.io.out.bits.param.csrw(log2Ceil(cRegNum)-1, 0),
      divSqrt.io.out.valid  -> divSqrt.io.out.bits.param.csrw(log2Ceil(cRegNum)-1, 0),
    ))



  assert( ~(fpu_exe_iwb_fifo.io.enq.valid & ~fpu_exe_iwb_fifo.io.enq.ready), "Assert Failed, the pipeline assert all fifo enq will success" )
  assert( ~(fpu_exe_fwb_fifo.io.enq.valid & ~fpu_exe_fwb_fifo.io.enq.ready), "Assert Failed, the pipeline assert all fifo enq will success" )
  assert( ~(fpu_exe_csr_fifo.io.enq.valid & ~fpu_exe_csr_fifo.io.enq.ready), "Assert Failed, the pipeline assert all fifo enq will success" )

}


class FakeFAlu(implicit p: Parameters) extends RiftModule with HasFPUParameters{

  val io: FAluIO = IO(new FAluIO)

  io.fpu_iss_exe.ready := true.B

  io.fpu_exe_iwb.valid := false.B
  io.fpu_exe_iwb.bits  := 0.U.asTypeOf(new WriteBack_info(dw=65))

  io.fpu_exe_fwb.valid := false.B
  io.fpu_exe_fwb.bits  := 0.U.asTypeOf(new WriteBack_info(dw=65))

  io.fpu_cWriteBack.valid := false.B
  
  io.fpu_cWriteBack.bits.addr  := 0.U
  io.fpu_cWriteBack.bits.dati  := 0.U
  io.fpu_cWriteBack.bits.op_rw := false.B
  io.fpu_cWriteBack.bits.op_rs := false.B
  io.fpu_cWriteBack.bits.op_rc := false.B
  io.fpu_cWriteBack.bits.idx   := 0.U

  assert( ~io.fpu_iss_exe.valid )
}
