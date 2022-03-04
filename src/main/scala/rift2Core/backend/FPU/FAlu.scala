
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

package rift2Core.backend.fpu

import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._
import rift2Core.privilege.csrFiles._
import chisel3.experimental.dataview._

class Exc_Info extends Fpu_iss_info { val exc = UInt(5.W) }
class Fres_Info extends Exc_Info { val toFloat = UInt(65.W) }
class Xres_Info extends Exc_Info { val toInt = UInt(64.W) }


class FAlu(latency: Int = 5, infly: Int = 8) extends Module with HasFPUParameters{
  val io = IO(new Bundle{
    val fpu_iss_exe = Flipped(DecoupledIO(new Fpu_iss_info))
    val fpu_exe_iwb = DecoupledIO(new WriteBack_info(dw=65, dp=64))
    val fpu_exe_fwb = DecoupledIO(new WriteBack_info(dw=65, dp=64))
    val fcsr_cmm_op = DecoupledIO( new Exe_Port )
    val fcsr = Input(UInt(24.W))


    val flush = Input(Bool())
  })

  val cnt = RegInit(0.U((log2Ceil(infly)).W))
  when( io.flush ) {
    cnt := 0.U
  } .elsewhen( io.fpu_iss_exe.fire & io.fcsr_cmm_op.fire ) {
    cnt := cnt
  } .elsewhen( io.fpu_iss_exe.fire ) {
    cnt := cnt + 1.U
    assert(cnt =/= (infly-1).U)
  } .elsewhen( io.fcsr_cmm_op.fire ) {
    cnt := cnt - 1.U
    assert(cnt =/= 0.U)
  }
  val infly_empty = (cnt === 0.U)
  val infly_full  = (cnt === (infly-1).U)

  val pending_csr = RegInit(false.B)
  when( io.flush | infly_empty  ) { pending_csr := false.B }
  .elsewhen( io.fpu_iss_exe.fire & io.fpu_iss_exe.bits.fun.is_fun_fcsr ) { pending_csr := true.B }


  val fpu_exe_iwb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info(dw=65, dp=64), infly ) )
    mdl.io.deq <> io.fpu_exe_iwb
    mdl.reset := io.flush | reset.asBool
    mdl
  }
  val fpu_exe_fwb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info(dw=65, dp=64), infly ) )
    mdl.io.deq <> io.fpu_exe_fwb
    mdl.reset := io.flush | reset.asBool
    mdl
  }


  val exc = Wire(UInt(5.W))

  val divSqrt = {
    val mdl = Module(new FDivSqrt())
    mdl.io.in.valid := io.fpu_iss_exe.valid & infly_empty
    mdl.io.in.bits := io.fpu_iss_exe.bits
    mdl.io.frm := io.fcsr(7,5) 

    mdl
  }


  val f2i = {
    val mdl = Module(new FPToInt(latency))
    mdl.io.in.valid := io.fpu_iss_exe.valid & ~infly_full & ~pending_csr & ~divSqrt.io.pending
    mdl.io.in.bits := io.fpu_iss_exe.bits
    mdl.io.frm := io.fcsr(7,5)
    mdl.reset := io.flush | reset.asBool
    mdl
  }

  val i2f = {
    val mdl = Module(new IntToFP(latency))
    mdl.io.in.valid := io.fpu_iss_exe.valid & ~infly_full & ~pending_csr & ~divSqrt.io.pending
    mdl.io.in.bits := io.fpu_iss_exe.bits
    mdl.io.frm := io.fcsr(7,5)
    mdl.reset := io.flush | reset.asBool
    mdl
  }

  val f2f = {
    val mdl = Module(new FPToFP(latency))
    mdl.io.in.valid := io.fpu_iss_exe.valid & ~infly_full & ~pending_csr & ~divSqrt.io.pending
    mdl.io.in.bits := io.fpu_iss_exe.bits
    mdl.io.frm := io.fcsr(7,5)
    mdl.reset := io.flush | reset.asBool
    mdl
  }

  val sfma = {
    val mdl = Module(new FPUFMAPipe(latency, FType.S))
    mdl.io.in.valid := io.fpu_iss_exe.valid & io.fpu_iss_exe.bits.fun.FtypeTagIn === 0.U & ~infly_full & ~pending_csr & ~divSqrt.io.pending
    mdl.io.in.bits := io.fpu_iss_exe.bits
    mdl.io.frm := io.fcsr(7,5)    
    mdl
  }


  val dfma = {
    val mdl = Module(new FPUFMAPipe(latency, FType.D))
    mdl.io.in.valid := io.fpu_iss_exe.valid & io.fpu_iss_exe.bits.fun.FtypeTagIn === 1.U & ~infly_full & ~pending_csr & ~divSqrt.io.pending
    mdl.io.in.bits := io.fpu_iss_exe.bits
    mdl.io.frm := io.fcsr(7,5)   
    mdl
  }



  exc := 
    Mux1H(Seq(
      f2i.io.out.valid  -> f2i.io.out.bits.exc,
      i2f.io.out.valid  -> i2f.io.out.bits.exc,
      f2f.io.out.valid  -> f2f.io.out.bits.exc,
      sfma.io.out.valid  -> sfma.io.out.bits.exc,
      dfma.io.out.valid  -> dfma.io.out.bits.exc,
      divSqrt.io.out.valid  -> divSqrt.io.out.bits.exc,
    ))

  val fcsr_op_fifo = {
    val mdl = Module(new Queue( new Exe_Port, infly ) )

    val rw = io.fpu_iss_exe.bits.fun.is_fun_frw
    val rs = 
      Mux(
        f2i.io.out.valid | f2f.io.out.valid | i2f.io.out.valid | sfma.io.out.valid | dfma.io.out.valid | divSqrt.io.out.valid,
        true.B ,
        io.fpu_iss_exe.bits.fun.is_fun_frs
      )
    val rc = io.fpu_iss_exe.bits.fun.is_fun_frc
    
    val dat =
      Mux(
        f2i.io.out.valid | f2f.io.out.valid | i2f.io.out.valid | sfma.io.out.valid | dfma.io.out.valid | divSqrt.io.out.valid,
        exc,
        io.fpu_iss_exe.bits.param.dat.op1
      )
    val addr =
      Mux(
        f2i.io.out.valid | f2f.io.out.valid | i2f.io.out.valid | sfma.io.out.valid | dfma.io.out.valid | divSqrt.io.out.valid,
        1.U,
        io.fpu_iss_exe.bits.param.dat.op2
      )
    val dontWrite = (dat === 0.U) & ( rs | rc )

    mdl.io.deq <> io.fcsr_cmm_op
    mdl.reset := reset.asBool | io.flush

    mdl.io.enq.bits.addr := addr
    mdl.io.enq.bits.dat_i := dat
    mdl.io.enq.bits.op_rw := rw & ~dontWrite
    mdl.io.enq.bits.op_rs := rs & ~dontWrite
    mdl.io.enq.bits.op_rc := rc & ~dontWrite

    mdl
  }
  fcsr_op_fifo.io.enq.valid :=
      (io.fpu_iss_exe.fire & io.fpu_iss_exe.bits.fun.is_fun_fcsr) |
      (f2i.io.out.valid | f2f.io.out.valid | i2f.io.out.valid | sfma.io.out.valid | dfma.io.out.valid | divSqrt.io.out.valid)


  val fcsr_res =
    Mux1H(Seq(
      (io.fpu_iss_exe.bits.param.dat.op2 === 1.U) -> io.fcsr(4,0),
      (io.fpu_iss_exe.bits.param.dat.op2 === 2.U) -> io.fcsr(7,5),
      (io.fpu_iss_exe.bits.param.dat.op2 === 3.U) -> io.fcsr,
    ))





  fpu_exe_iwb_fifo.io.enq.valid := (io.fpu_iss_exe.fire & io.fpu_iss_exe.bits.fun.is_fun_fcsr) | f2i.io.out.valid

  fpu_exe_iwb_fifo.io.enq.bits.res :=
    Mux1H(Seq(
      (io.fpu_iss_exe.fire & io.fpu_iss_exe.bits.fun.is_fun_fcsr)  -> fcsr_res,
      f2i.io.out.valid -> f2i.io.out.bits.toInt,
    ))
  fpu_exe_iwb_fifo.io.enq.bits.rd0 :=
    Mux1H(Seq(
      (io.fpu_iss_exe.fire & io.fpu_iss_exe.bits.fun.is_fun_fcsr) -> io.fpu_iss_exe.bits.param.rd0,
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

  io.fpu_iss_exe.ready := Mux( io.fpu_iss_exe.bits.fun.is_fun_fcsr | io.fpu_iss_exe.bits.fun.is_fun_divSqrt, infly_empty, ~infly_full & ~pending_csr & ~divSqrt.io.pending )

  assert( ~(fcsr_op_fifo.io.enq.valid     & ~fcsr_op_fifo.io.enq.ready)    , "Assert Failed, the pipeline assert all fifo enq will success" )
  assert( ~(fpu_exe_iwb_fifo.io.enq.valid & ~fpu_exe_iwb_fifo.io.enq.ready), "Assert Failed, the pipeline assert all fifo enq will success" )
  assert( ~(fpu_exe_fwb_fifo.io.enq.valid & ~fpu_exe_fwb_fifo.io.enq.ready), "Assert Failed, the pipeline assert all fifo enq will success" )

}

