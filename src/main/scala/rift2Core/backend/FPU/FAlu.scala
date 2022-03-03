
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


class FAlu() extends Module with HasFPUParameters{
  val io = IO(new Bundle{
    val fpu_iss_exe = Flipped(DecoupledIO(new Fpu_iss_info))
    val fpu_exe_iwb = DecoupledIO(new WriteBack_info(dw=65, dp=64))
    val fpu_exe_fwb = DecoupledIO(new WriteBack_info(dw=65, dp=64))

    val fcsr = Input(UInt(24.W))
    val fcsr_cmm_op = DecoupledIO( new Exe_Port )

    val flush = Input(Bool())
  })

  val fpu_exe_iwb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info(dw=65, dp=64), 1, true, false ) )
    mdl.io.deq <> io.fpu_exe_iwb
    mdl
  }
  val fpu_exe_fwb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info(dw=65, dp=64), 1, true, false ) )
    mdl.io.deq <> io.fpu_exe_fwb
    mdl
  }


  val exc = Wire(UInt(5.W))

  val f2i = {
    val mdl = Module(new FPToInt)
    mdl.io.in.valid := io.fpu_iss_exe.valid
    mdl.io.in.bits := io.fpu_iss_exe.bits
    mdl.io.frm := io.fcsr(7,5)
    mdl
  }

  val i2f = {
    val mdl = Module(new IntToFP())
    mdl.io.in.valid := io.fpu_iss_exe.valid
    mdl.io.in.bits := io.fpu_iss_exe.bits
    mdl.io.frm := io.fcsr(7,5)
    mdl
  }

  val f2f = {
    val mdl = Module(new FPToFP())
    mdl.io.in.valid := io.fpu_iss_exe.valid
    mdl.io.in.bits := io.fpu_iss_exe.bits
    mdl.io.frm := io.fcsr(7,5)
    mdl
  }

  exc := 
    Mux1H(Seq(
      io.fpu_iss_exe.bits.fun.is_fun_class  -> f2i.io.out.bits.exc,
      io.fpu_iss_exe.bits.fun.is_fun_fcvtX  -> f2i.io.out.bits.exc,
      io.fpu_iss_exe.bits.fun.is_fun_xcvtF  -> i2f.io.out.bits.exc,
      io.fpu_iss_exe.bits.fun.is_fun_fcvtF  -> f2f.io.out.bits.exc,
      io.fpu_iss_exe.bits.fun.is_fun_fsgn   -> f2f.io.out.bits.exc,
      io.fpu_iss_exe.bits.fun.is_fun_maxMin -> f2f.io.out.bits.exc,
      io.fpu_iss_exe.bits.fun.is_fun_fcmp   -> f2i.io.out.bits.exc,
    ))

  val fcsr_op_fifo = {
    val mdl = Module(new Queue( new Exe_Port, 1, false, false ) )

    val rw = io.fpu_iss_exe.bits.fun.is_fun_frw
    val rs = Mux( io.fpu_iss_exe.bits.fun.is_fun_fcsr, io.fpu_iss_exe.bits.fun.is_fun_frs, true.B )
    val rc = io.fpu_iss_exe.bits.fun.is_fun_frc
    
    val dat = Mux( io.fpu_iss_exe.bits.fun.is_fun_fcsr, io.fpu_iss_exe.bits.param.dat.op1, exc )
    val addr = Mux( io.fpu_iss_exe.bits.fun.is_fun_fcsr, io.fpu_iss_exe.bits.param.dat.op2, 1.U )
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
      (f2i.io.out.valid | f2f.io.out.valid | i2f.io.out.valid)


  val fcsr_res =
    Mux1H(Seq(
      (io.fpu_iss_exe.bits.param.dat.op2 === 1.U) -> io.fcsr(4,0),
      (io.fpu_iss_exe.bits.param.dat.op2 === 2.U) -> io.fcsr(7,5),
      (io.fpu_iss_exe.bits.param.dat.op2 === 3.U) -> io.fcsr,
    ))



  fpu_exe_iwb_fifo.io.enq.valid := (io.fpu_iss_exe.fire & io.fpu_iss_exe.bits.fun.is_fun_fcsr) | f2i.io.out.valid

  fpu_exe_iwb_fifo.io.enq.bits.res :=
    Mux1H(Seq(
      io.fpu_iss_exe.fire & io.fpu_iss_exe.bits.fun.is_fun_fcsr  -> fcsr_res,
      f2i.io.out.valid -> f2i.io.out.bits.toInt,
    ))
  fpu_exe_iwb_fifo.io.enq.bits.viewAsSupertype(new Register_dstntn(64)) :=
    Mux1H(Seq(
      io.fpu_iss_exe.fire & io.fpu_iss_exe.bits.fun.is_fun_fcsr -> fcsr_res,
      f2i.io.out.valid -> f2i.io.out.bits.param.viewAsSupertype(new Register_dstntn(64)),
    ))

  fpu_exe_fwb_fifo.io.enq.valid := i2f.io.out.valid | f2f.io.out.valid
  fpu_exe_fwb_fifo.io.enq.bits.res := 
    Mux1H(Seq(
      i2f.io.out.valid  -> i2f.io.out.bits.toFloat,
      f2f.io.out.valid  -> f2f.io.out.bits.toFloat,
    ))
  fpu_exe_fwb_fifo.io.enq.bits.viewAsSupertype(new Register_dstntn(dp=64)) :=
    Mux1H(Seq(
      i2f.io.out.valid -> i2f.io.out.bits.param.viewAsSupertype(new Register_dstntn(dp=64)),
      f2f.io.out.valid -> f2f.io.out.bits.param.viewAsSupertype(new Register_dstntn(dp=64)),
    ))

  def is_all_empty =  f2i.io.is_empty & f2f.io.is_empty & i2f.io.is_empty

  io.fpu_iss_exe.ready := 
    Mux(
      io.fpu_iss_exe.bits.fun.is_fun_fcsr, is_all_empty & fcsr_op_fifo.io.enq.ready, fcsr_op_fifo.io.enq.ready
    )

  // assert( io.fpu_iss_exe.fire === fcsr_op_fifo.io.enq.fire )
  // assert( io.fpu_iss_exe.fire === (fpu_exe_iwb_fifo.io.enq.fire | fpu_exe_fwb_fifo.io.enq.fire) )
}

