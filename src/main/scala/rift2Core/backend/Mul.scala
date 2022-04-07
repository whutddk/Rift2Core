
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





class Mul extends Module {
  val io = IO(new Bundle {
    val mul_iss_exe = Flipped(new DecoupledIO(new Mul_iss_info))
    val mul_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64,dp=64))
  })

  val mul_exe_iwb_fifo = Module( new Queue( new WriteBack_info(dw=64,dp=64), 1, true, false ) )
  io.mul_exe_iwb <> mul_exe_iwb_fifo.io.deq

  val info = Wire( new Mul_iss_info )

  val op1 = info.param.dat.op1
  val op2 = info.param.dat.op2


  val mul_op1_sign =
    Mux(
      ( info.fun.mul | info.fun.mulh | info.fun.mulhsu | info.fun.mulw),
      Mux(info.fun.mulw, op1(31), op1(63)),
      0.U
    )

  val mul_op2_sign =
    Mux(
      (info.fun.mul | info.fun.mulh | info.fun.mulw),
      Mux(info.fun.mulw, op2(31), op2(63)),
      0.U
    )

  val mul_op1 =
    Mux(
      info.fun.mulw,
      Cat(Fill(33, mul_op1_sign), op1(31,0)),
      Cat(mul_op1_sign, op1)
    )

  val mul_op2 =
    Mux(
      info.fun.mulw,
      Cat(Fill(33, mul_op2_sign), op2(31,0)),
      Cat(mul_op2_sign, op2)
    )

  val mul_res_128w = mul_op1.asSInt * mul_op2.asSInt






  val pending_div = RegInit(false.B)
  val pending_info = RegInit(0.U.asTypeOf(new Mul_iss_info) )

  info := Mux( io.mul_iss_exe.fire, io.mul_iss_exe.bits, pending_info )



  val is_32w = info.fun.divw | info.fun.divuw | info.fun.remw | info.fun.remuw;
  val is_usi = info.fun.divu | info.fun.remu | info.fun.divuw | info.fun.remuw;
  val is_div = info.fun.div | info.fun.divu | info.fun.rem | info.fun.remu | info.fun.divw | info.fun.divuw | info.fun.remw | info.fun.remuw

  val dividend_load =
    Cat ( 0.U(64.W),
      Mux(
        is_usi, 
        op1,
        Mux(  
          is_32w,
          Cat( 0.U(32.W), Mux(op1(31), (~op1(31,0) + 1.U), op1(31,0))),
          Mux( op1(63), (~op1 + 1.U), op1)
        )
      )
    )

  val divisor_load =
    Mux(
      is_usi,
      op2,
      Mux( 
        is_32w,
        Cat( Fill(32, 0.U), Mux(op2(31), (~op2(31,0) + 1.U), op2(31,0))),
        Mux(op2(63), (~op2 + 1.U), op2)
      )
    )


  val ( cnt, isEnd ) = Counter( 0 until 66 by 1, pending_div, io.mul_iss_exe.fire)

  when( io.mul_iss_exe.fire & ~mul_exe_iwb_fifo.io.enq.fire) {
    assert( is_div )
    pending_div := true.B
    pending_info := io.mul_iss_exe.bits
  } .elsewhen( mul_exe_iwb_fifo.io.enq.fire ) {
    pending_div := false.B
  }


  val dividend = RegInit(0.U(128.W))
  val divisor = RegInit(0.U(64.W))

  val dividend_shift = dividend << 1;
  val div_cmp = (dividend_shift(127,64) >= divisor);
  val divided = 
    Mux(
      div_cmp,
      Cat((dividend_shift(127,64) - divisor), dividend_shift(63,1), 1.U(1.W)),
      dividend_shift
    )



  val dividend_sign = Mux(is_usi, false.B, Mux(is_32w, op1(31).asBool, op1(63).asBool))
  val divisor_sign  = Mux(is_usi, false.B, Mux(is_32w, op2(31).asBool, op2(63).asBool))
  val div_by_zero = (op2 === 0.U)
  val div_overflow = ~is_usi & 
              (
                ( is_32w & (op1(31).asBool & (op1(30,0) === 0.U) ) & (op2(31,0).andR.asBool))
                |
                (~is_32w & (op1(63).asBool & (op1(62,0) === 0.U) ) & (op2(63,0).andR.asBool))								
              ); 

  when( cnt === 0.U ) {
    dividend := dividend_load 
    divisor := divisor_load
  }
  .otherwise {
    dividend := divided
  }

  val quot_sign_corrcet = 
    Mux(dividend_sign^divisor_sign, ~dividend(63,0) + 1.U, dividend(63,0))

  val rema_sign_corrcet = 
    Mux(dividend_sign, ~dividend(127,64) + 1.U, dividend(127,64))

  val quot_res = MuxCase(0.U, Array(
    div_by_zero -> Fill(64, 1.U),
    div_overflow -> Mux( is_32w, Cat( Fill(33, 1.U(1.W)), 0.U(31.W)), Cat(1.U, 0.U(63.W))),
    (~div_by_zero & ~div_overflow) -> quot_sign_corrcet
    ))



  val rema_res = MuxCase(0.U, Array(
    div_by_zero -> Mux(is_32w, sextXTo(op1(31,0), 64), op1),
    div_overflow -> 0.U,
    (~div_by_zero & ~div_overflow) -> rema_sign_corrcet
    ))













  val res = MuxCase(DontCare, Array(
    info.fun.mul    -> mul_res_128w(63,0),
    info.fun.mulh   -> mul_res_128w(127,64),
    info.fun.mulhsu -> mul_res_128w(127,64),
    info.fun.mulhu  -> mul_res_128w(127,64),
    info.fun.div    -> quot_res,
    info.fun.divu   -> quot_res,
    info.fun.rem    -> rema_res,
    info.fun.remu   -> rema_res,
    info.fun.mulw   -> sextXTo(mul_res_128w(31,0), 64 ),
    info.fun.divw   -> sextXTo(quot_res(31,0), 64),
    info.fun.divuw  -> sextXTo(quot_res(31,0), 64),
    info.fun.remw   -> sextXTo(rema_res(31,0), 64),
    info.fun.remuw  -> sextXTo(rema_res(31,0), 64)
  ))




  // val is_fun_end = 
  //   Mux( io.mul_iss_exe.valid,
  //     MuxCase( false.B, Array(
  //       (~is_div === true.B) -> true.B,
  //       ((div_by_zero | div_overflow) === true.B) -> true.B,
  //       ( cnt === 65.U ) -> true.B
  //     )),
  //     false.B
  //   )
  
  

  io.mul_iss_exe.ready := ~pending_div


  mul_exe_iwb_fifo.io.enq.valid := 
    (io.mul_iss_exe.fire & ~is_div) |
    (io.mul_iss_exe.fire & is_div & ( div_by_zero | div_overflow )) |
    (pending_div & ( cnt === 65.U ))


  mul_exe_iwb_fifo.io.enq.bits.res := res
  mul_exe_iwb_fifo.io.enq.bits.rd0 := 
    Mux( io.mul_iss_exe.fire, info.param.rd0, pending_info.param.rd0)


}

