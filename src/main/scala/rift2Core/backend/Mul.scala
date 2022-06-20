
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
import chipsalliance.rocketchip.config.Parameters



class MulBase(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle {
    val mul_iss_exe = Flipped(new DecoupledIO(new Mul_iss_info))
    val mul_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64,dp=64))
  })

  val mul_exe_iwb_fifo = Module( new Queue( new WriteBack_info(dw=64,dp=64), 1, true, false ) )
  io.mul_exe_iwb <> mul_exe_iwb_fifo.io.deq

  val info = Wire( new Mul_iss_info )

  val op1 = info.param.dat.op1
  val op2 = info.param.dat.op2

}






class Mul(implicit p: Parameters) extends MulBase  {

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

  // val mul_res_128w = mul_op1.asSInt * mul_op2.asSInt

  val mul_res_128w = addRows(booth4Enc( a = mul_op1, b = mul_op2 ))






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
        Mux(is_32w, op1(31,0), op1),
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
      Mux(is_32w, op2(31,0), op2),
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


  
  

  io.mul_iss_exe.ready := ~pending_div


  mul_exe_iwb_fifo.io.enq.valid := 
    (io.mul_iss_exe.fire & ~is_div) |
    (io.mul_iss_exe.fire & is_div & ( div_by_zero | div_overflow )) |
    (pending_div & ( cnt === 65.U ))


  mul_exe_iwb_fifo.io.enq.bits.res := res
  mul_exe_iwb_fifo.io.enq.bits.rd0 := 
    Mux( io.mul_iss_exe.fire, info.param.rd0, pending_info.param.rd0)



  def booth4Enc( a: UInt, b: UInt ): Vec[UInt] = {
    require( a.getWidth == b.getWidth )
    val len = a.getWidth
    require( a.getWidth == 65 )


    val rows = Wire(Vec((len+1)/2, UInt((2*len).W)))

    for( i <- 0 until (len+1)/2 ) {
      val booth4 = if ( i == 0 ) { Cat( a(1), a(0), 0.U(1.W) ) } else if(2*i+1 == len) { Cat( a(2*i), a(2*i), a(2*i-1)) } else { a(2*i+1, 2*i-1) } 
      
      rows(i) := Mux1H(Seq(
        ( booth4 === "b000".U ) -> 0.U,
        ( booth4 === "b001".U ) -> sextXTo( b, 2*len),
        ( booth4 === "b010".U ) -> sextXTo( b, 2*len),
        ( booth4 === "b011".U ) -> sextXTo(  b << 1, 2*len),
        ( booth4 === "b100".U ) -> sextXTo(-(b << 1), 2*len),
        ( booth4 === "b101".U ) -> sextXTo(-b, 2*len),
        ( booth4 === "b110".U ) -> sextXTo(-b, 2*len),
        ( booth4 === "b111".U ) -> 0.U,
      )) << (2*i)
    }
    return rows
  }

  def csa_3_2( a: UInt, b: UInt, c: UInt ): ( UInt, UInt ) = {
    val len = a.getWidth max b.getWidth max c.getWidth

    val in   = Wire(Vec( 3, UInt(len.W) ))
    val cout = Wire(Vec(len, UInt(1.W)))
    val sum  = Wire(Vec(len, UInt(1.W)))
    in(0) := a; in(1) := b; in(2) := c

    for ( i <- 0 until len ) {
      sum(i)  := in(0)(i) ^ in(1)(i) ^ in(2)(i)
      cout(i) := (in(0)(i) & in(1)(i)) | ((in(0)(i) ^ in(1)(i)) & in(2)(i))
    }
    return (Cat(cout.reverse) << 1, Cat(sum.reverse))
  }

  def addRows(rows33: Vec[UInt]): UInt = {
    require(rows33.length == 33)

    val rows22 = Wire( Vec(22, UInt(130.W)) )
    val rows15 = Wire( Vec(15, UInt(130.W)) )
    val rows10 = Wire( Vec(10, UInt(130.W)) )
    val rows7  = Wire( Vec( 7, UInt(130.W)) )
    val rows5  = Wire( Vec( 5, UInt(130.W)) )
    val rows4  = Wire( Vec( 4, UInt(130.W)) )
    val rows3  = Wire( Vec( 3, UInt(130.W)) )
    val rows2  = Wire( Vec( 2, UInt(130.W)) )

    dontTouch(rows22)
    dontTouch(rows15)
    dontTouch(rows10)
    dontTouch(rows7)
    dontTouch(rows5)
    dontTouch(rows4)
    dontTouch(rows3)
    dontTouch(rows2)

    for ( i <- 0 until 11 ) {
      val (cout, sum) = csa_3_2( rows33(3*i), rows33(3*i+1), rows33(3*i+2) )
      rows22(2*i)   := cout
      rows22(2*i+1) := sum
    }

    for( i <- 0 until 7 ) {
      val (cout, sum) = csa_3_2( rows22(1+(3*i)), rows22(1+(3*i)+1), rows22(1+(3*i)+2) )
      rows15(2*i)   := cout
      rows15(2*i+1) := sum      
    }
    rows15(14) := rows22(0)

    for( i <- 0 until 5 ) {
      val (cout, sum) = csa_3_2( rows15(3*i), rows15(3*i+1), rows15(3*i+2) )
      rows10(2*i)   := cout
      rows10(2*i+1) := sum      
    }

    for( i <- 0 until 3 ) {
      val (cout, sum) = csa_3_2( rows10(1+(3*i)), rows10(1+(3*i)+1), rows10(1+(3*i)+2) )
      rows7(2*i)   := cout
      rows7(2*i+1) := sum      
    }
    rows7(6) := rows10(0)    

    for( i <- 0 until 2 ) {
      val (cout, sum) = csa_3_2( rows7(1+(3*i)), rows7(1+(3*i)+1), rows7(1+(3*i)+2) )
      rows5(2*i)   := cout
      rows5(2*i+1) := sum      
    }
    rows5(4) := rows7(0)


    for( i <- 0 until 1 ) {
      val (cout, sum) = csa_3_2( rows5(2+(3*i)), rows5(2+(3*i)+1), rows5(2+(3*i)+2) )
      rows4(2*i)   := cout
      rows4(2*i+1) := sum      
    }
    rows4(2) := rows5(0)
    rows4(3) := rows5(1)

    for( i <- 0 until 1 ) {
      val (cout, sum) = csa_3_2( rows4(1+(3*i)), rows4(1+(3*i)+1), rows4(1+(3*i)+2) )
      rows3(2*i)   := cout
      rows3(2*i+1) := sum      
    }
    rows3(2) := rows4(0)

    for( i <- 0 until 1 ) {
      val (cout, sum) = csa_3_2( rows3(3*i), rows3(3*i+1), rows3(3*i+2) )
      rows2(2*i)   := cout
      rows2(2*i+1) := sum      
    }

    return rows2(0) + rows2(1)
  }

}