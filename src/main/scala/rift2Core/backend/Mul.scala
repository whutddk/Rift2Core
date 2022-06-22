
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



abstract class MulDivBase(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle {
    val mul_iss_exe = Flipped(new DecoupledIO(new Mul_iss_info))
    val mul_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64,dp=64))
    val flush = Input(Bool())
  })

  val mul_exe_iwb_fifo = Module( new Queue( new WriteBack_info(dw=64,dp=64), 1, true, false ) )
  mul_exe_iwb_fifo.reset := reset.asBool | io.flush
  io.mul_exe_iwb <> mul_exe_iwb_fifo.io.deq

  



}

trait Mul { this: MulDivBase =>


  val mul_op1_sign =
    Mux(
      ( io.mul_iss_exe.bits.fun.mul | io.mul_iss_exe.bits.fun.mulh | io.mul_iss_exe.bits.fun.mulhsu | io.mul_iss_exe.bits.fun.mulw),
      Mux(io.mul_iss_exe.bits.fun.mulw, io.mul_iss_exe.bits.param.dat.op1(31), io.mul_iss_exe.bits.param.dat.op1(63)),
      0.U
    )

  val mul_op2_sign =
    Mux(
      (io.mul_iss_exe.bits.fun.mul | io.mul_iss_exe.bits.fun.mulh | io.mul_iss_exe.bits.fun.mulw),
      Mux(io.mul_iss_exe.bits.fun.mulw, io.mul_iss_exe.bits.param.dat.op2(31), io.mul_iss_exe.bits.param.dat.op2(63)),
      0.U
    )

  val mul_op1 =
    Mux(
      io.mul_iss_exe.bits.fun.mulw,
      Cat(Fill(33, mul_op1_sign), io.mul_iss_exe.bits.param.dat.op1(31,0)),
      Cat(mul_op1_sign, io.mul_iss_exe.bits.param.dat.op1)
    )

  val mul_op2 =
    Mux(
      io.mul_iss_exe.bits.fun.mulw,
      Cat(Fill(33, mul_op2_sign), io.mul_iss_exe.bits.param.dat.op2(31,0)),
      Cat(mul_op2_sign, io.mul_iss_exe.bits.param.dat.op2)
    )


  val rows33 = Wire( Vec(33, UInt(130.W)) )
  val rows22 = Wire( Vec(22, UInt(130.W)) )
  val rows15 = Wire( Vec(15, UInt(130.W)) )
  val rows10 = Wire( Vec(10, UInt(130.W)) )
  val rows07 = Wire( Vec( 7, UInt(130.W)) )
  val rows05 = Wire( Vec( 5, UInt(130.W)) )
  val rows04 = Wire( Vec( 4, UInt(130.W)) )
  val rows03 = Wire( Vec( 3, UInt(130.W)) )
  val rows02 = Wire( Vec( 2, UInt(130.W)) )


  val pipeStage05Rows = Module( new Queue( Vec( 5, UInt(130.W)), 1, true, false ) )
  val pipeStage05Info  = RegEnable( io.mul_iss_exe.bits, io.mul_iss_exe.fire & io.mul_iss_exe.bits.fun.isMul)
  val pipeStage02Rows = Module( new Queue( Vec( 2, UInt(130.W)), 1, true, false ) )
  val pipeStage02Info  = RegEnable( pipeStage05Info, pipeStage05Rows.io.deq.fire )

  pipeStage05Rows.reset := reset.asBool | io.flush
  pipeStage02Rows.reset := reset.asBool | io.flush


  rows33 := booth4Enc( a = mul_op1, b = mul_op2 )
  rows22 := addRows(rowsIn = rows33)
  rows15 := addRows(rowsIn = rows22)
  rows10 := addRows(rowsIn = rows15)
  rows07 := addRows(rowsIn = rows10)
  rows05 := addRows(rowsIn = rows07)
  pipeStage05Rows.io.enq.bits := rows05
  pipeStage05Rows.io.enq.valid := io.mul_iss_exe.valid & io.mul_iss_exe.bits.fun.isMul

  rows04 := addRows(rowsIn = pipeStage05Rows.io.deq.bits)
  rows03 := addRows(rowsIn = rows04)
  rows02 := addRows(rowsIn = rows03)

  pipeStage02Rows.io.enq.bits := rows02
  pipeStage02Rows.io.enq.valid := pipeStage05Rows.io.deq.valid
  pipeStage05Rows.io.deq.ready := pipeStage02Rows.io.enq.ready

  // val mul_res_128w = mul_op1.asSInt * mul_op2.asSInt
  val mul_res_128w = pipeStage02Rows.io.deq.bits(0) + pipeStage02Rows.io.deq.bits(1)
  val mulRes = Mux1H(Seq(
    pipeStage02Info.fun.mul    -> mul_res_128w(63,0),
    pipeStage02Info.fun.mulh   -> mul_res_128w(127,64),
    pipeStage02Info.fun.mulhsu -> mul_res_128w(127,64),
    pipeStage02Info.fun.mulhu  -> mul_res_128w(127,64),
    pipeStage02Info.fun.mulw   -> sextXTo(mul_res_128w(31,0), 64 ),
  ))


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


  def addRows(rowsIn: Vec[UInt]): Vec[UInt] = {
    val iWidth = rowsIn(0).getWidth
    val iLen = rowsIn.length
    require( iLen >= 3, "inLen is "+iLen+", Which less than 3!\n" )

    val oLen = ((iLen / 3) * 2) + (iLen % 3)

    val rowsOut = Wire( Vec(oLen, UInt(iWidth.W) ) )
    
    for( i <- 0 until iLen / 3 ) {
      val (cout, sum) = csa_3_2( rowsIn((iLen%3)+(3*i)), rowsIn((iLen%3)+(3*i)+1), rowsIn((iLen%3)+(3*i)+2) )
      rowsOut(2*i)   := cout
      rowsOut(2*i+1) := sum      
    }

    for( i <- 0 until (iLen%3) ) {
      rowsOut( oLen-1-i ) := rowsIn(i)
    }
    return rowsOut

  }


//   def addRows(rows33: Vec[UInt]): UInt = {
//     require(rows33.length == 33)

//     val rows22 = Wire( Vec(22, UInt(130.W)) )
//     val rows15 = Wire( Vec(15, UInt(130.W)) )
//     val rows10 = Wire( Vec(10, UInt(130.W)) )
//     val rows7  = Wire( Vec( 7, UInt(130.W)) )
//     val rows5  = Wire( Vec( 5, UInt(130.W)) )
//     val rows4  = Wire( Vec( 4, UInt(130.W)) )
//     val rows3  = Wire( Vec( 3, UInt(130.W)) )
//     val rows2  = Wire( Vec( 2, UInt(130.W)) )

//     dontTouch(rows22)
//     dontTouch(rows15)
//     dontTouch(rows10)
//     dontTouch(rows7)
//     dontTouch(rows5)
//     dontTouch(rows4)
//     dontTouch(rows3)
//     dontTouch(rows2)

//     for ( i <- 0 until 11 ) {
//       val (cout, sum) = csa_3_2( rows33(3*i), rows33(3*i+1), rows33(3*i+2) )
//       rows22(2*i)   := cout
//       rows22(2*i+1) := sum
//     }

//     for( i <- 0 until 7 ) {
//       val (cout, sum) = csa_3_2( rows22(1+(3*i)), rows22(1+(3*i)+1), rows22(1+(3*i)+2) )
//       rows15(2*i)   := cout
//       rows15(2*i+1) := sum      
//     }
//     rows15(14) := rows22(0)

//     for( i <- 0 until 5 ) {
//       val (cout, sum) = csa_3_2( rows15(3*i), rows15(3*i+1), rows15(3*i+2) )
//       rows10(2*i)   := cout
//       rows10(2*i+1) := sum      
//     }

//     for( i <- 0 until 3 ) {
//       val (cout, sum) = csa_3_2( rows10(1+(3*i)), rows10(1+(3*i)+1), rows10(1+(3*i)+2) )
//       rows7(2*i)   := cout
//       rows7(2*i+1) := sum      
//     }
//     rows7(6) := rows10(0)    

//     for( i <- 0 until 2 ) {
//       val (cout, sum) = csa_3_2( rows7(1+(3*i)), rows7(1+(3*i)+1), rows7(1+(3*i)+2) )
//       rows5(2*i)   := cout
//       rows5(2*i+1) := sum      
//     }
//     rows5(4) := rows7(0)


//     for( i <- 0 until 1 ) {
//       val (cout, sum) = csa_3_2( rows5(2+(3*i)), rows5(2+(3*i)+1), rows5(2+(3*i)+2) )
//       rows4(2*i)   := cout
//       rows4(2*i+1) := sum      
//     }
//     rows4(2) := rows5(0)
//     rows4(3) := rows5(1)

//     for( i <- 0 until 1 ) {
//       val (cout, sum) = csa_3_2( rows4(1+(3*i)), rows4(1+(3*i)+1), rows4(1+(3*i)+2) )
//       rows3(2*i)   := cout
//       rows3(2*i+1) := sum      
//     }
//     rows3(2) := rows4(0)

//     for( i <- 0 until 1 ) {
//       val (cout, sum) = csa_3_2( rows3(3*i), rows3(3*i+1), rows3(3*i+2) )
//       rows2(2*i)   := cout
//       rows2(2*i+1) := sum      
//     }

//     return rows2(0) + rows2(1)
//   }
// }

}

trait Div { this: MulDivBase =>
  val dividor = Module(new Dividor)

  dividor.io.enq.bits  := io.mul_iss_exe.bits
  dividor.io.enq.valid := io.mul_iss_exe.bits.fun.isDiv & io.mul_iss_exe.valid

  dividor.io.flush := io.flush




}



class MulDiv(implicit p: Parameters) extends MulDivBase with Mul with Div {


  // io.mul_iss_exe.ready := ~isDivBusy
  io.mul_iss_exe.ready :=
    Mux( io.mul_iss_exe.bits.fun.isMul, pipeStage05Rows.io.enq.ready, dividor.io.enq.ready )


  val iwbArb = Module(new Arbiter(new WriteBack_info(dw=64,dp=64), 2))


  pipeStage02Rows.io.deq.ready := iwbArb.io.in(0).ready
  iwbArb.io.in(0).valid := pipeStage02Rows.io.deq.valid
  iwbArb.io.in(0).bits.rd0 <> pipeStage02Info.param.rd0
  iwbArb.io.in(0).bits.res <> mulRes

  iwbArb.io.in(1) <> dividor.io.deq
  
  



  

  mul_exe_iwb_fifo.io.enq <> iwbArb.io.out


}


class csa_3_2(len: Int) extends Module {
  val io = IO(new Bundle{
    val in = Input(Vec( 3, UInt(len.W) ))
    val cout = Output(UInt(len.W))
    val sum  = Output(UInt(len.W))
  })

  val coutBits = Wire(Vec(len, UInt(1.W)))
  val sumBits  = Wire(Vec(len, UInt(1.W)))

  for ( i <- 0 until len ) {
    sumBits(i)  := io.in(0)(i) ^ io.in(1)(i) ^ io.in(2)(i)
    coutBits(i) := (io.in(0)(i) & io.in(1)(i)) | ((io.in(0)(i) ^ io.in(1)(i)) & io.in(2)(i))
  }

  io.cout := Cat(coutBits.reverse) << 1
  io.sum  := Cat(sumBits.reverse)

}

object csa_3_2 {
  def apply( a: UInt, b: UInt, c: UInt ): ( UInt, UInt ) = {
    val len = a.getWidth max b.getWidth max c.getWidth
    val mdl = Module(new csa_3_2(len))

    mdl.io.in(0) := a; mdl.io.in(1) := b; mdl.io.in(2) := c;

    return (mdl.io.cout, mdl.io.sum)
  }
}

class Dividor(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Mul_iss_info))
    val deq = Decoupled(new WriteBack_info(dw=64,dp=64))
    val flush = Input(Bool())
  })

  when( io.enq.fire ) { assert( io.enq.bits.fun.isDiv ) }

  val divBypass = Wire(Bool())
  val isDivBusy = RegInit(false.B)
  val pendingInfo = RegEnable( io.enq.bits, io.enq.fire & ~divBypass )

  io.enq.ready := ~isDivBusy & io.deq.ready

  when( io.deq.fire | io.flush ) {
    isDivBusy := false.B
  } .elsewhen( io.enq.fire & ~divBypass) {
    isDivBusy := true.B
  } 

  val info = Mux( io.enq.fire, io.enq.bits, pendingInfo )

  val divOp1 = info.param.dat.op1
  val divOp2 = info.param.dat.op2
  val is_32w = info.fun.divw | info.fun.divuw | info.fun.remw | info.fun.remuw;
  val is_usi = info.fun.divu | info.fun.remu | info.fun.divuw | info.fun.remuw;
  val is_div = io.enq.bits.fun.isDiv

  val dividend_load =
    Cat ( 0.U(64.W),
      Mux(
        is_usi, 
        Mux(is_32w, divOp1(31,0), divOp1),
        Mux(  
          is_32w,
          Cat( 0.U(32.W), Mux(divOp1(31), (~divOp1(31,0) + 1.U), divOp1(31,0))),
          Mux( divOp1(63), (~divOp1 + 1.U), divOp1)
        )
      )
    )

  val divisor_load =
    Mux(
      is_usi,
      Mux(is_32w, divOp2(31,0), divOp2),
      Mux( 
        is_32w,
        Cat( Fill(32, 0.U), Mux(divOp2(31), (~divOp2(31,0) + 1.U), divOp2(31,0))),
        Mux( divOp2(63), (~divOp2 + 1.U), divOp2 )
      )
    )


  val ( cnt, isEnd ) = Counter( 0 until 66 by 1, isDivBusy, io.enq.fire | io.flush)




  val dividend = Reg(UInt(128.W))
  val divisor = Reg(UInt(64.W))

  val dividend_shift = dividend << 1;
  val div_cmp = (dividend_shift(127,64) >= divisor);
  val divided = 
    Mux(
      div_cmp,
      Cat((dividend_shift(127,64) - divisor), dividend_shift(63,1), 1.U(1.W)),
      dividend_shift
    )



  val dividend_sign = Mux(is_usi, false.B, Mux(is_32w, divOp1(31).asBool, divOp1(63).asBool))
  val divisor_sign  = Mux(is_usi, false.B, Mux(is_32w, divOp2(31).asBool, divOp2(63).asBool))
  val div_by_zero = (divOp2 === 0.U)
  val div_overflow = ~is_usi & 
              (
                ( is_32w & (divOp1(31).asBool & (divOp1(30,0) === 0.U) ) & (divOp2(31,0).andR.asBool))
                |
                (~is_32w & (divOp1(63).asBool & (divOp1(62,0) === 0.U) ) & (divOp2(63,0).andR.asBool))								
              )
  divBypass := div_by_zero | div_overflow
  val divFinish = isDivBusy & (cnt === 65.U)
  val quotDZRes = Fill(64, 1.U)
  val remaDZRes = Mux(is_32w, sextXTo(divOp1(31,0), 64), divOp1)
  val quotOFRes = Mux( is_32w, Cat( Fill(33, 1.U(1.W)), 0.U(31.W)), Cat(1.U, 0.U(63.W)))
  val remaOFRes = 0.U








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

  val quot_res = MuxCase(quot_sign_corrcet, Array(
    div_by_zero  -> quotDZRes,
    div_overflow -> quotOFRes,
  ))

  val rema_res = MuxCase(rema_sign_corrcet, Array(
    div_by_zero  -> remaDZRes,
    div_overflow -> remaOFRes,
  ))

  val divRes = Mux1H(Seq(
    info.fun.div    -> quot_res,
    info.fun.divu   -> quot_res,
    info.fun.rem    -> rema_res,
    info.fun.remu   -> rema_res,
    info.fun.divw   -> sextXTo(quot_res(31,0), 64),
    info.fun.divuw  -> sextXTo(quot_res(31,0), 64),
    info.fun.remw   -> sextXTo(rema_res(31,0), 64),
    info.fun.remuw  -> sextXTo(rema_res(31,0), 64)
  ))

  io.deq.valid := 
    (io.enq.valid & divBypass ) |
    (divFinish)

  io.deq.bits.res := divRes
  io.deq.bits.rd0 := info.param.rd0

}
