
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


/** the RV64M will be executed in this module */
abstract class MulDivBase(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle {
    val mul_iss_exe = Flipped(new DecoupledIO(new Mul_iss_info))
    val mul_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64))
    val flush = Input(Bool())
  })

  val mul_exe_iwb_fifo = Module( new Queue( new WriteBack_info(dw=64), 1, true, false ) )
  mul_exe_iwb_fifo.reset := reset.asBool | io.flush
  io.mul_exe_iwb <> mul_exe_iwb_fifo.io.deq

}


/** construct multiplier in this trait */
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

  

  val multiplier = Module(new Multiplier(new Mul_iss_info, 65) )
  multiplier.io.enq.valid := io.mul_iss_exe.valid & io.mul_iss_exe.bits.fun.isMul
  multiplier.io.op1 := mul_op1
  multiplier.io.op2 := mul_op2
  multiplier.io.enq.bits := io.mul_iss_exe.bits
  multiplier.io.flush := io.flush


  val mulRes = Mux1H(Seq(
    multiplier.io.deq.bits.fun.mul    -> multiplier.io.res(63,0),
    multiplier.io.deq.bits.fun.mulh   -> multiplier.io.res(127,64),
    multiplier.io.deq.bits.fun.mulhsu -> multiplier.io.res(127,64),
    multiplier.io.deq.bits.fun.mulhu  -> multiplier.io.res(127,64),
    multiplier.io.deq.bits.fun.mulw   -> sextXTo(multiplier.io.res(31,0), 64 ),
  ))



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
    Mux( io.mul_iss_exe.bits.fun.isMul, multiplier.io.enq.ready, dividor.io.enq.ready )

  

  val iwbArb = Module(new Arbiter(new WriteBack_info(dw=64), 2))


  multiplier.io.deq.ready := iwbArb.io.in(0).ready
  iwbArb.io.in(0).valid := multiplier.io.deq.valid
  iwbArb.io.in(0).bits.rd0 := multiplier.io.deq.bits.param.rd0
  iwbArb.io.in(0).bits.res := mulRes

  iwbArb.io.in(1) <> dividor.io.deq
  
  

  mul_exe_iwb_fifo.io.enq <> iwbArb.io.out


}

class Multiplier[T<:Data]( pipeType: T, dw: Int ) extends Module {
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(pipeType))
    val deq = Decoupled(pipeType)
    val op1 = Input(UInt(dw.W))
    val op2 = Input(UInt(dw.W))
    val res = Output(UInt((2*dw).W))
    val flush = Input(Bool()) 
  })

  val pipeMidStageInfo = Module( new Queue( pipeType, 1, true, false ) )
  val pipeFnlStageInfo = Module( new Queue( pipeType, 1, true, false ) )

  pipeMidStageInfo.reset := reset.asBool | io.flush
  pipeFnlStageInfo.reset := reset.asBool | io.flush

  val (oriTree, oriLat) = booth4Enc( a = io.op1, b = io.op2 )
  val (sum, lat) = WallaceTreeCompress( oriTree, oriLat )
  io.res  := sum(0) + sum(1)

  io.enq <> pipeMidStageInfo.io.enq
  pipeMidStageInfo.io.deq <> pipeFnlStageInfo.io.enq
  io.deq <> pipeFnlStageInfo.io.deq









  /** an algorithm that reduce the sign-bits at MSB
    * @param a the input of partial product, the length of a should be clearly stated or the sign may insert at wrong bits
    * @return the processed partial product with sign optimation
    */
  def preProcess( a: UInt ): UInt = {
    val len = a.getWidth
    return Cat( 1.U(1.W), ~a(len-1), a(len-2, 0) )
  }

 
  /** a single bits full-adder
    * @param in Three input of the adder
    * @param lat the latency of the input
    * @return cout is the carry result
    * @return sum is the sum
    * @return latency the the latency module of the adder, in this version, the output latency is the maximum of the input + 2
    */
  def fullAdder( in: Vec[Bool], lat: Seq[Int] ): (Bool, Bool, Int) = {
    require( in.length == 3 )
    val sum  = WireDefault(in(0) ^ in(1) ^ in(2))
    val cout = WireDefault((in(0) & in(1)) | ((in(0) ^ in(1)) & in(2)))
    val latency = lat.max + 2
    return (cout, sum, latency)
  }

  /** a single bits half-adder
    * @param in Two input of the adder
    * @param lat the latency of the input
    * @return cout is the carry result
    * @return sum is the sum
    * @return latency the the latency module of the adder, in this version, the output latency is the maximum of the input + 1
    */
  def halfAdder( in: Vec[Bool], lat: Seq[Int] ): (Bool, Bool, Int) = {
    require( in.length == 2 )
    val sum  = WireDefault(in(0) ^ in(1))
    val cout = WireDefault(in(0) & in(1))
    val latency = lat.max + 1
    return (cout, sum, latency)
  }


  /** compress one Column */
  def ColumnCompress(col: Seq[Bool], lat: Seq[Int]): ( Seq[Bool], Seq[Bool], Seq[Int] ) = {
    require( col.length == lat.length )

    val len = col.length
            val column  = col ++ Seq.fill(3-(len%3))(false.B)
            val latency = lat ++ Seq.fill(3-(len%3))(0)

    // println( "ColumnCompress: col length = "+ col.length + " , column length = "+ column.length + " \n" )

            // val adder = for( i <- 0 until ((len+2) / 3) ) yield {
            //   csa_3_2(VecInit(column(3*i), column(3*i+1), column(3*i+2)), Seq( latency(3*i), latency(3*i+1), latency(3*i+2) ) )
            // }



    if ( len == 1) {
      return ( Seq(false.B), Seq(col(0)), Seq(lat(0)) )
    } else if ( len == 2 ) { 
      val adder1 = halfAdder( VecInit(col), lat = lat )
      return ( Seq(adder1._1),Seq(adder1._2), Seq(adder1._3) )
    } else if ( len == 3 ) {
      val adder2 = fullAdder( VecInit(col), lat = lat )
      return ( Seq(adder2._1),Seq(adder2._2), Seq(adder2._3) )
    } else if ( len == 4 ) {
      val adder3 = halfAdder( VecInit(col(0), col(1)), Seq(lat(0), lat(1)) )
      val adder4 = halfAdder( VecInit(col(2), col(3)), Seq(lat(2), lat(3)) )
      return ( Seq(adder3._1, adder4._1), Seq(adder3._2, adder4._2), Seq(adder3._3, adder4._3) )
    } else {
      val (cout, sum, latency) = fullAdder( VecInit(col take 3), lat take 3 )
      val (scout, ssum, slatency ) = ColumnCompress(col drop 3, lat drop 3)

      return ( Seq(cout) ++ scout, Seq(sum) ++ ssum, Seq(latency) ++ slatency )
    }

  }

  /** compress the WallaceTree */
  def WallaceTreeCompress( tree: MixedVec[Vec[Bool]], lat: Seq[Seq[Int]] ): (Vec[UInt], Int) = {
    require( tree.length == lat.length )
    require( (tree zip lat).map{ case(x, y) => (x.length == y.length) }.foldLeft( true )( _ & _ ) )

    require(tree.length == 2*dw)

    val treeHight   = tree.map{ _.length}.max
    val treeLatency = lat.map{ _.max }.max


    // println( "WallaceTree is "+ treeHight + " meters tall. WallaceTree is "+ treeLatency + " seconds delay\n" )
    // for ( i <- 0 until 128 ) { println("tree("+i+") is "+tree(i).length+"meters tall") }

    if ( treeHight == 2 ) {
      val sum = Wire( Vec(2*dw, Bool() ))
      val cin = Wire( Vec(2*dw, Bool() ))

      for ( i <- 0 until 2*dw ) {
        if ( tree(i).length == 0 ) {
          sum(i) := false.B
          cin(i) := false.B
        } else if ( tree(i).length == 1 ) {
          sum(i) := tree(i)(0)
          cin(i) := false.B
        } else if ( tree(i).length == 2 ) {
          sum(i) := tree(i)(0)
          cin(i) := tree(i)(1)
        } else {
          require(false)
          sum(i) := false.B
          cin(i) := false.B
        }
      }
      // println( "WallaceTree Compression Finish, latency is " + treeLatency +"\n")
      return (RegEnable( VecInit(cin.asUInt, sum.asUInt), pipeFnlStageInfo.io.enq.fire ), treeLatency )
    } else if ( treeHight > 2 ) {
      val compress = for( i <- 0 until 2*dw ) yield { ColumnCompress(col = tree(i), lat(i)) }

      val newCout: Seq[Seq[Bool]] = compress.map{ _._1 }
      val newSum : Seq[Seq[Bool]] = compress.map{ _._2 }
      val newLat : Seq[Seq[Int ]] = compress.map{ _._3 }

      val newTree = MixedVecInit(for( i <- 0 until 2*dw ) yield {
        if( i == 0 ) { VecInit(newSum(0)) }
        else {  VecInit(newSum(i) ++ newCout(i-1)) }
      })

      val newLatency = for( i <- 0 until 2*dw ) yield {
        if( i == 0 ) { newLat(0) }
        else { newLat(i) ++ newLat(i-1) }
      }

      val emptyLatency = for( i <- 0 until 2*dw ) yield {
        if( i == 0 ) { newLat(0).map{ _ => 0} }
        else { (newLat(i) ++ newLat(i-1)).map{ _ => 0} }
      }

      if( treeLatency == 120 ) {
        return WallaceTreeCompress( RegEnable( newTree, pipeMidStageInfo.io.enq.fire ), emptyLatency )
      } else {
        return WallaceTreeCompress( tree = newTree, lat = newLatency )        
      }

    } else {
      require(false)
      return ( VecInit(0.U, 0.U), 0 )
    }


  }


  def booth4Enc( a: UInt, b: UInt ): (MixedVec[Vec[Bool]], Seq[Seq[Int]]) = {
    require( a.getWidth == b.getWidth )
    val len = a.getWidth
    require( a.getWidth == dw )
    val oriB, ori2B, negB, neg2B = Wire(UInt((len+1).W))
    oriB  := sextXTo(b, len+1)
    ori2B := sextXTo(b << 1, len+1)
    negB  := sextXTo(-b, len+1)
    neg2B := sextXTo(-(b << 1), len+1)


    // val rows34 = Wire( Vec(34, UInt(130.W)) )
    // dontTouch(rows34)

    val buildingTrees = for( i <- 0 until (len+1)/2 ) yield {
      val booth4 = if ( i == 0 ) { Cat( a(1), a(0), 0.U(1.W) ) } else if(2*i+1 == len) { Cat( a(2*i), a(2*i), a(2*i-1)) } else { a(2*i+1, 2*i-1) } 
      
      val payload = Mux1H(Seq(
        ( booth4 === "b000".U ) -> preProcess((0.U)((len+1).W)),
        ( booth4 === "b001".U ) -> preProcess( oriB ),
        ( booth4 === "b010".U ) -> preProcess( oriB ),
        ( booth4 === "b011".U ) -> preProcess( ori2B ),
        ( booth4 === "b100".U ) -> preProcess( neg2B ),
        ( booth4 === "b101".U ) -> preProcess( negB ),
        ( booth4 === "b110".U ) -> preProcess( negB ),
        ( booth4 === "b111".U ) -> preProcess((0.U)((len+1).W)),
      ))

      // rows34(i) := payload << 2*i
      map2WallaceTree( payload, offset = 2*i )
    }

    val trees = buildingTrees.map{_._1}
    val lats  = buildingTrees.map{_._2}


    // println( "trees is "+ trees )
    val oriTree = for( i <- 0 until 2*dw ) yield { VecInit(
      if ( i == (len) ) {
        trees.map{ _(i) }.foldLeft(Seq(): Seq[Bool])( _ ++ _ ) ++ Seq(true.B)
      } else {
        trees.map{ _(i) }.foldLeft(Seq(): Seq[Bool])( _ ++ _ )
      }
    )}

    val oriLat = for( i <- 0 until 2*dw ) yield { 
      if ( i == (len) ) {
        lats.map{ _(i) }.foldLeft(Seq(): Seq[Int])( _ ++ _ ) ++ Seq(0)
      } else {
        lats.map{ _(i) }.foldLeft(Seq(): Seq[Int])( _ ++ _ )        
      }
    }


    return (MixedVecInit(oriTree), oriLat)
  }

  def map2WallaceTree( payload: UInt, offset: Int ): (Seq[Seq[Bool]], Seq[Seq[Int]]) = {

    val tree = for ( i <- 0 until 2*dw ) yield {
      if ( (i >= offset) && ( i < offset + payload.getWidth ) ) {
        Seq( Mux( payload(i-offset) === 1.U, true.B, false.B ) )
      } else {
        Seq(): Seq[Bool]
      }
    }

    val lat = for ( i <- 0 until 2*dw ) yield {
      if ( (i >= offset) && ( i < offset + payload.getWidth ) ) {
        Seq( 0 )
      } else {
        Seq(): Seq[Int]
      }
    }

    // println( "tree is" + tree+"\n" )
    return (tree, lat)
  }
}


class Dividor(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Mul_iss_info))
    val deq = Decoupled(new WriteBack_info(dw=64))
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


