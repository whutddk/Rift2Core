
/*
  Copyright (c) 2020 - 2024 Wuhan University of Technology <295054118@whut.edu.cn>

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

import org.chipsalliance.cde.config._


/** the RV64M will be executed in this module */
abstract class MulDivBase(implicit p: Parameters) extends RiftModule {
  
  class MulDivIO extends Bundle{
    val mul_iss_exe = Flipped(new DecoupledIO(new Mul_iss_info))
    val mul_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64))
    val flush = Input(Bool())    
  }

  val io: MulDivIO = IO(new MulDivIO)

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

  // val multiplier = Module(new NorMultiplier(new Mul_iss_info, 65) )

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

class MultiplierIO[T<:Data]( pipeType: T, dw: Int ) extends Bundle{
  val enq = Flipped(new DecoupledIO(pipeType))
  val deq = Decoupled(pipeType)
  val op1 = Input(UInt(dw.W))
  val op2 = Input(UInt(dw.W))
  val res = Output(UInt((2*dw).W))
  val flush = Input(Bool())     
}

class Multiplier[T<:Data]( pipeType: T, dw: Int ) extends Module {



  val io: MultiplierIO[T] = IO(new MultiplierIO( pipeType, dw ))

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
            // val column  = col ++ Seq.fill(3-(len%3))(false.B)
            // val latency = lat ++ Seq.fill(3-(len%3))(0)

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
        trees.map{ _(i) }.foldLeft(Seq(): Seq[Bool])( _ ++ _ ) ++ Seq(true.B) //the first sign bits
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


class NorMultiplier[T<:Data]( pipeType: T, dw: Int ) extends Module {
  val io: MultiplierIO[T] = IO(new MultiplierIO( pipeType, dw ))

  io.deq <> io.enq
  io.res := (io.op1.asSInt * io.op2.asSInt).asUInt
}




class Dividor(implicit p: Parameters) extends RiftModule {

  class DividorIO extends Bundle{
    val enq = Flipped(new DecoupledIO(new Mul_iss_info))
    val deq = Decoupled(new WriteBack_info(dw=64))
    val flush = Input(Bool())  
  }
  val io: DividorIO = IO(new DividorIO)

  when( io.enq.fire ) { assert( io.enq.bits.fun.isDiv ) }

  
  // val pendingInfo = RegEnable( io.enq.bits, io.enq.fire & ~divBypass )



  // val info = Mux( io.enq.fire, io.enq.bits, pendingInfo )

  val op1Pre   = io.enq.bits.param.dat.op1
  val op2Pre   = io.enq.bits.param.dat.op2
  val is32wPre = io.enq.bits.fun.isDiv32w;
  val isUsiPre = io.enq.bits.fun.isDivusi;


  val dividend_load =
    // Cat ( 0.U(64.W),
      Mux(
        isUsiPre, 
        Mux(is32wPre, op1Pre(31,0), op1Pre),
        Mux(  
          is32wPre,
          Cat( 0.U(32.W), Mux(op1Pre(31), (~op1Pre(31,0) + 1.U), op1Pre(31,0))),
          Mux( op1Pre(63), (~op1Pre + 1.U), op1Pre)
        )
      )
    // )

  val divisor_load =
    Mux(
      isUsiPre,
      Mux(is32wPre, op2Pre(31,0), op2Pre),
      Mux( 
        is32wPre,
        Cat( Fill(32, 0.U), Mux(op2Pre(31), (~op2Pre(31,0) + 1.U), op2Pre(31,0))),
        Mux( op2Pre(63), (~op2Pre + 1.U), op2Pre )
      )
    )












  val isDivByZero = (op2Pre === 0.U)
  val isDivOverflow = ~isUsiPre & 
              (
                ( is32wPre & (op1Pre(31).asBool & (op1Pre(30,0) === 0.U) ) & (op2Pre(31,0).andR.asBool))
                |
                (~is32wPre & (op1Pre(63).asBool & (op1Pre(62,0) === 0.U) ) & (op2Pre(63,0).andR.asBool))								
              )
  val divBypass = isDivByZero | isDivOverflow

  val quotDZRes = Fill(64, 1.U)
  val remaDZRes = Mux( is32wPre, sextXTo(op1Pre(31,0), 64), op1Pre)
  val quotOFRes = Mux( is32wPre, Cat( Fill(33, 1.U(1.W)), 0.U(31.W)), Cat(1.U, 0.U(63.W)))
  val remaOFRes = 0.U
  val byPassQuo = Mux1H( Seq( isDivByZero -> quotDZRes, isDivOverflow -> quotOFRes ))
  val byPassRem = Mux1H( Seq( isDivByZero -> remaDZRes, isDivOverflow -> remaOFRes ))
  val byPassRes = Mux1H(Seq(
                    io.enq.bits.fun.div    -> byPassQuo,
                    io.enq.bits.fun.divu   -> byPassQuo,
                    io.enq.bits.fun.divw   -> byPassQuo,
                    io.enq.bits.fun.divuw  -> byPassQuo,
                    io.enq.bits.fun.remw   -> byPassRem,
                    io.enq.bits.fun.remuw  -> byPassRem,
                    io.enq.bits.fun.rem    -> byPassRem,
                    io.enq.bits.fun.remu   -> byPassRem,
                  ))


  val algDivider = Module(new SRT4Divider(new Mul_iss_info, 64))

    algDivider.io.enq.valid := io.enq.valid & ~divBypass
    algDivider.io.enq.bits  := io.enq.bits
    
    algDivider.io.op1 := dividend_load
    algDivider.io.op2 := divisor_load

    algDivider.io.flush := io.flush

  val op1Post   = algDivider.io.deq.bits.param.dat.op1
  val op2Post   = algDivider.io.deq.bits.param.dat.op2
  val is32wPost = algDivider.io.deq.bits.fun.isDiv32w;
  val isUsiPost = algDivider.io.deq.bits.fun.isDivusi;




  val dividend_sign = Mux(isUsiPost, false.B, Mux(is32wPost, op1Post(31).asBool, op1Post(63).asBool))
  val divisor_sign  = Mux(isUsiPost, false.B, Mux(is32wPost, op2Post(31).asBool, op2Post(63).asBool))

  val quot_sign_corrcet = 
    Mux(dividend_sign^divisor_sign, ~algDivider.io.quo + 1.U, algDivider.io.quo)

  val rema_sign_corrcet = 
    Mux(dividend_sign, ~algDivider.io.rem + 1.U, algDivider.io.rem)


  val divRes = Mux1H(Seq(
    algDivider.io.deq.bits.fun.div    -> quot_sign_corrcet,
    algDivider.io.deq.bits.fun.divu   -> quot_sign_corrcet,
    algDivider.io.deq.bits.fun.rem    -> rema_sign_corrcet,
    algDivider.io.deq.bits.fun.remu   -> rema_sign_corrcet,
    algDivider.io.deq.bits.fun.divw   -> sextXTo(quot_sign_corrcet(31,0), 64),
    algDivider.io.deq.bits.fun.divuw  -> sextXTo(quot_sign_corrcet(31,0), 64),
    algDivider.io.deq.bits.fun.remw   -> sextXTo(rema_sign_corrcet(31,0), 64),
    algDivider.io.deq.bits.fun.remuw  -> sextXTo(rema_sign_corrcet(31,0), 64)
  ))



  val divRtnArb = Module(new Arbiter(gen = new WriteBack_info(dw=64), n = 2))
  divRtnArb.io.in(1).valid := io.enq.valid & divBypass
  divRtnArb.io.in(1).bits.res := byPassRes
  divRtnArb.io.in(1).bits.rd0 := io.enq.bits.param.rd0


  divRtnArb.io.in(0).valid := algDivider.io.deq.valid
  divRtnArb.io.in(0).bits.res := divRes
  divRtnArb.io.in(0).bits.rd0 := algDivider.io.deq.bits.param.rd0

  algDivider.io.deq.ready  := divRtnArb.io.in(0).ready

  io.enq.ready := Mux(divBypass, divRtnArb.io.in(1).ready, algDivider.io.enq.ready)
  io.deq <> divRtnArb.io.out







}


class NorDivider[T<:Data]( pipeType: T, dw: Int ) extends Module {

  class NorDividerIO extends Bundle{
    val enq = Flipped(new DecoupledIO(pipeType))
    val deq = Decoupled(pipeType)
    val op1 = Input(UInt(dw.W))
    val op2 = Input(UInt(dw.W))
    val quo = Output(UInt((dw).W))
    val rem = Output(UInt((dw).W))
    val flush = Input(Bool())     
  }

  val io: NorDividerIO = IO(new NorDividerIO)

  val isDivBusy = RegInit(false.B)

  val cnt = RegInit(0.U(7.W))
  when( io.enq.fire | io.flush ) {
    cnt := 0.U
  } .elsewhen( isDivBusy & cnt =/= 64.U ) {
    cnt := cnt + 1.U
  }

  // val ( cnt, isEnd ) = Counter( 0 until 65 by 1, isDivBusy, io.enq.fire | io.flush)


  io.enq.ready := ~isDivBusy

  when( io.deq.fire | io.flush ) {
    isDivBusy := false.B
  } .elsewhen( io.enq.fire ) {
    isDivBusy := true.B
  } 



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

  when( io.enq.fire ) {
    dividend := io.op1
    divisor := io.op2
  }
  .otherwise {
    dividend := divided

  }

  val pendingInfo = RegEnable( io.enq.bits, io.enq.fire )
  val divFinish = isDivBusy & (cnt === 64.U)
  io.deq.valid := divFinish
  io.deq.bits := pendingInfo
  io.quo := dividend(63,0)
  io.rem := dividend(127,64)
}



class SRT4Divider[T<:Data]( pipeType: T, dw: Int ) extends Module {
  class SRT4DividerIO extends Bundle{
    val enq = Flipped(new DecoupledIO(pipeType))
    val deq = Decoupled(pipeType)
    val op1 = Input(UInt(dw.W))
    val op2 = Input(UInt(dw.W))
    val quo = Output(UInt((dw).W))
    val rem = Output(UInt((dw).W))
    val flush = Input(Bool())     
  }

  val io: SRT4DividerIO = IO(new SRT4DividerIO)


  def preProcess( preDividend: UInt, preDdivisor: UInt ): (UInt, UInt, UInt, UInt) = {

    require( preDividend.getWidth == dw )
    require( preDdivisor.getWidth == dw )


    val dividend = Wire( UInt((dw+4).W) )
    val divisor  = Wire( UInt((dw+4).W ) )
    val iterations = Wire( UInt( (log2Ceil(dw+1)).W) )
    val recovery   = Wire( UInt( (log2Ceil(dw+1)).W) )

    val aShift = PriorityEncoder(preDividend.asBools.reverse)
    val bShift = PriorityEncoder(preDdivisor.asBools.reverse)

    val shiftDiff = Cat(0.U(1.W), bShift).asSInt - Cat(0.U(1.W), aShift).asSInt
    val quoBits   = Mux( shiftDiff < 0.S, 0.U, shiftDiff.asUInt)


    dividend := 
      Mux( quoBits(0), 
        Mux( shiftDiff < 0.S, preDividend << bShift, preDividend << aShift ), 
        Mux( shiftDiff < 0.S, preDividend << bShift, preDividend << aShift )  << 1
      )
    divisor  := (preDdivisor << bShift) << 1 


    iterations := (quoBits + 1.U) >> 1
    // ( bShift + 1.U((log2Ceil(dw+1)).W) ) >> 1

    recovery   := dw.U((log2Ceil(dw+1)).W) - bShift
    return (dividend, divisor, iterations, recovery)
  }


  def QDS( dividendIdx: UInt, divisorIdx: UInt ): UInt = {
    
    require( dividendIdx.getWidth == 7 )
    require( divisorIdx.getWidth  == 4 )


    val qSel = Wire(UInt(3.W))

    val table = Seq(
      Mux1H(Seq(
        ((dividendIdx.asSInt >=  12.S)                                ) -> "b010".U,
        ((dividendIdx.asSInt >=   4.S) && (dividendIdx.asSInt <  12.S)) -> "b001".U,
        ((dividendIdx.asSInt >=  -4.S) && (dividendIdx.asSInt <   4.S)) -> 0.U,
        ((dividendIdx.asSInt >= -13.S) && (dividendIdx.asSInt <  -4.S)) -> "b101".U,
        (                                 (dividendIdx.asSInt < -13.S)) -> "b110".U,
      )),

      Mux1H(Seq(
        ((dividendIdx.asSInt >=  14.S)                                ) -> "b010".U,
        ((dividendIdx.asSInt >=   4.S) && (dividendIdx.asSInt <  14.S)) -> "b001".U,
        ((dividendIdx.asSInt >=  -6.S) && (dividendIdx.asSInt <   4.S)) -> 0.U,
        ((dividendIdx.asSInt >= -15.S) && (dividendIdx.asSInt <  -6.S)) -> "b101".U,
        (                                 (dividendIdx.asSInt < -15.S)) -> "b110".U,

      )),

      Mux1H(Seq(
        ((dividendIdx.asSInt >=  15.S)                                ) -> "b010".U,
        ((dividendIdx.asSInt >=   4.S) && (dividendIdx.asSInt <  15.S)) -> "b001".U,
        ((dividendIdx.asSInt >=  -6.S) && (dividendIdx.asSInt <   4.S)) -> 0.U,
        ((dividendIdx.asSInt >= -16.S) && (dividendIdx.asSInt <  -6.S)) -> "b101".U,
        (                                 (dividendIdx.asSInt < -16.S)) -> "b110".U,

      )),

      Mux1H(Seq(
        ((dividendIdx.asSInt >=  16.S)                                ) -> "b010".U,
        ((dividendIdx.asSInt >=   4.S) && (dividendIdx.asSInt <  16.S)) -> "b001".U,
        ((dividendIdx.asSInt >=  -6.S) && (dividendIdx.asSInt <   4.S)) -> 0.U,
        ((dividendIdx.asSInt >= -18.S) && (dividendIdx.asSInt <  -6.S)) -> "b101".U,
        (                                 (dividendIdx.asSInt < -18.S)) -> "b110".U,

      )),

      Mux1H(Seq(
        ((dividendIdx.asSInt >=  18.S)                                ) -> "b010".U,
        ((dividendIdx.asSInt >=   6.S) && (dividendIdx.asSInt <  18.S)) -> "b001".U,
        ((dividendIdx.asSInt >=  -8.S) && (dividendIdx.asSInt <   6.S)) -> 0.U,
        ((dividendIdx.asSInt >= -20.S) && (dividendIdx.asSInt <  -8.S)) -> "b101".U,
        (                                 (dividendIdx.asSInt < -20.S)) -> "b110".U,

      )),

      Mux1H(Seq(
        ((dividendIdx.asSInt >=  20.S)                                ) -> "b010".U,
        ((dividendIdx.asSInt >=   6.S) && (dividendIdx.asSInt <  20.S)) -> "b001".U,
        ((dividendIdx.asSInt >=  -8.S) && (dividendIdx.asSInt <   6.S)) -> 0.U,
        ((dividendIdx.asSInt >= -20.S) && (dividendIdx.asSInt <  -8.S)) -> "b101".U,
        (                                 (dividendIdx.asSInt < -20.S)) -> "b110".U,

      )),

      Mux1H(Seq(
        ((dividendIdx.asSInt >=  20.S)                                ) -> "b010".U,
        ((dividendIdx.asSInt >=   8.S) && (dividendIdx.asSInt <  20.S)) -> "b001".U,
        ((dividendIdx.asSInt >=  -8.S) && (dividendIdx.asSInt <   8.S)) -> 0.U,
        ((dividendIdx.asSInt >= -22.S) && (dividendIdx.asSInt <  -8.S)) -> "b101".U,
        (                                 (dividendIdx.asSInt < -22.S)) -> "b110".U,

      )),

      Mux1H(Seq(
        ((dividendIdx.asSInt >=  24.S)                                ) -> "b010".U,
        ((dividendIdx.asSInt >=   8.S) && (dividendIdx.asSInt <  24.S)) -> "b001".U,
        ((dividendIdx.asSInt >=  -8.S) && (dividendIdx.asSInt <   8.S)) -> 0.U,
        ((dividendIdx.asSInt >= -24.S) && (dividendIdx.asSInt <  -8.S)) -> "b101".U,
        (                                 (dividendIdx.asSInt < -24.S)) -> "b110".U,

      )),
    )


    qSel := Mux1H(Seq(
      (divisorIdx === "b1000".U) -> table(0),
      (divisorIdx === "b1001".U) -> table(1),
      (divisorIdx === "b1010".U) -> table(2),
      (divisorIdx === "b1011".U) -> table(3),
      (divisorIdx === "b1100".U) -> table(4),
      (divisorIdx === "b1101".U) -> table(5),
      (divisorIdx === "b1110".U) -> table(6),
      (divisorIdx === "b1111".U) -> table(7),
    ))
    return qSel
  }

  def ontheFlyQuotientConversion( qPre: UInt, qmPre: UInt, qSel: UInt ): (UInt, UInt) =  {
    require( qPre.getWidth  == dw )
    require( qmPre.getWidth == dw )
    require( qSel.getWidth  == 3  )

    val qmNext = Wire(UInt(dw.W))
    val qNext  = Wire(UInt(dw.W))
    

    qmNext := Mux1H(Seq(
      ( qSel === "b010".U ) -> Cat( qPre (dw-3, 0), "b01".U(2.W) ),
      ( qSel === "b001".U ) -> Cat( qPre (dw-3, 0), "b00".U(2.W) ),
      ( qSel === "b000".U ) -> Cat( qmPre(dw-3, 0), "b11".U(2.W) ),
      ( qSel === "b101".U ) -> Cat( qmPre(dw-3, 0), "b10".U(2.W) ),
      ( qSel === "b110".U ) -> Cat( qmPre(dw-3, 0), "b01".U(2.W) ),
    ))


    qNext := Mux1H(Seq(
      ( qSel === "b010".U ) -> Cat(qPre (dw-3, 0), "b10".U(2.W) ),
      ( qSel === "b001".U ) -> Cat(qPre (dw-3, 0), "b01".U(2.W) ),
      ( qSel === "b000".U ) -> Cat(qPre (dw-3, 0), "b00".U(2.W) ),
      ( qSel === "b101".U ) -> Cat(qmPre(dw-3, 0), "b11".U(2.W) ),
      ( qSel === "b110".U ) -> Cat(qmPre(dw-3, 0), "b10".U(2.W) ),
    ))
  
    return (qmNext, qNext)
  }

  val ( dividendInit, divisorInit, iterationsInit, recoveryInit ) = preProcess( preDividend = io.op1, preDdivisor = io.op2 )

  val isDivBusy = RegInit(false.B)
  val isRecurrence = RegInit(false.B)




  val cnt        = Reg(UInt( (log2Ceil(dw)).W))
  val ws = Reg(UInt(( dw + 4 ).W))
  val d          = RegEnable(divisorInit, io.enq.fire)
  val qm = Reg(UInt(dw.W))
  val q  = Reg(UInt(dw.W))
  val iterations = RegEnable(iterationsInit, io.enq.fire)
  val recovery   = RegEnable(recoveryInit  , io.enq.fire)


  val dividendIdx = ws(dw+3, dw-3)

  dontTouch(dividendIdx)
  val divisorIdx  = d(dw, dw-3)
  when(isRecurrence) { assert( divisorIdx(3) === 1.U ) }

  val qSel = QDS( dividendIdx, divisorIdx )
  val (qmNext, qNext) = ontheFlyQuotientConversion(q, qm, qSel)

  val pd1 = Wire( UInt((dw+4).W ) ); pd1 := d
  val pd2 = Wire( UInt((dw+4).W ) ); pd2 := d << 1
  val nd1 = Wire( UInt((dw+4).W ) ); nd1 := -(d.asSInt).asUInt
  val nd2 = Wire( UInt((dw+4).W ) ); nd2 := nd1 << 1

  val iterRem = Mux1H(Seq(
    (qSel === "b000".U) -> (ws),
    (qSel === "b001".U) -> (ws + nd1), (qSel === "b010".U) -> (ws + nd2),
    (qSel === "b101".U) -> (ws + pd1), (qSel === "b110".U) -> (ws + pd2),
  ))


  when( io.flush ) {
    isDivBusy    := false.B
  }.elsewhen( io.enq.fire ) {
    assert( isDivBusy === false.B )
    isDivBusy := true.B
  } .elsewhen( io.deq.fire ) {
    assert( isDivBusy === true.B )
    assert( isRecurrence === false.B )
    isDivBusy := false.B
  }

  when( io.flush ) {
    isRecurrence := false.B
  }.elsewhen( io.enq.fire ) {
    assert( isRecurrence === false.B )
    isRecurrence := true.B
  } .elsewhen( (cnt === iterations) & (isRecurrence === true.B) ) {
    isRecurrence := false.B 
  }

  when( io.enq.fire ) {
    ws := dividendInit
    qm := 0.U
    q  := 0.U
    cnt := 0.U
  } .elsewhen(isRecurrence) {
    qm := qmNext
    q  := qNext
    when( cnt =/= iterations ) {
      ws  := iterRem << 2
      cnt := cnt + 1.U
    } .elsewhen( cnt === iterations ) {
      ws  := iterRem
    }    
  }

  val wsFix = Mux( ws(dw+3), ws + pd1, ws )
  val wsExt = Wire( UInt((2*dw+1).W) ); wsExt := wsFix << recovery
  val qFix  = Mux( ws(dw+3), qm, q)


  val pendingInfo = RegEnable( io.enq.bits, io.enq.fire )
  val divFinish = ~isRecurrence & isDivBusy

  io.enq.ready := ~isDivBusy

  io.rem := wsExt( 2*dw, dw+1 )
  io.quo := qFix


  io.deq.valid := divFinish
  io.deq.bits := pendingInfo

}





class FakeMulDiv(implicit p: Parameters) extends MulDivBase {

  io.mul_iss_exe.ready := true.B

  mul_exe_iwb_fifo.io.enq.valid := false.B
  mul_exe_iwb_fifo.io.enq.bits := DontCare

  assert( ~io.mul_iss_exe.valid, "Assert Failed at FakeMulDiv, un-support MulDiv!" )

}


