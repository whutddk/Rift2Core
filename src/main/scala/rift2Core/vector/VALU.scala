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

package rift2Core.backend

import chisel3._
import chisel3.util._
import rift2Core.define._

import rift2Chip._
import org.chipsalliance.cde.config._


// class Alu_function(implicit p: Parameters) extends RiftBundle {
//   val add = Bool()
//   val slt = Bool()
//   val xor = Bool()
//   val or  = Bool()
//   val and = Bool()
//   val sll = Bool()
//   val srl = Bool()
//   val sra = Bool()
// }

class VAlu_param_Bundle(implicit p: Parameters) extends RD_PHY {
  // val is_32w = Bool()
  val is_usi = Bool()

  val dat = new Operation_source(dw=vParams.vlen)
}

class VALU_Issue_Bundle(implicit p: Parameters) extends RiftBundle {
  val fun = new Alu_function
  val param = new VAlu_param_Bundle
}

class Vector_VWriteBack_Bundle(implicit p: Parameters) extends WriteBack_info(dw = vParams.vlen)







// class Adder8 extends Module{

//   class Adder8IO extends Bundle{

//   }

//   val io:
// }


















class VALUIO(implicit p: Parameters) extends RiftBundle{
  val valu_iss_exe = Flipped(new DecoupledIO(new VALU_Issue_Bundle))
  val valu_exe_vwb = Decoupled(new Vector_VWriteBack_Bundle)

  val flush = Input(Bool())   
}




class VALU()(implicit p: Parameters) extends RiftModule{
  val io: VALUIO = IO(new VALUIO)

  val vAlu_exe_vwb_fifo = Module( new Queue( new Vector_VWriteBack_Bundle, 1, true, false ) )
  io.valu_exe_vwb <> valu_exe_vwb_fifo.io.deq
  valu_exe_vwb_fifo.reset := reset.asBool | io.flush

  val ain8 = Wire( Vec( vParams.vlen/8, UInt(8+1.W) ) )
  val bin8 = Wire( Vec( vParams.vlen/8, UInt(8+1.W) ) )
  val sum  = Wire( Vec( vParams.vlen/8, UInt(8+1.W) ) )
  val sum8 = sum.map{ x => x(7,0) }
  val cin = sum.map{ x => x.extract(8) }

  val res8   = Wire( Vec( vParams.vlen/8,   UInt(8+1.W) ) )
  val res16  = Wire( Vec( vParams.vlen/16,  UInt(16+1.W) ) )
  val res32  = Wire( Vec( vParams.vlen/32,  UInt(32+1.W) ) )
  val res64  = Wire( Vec( vParams.vlen/64,  UInt(64+1.W) ) )
  val res128 = Wire( Vec( vParams.vlen/128, UInt(128+1.W) ) )

  val cout8   = Wire( UInt((vParams.vlen/8).W) )
  val cout16  = Wire( UInt((vParams.vlen/16).W) )
  val cout32  = Wire( UInt((vParams.vlen/32).W) )
  val cout64  = Wire( UInt((vParams.vlen/64).W) )
  val cout128 = Wire( UInt((vParams.vlen/128).W) )

  for( i <- 0 until vParams.vlen/8 ){
    ain8(i) := 
    bin8(i) :=
    sum(i) := ain8(i) + bin8(i)



    cout
  }

  for( i <- 0 until vParams.vlen/8 ){
    res8(i) := 
      sum(i) + 
      vmask.extract(i)

    cout8 := Cat( res8.map{ x => x.extract(8) }.reverse )
  }

  for( i <- 0 until vParams.vlen/16 ){
    res16(i) := 
      sum(2*i+1) << 8 +
      sum(2*i) +
      vmask.extract(i)

    cout16 := Cat( res16.map{ x => x.extract(16) }.reverse )
  }

  for( i <- 0 until vParams.vlen/32 ){
    res32(i) := 
      sum(4*i+3) << 24 +
      sum(4*i+2) << 16 +
      sum(4*i+1) << 8 +
      sum(4*i) +
      vmask.extract(i)

    cout32 := Cat( res32.map{ x => x.extract(32) }.reverse )
  }

  for( i <- 0 until vParams.vlen/64 ){
    res64(i) := 
      sum(8*i+7) << 56 +
      sum(8*i+6) << 48 +
      sum(8*i+5) << 40 +
      sum(8*i+4) << 32 +
      sum(8*i+3) << 24 +
      sum(8*i+2) << 16 +
      sum(8*i+1) << 8  +
      sum(8*i) +
      vmask.extract(i)

    cout64 := Cat( res64.map{ x => x.extract(64) }.reverse )
  }

  for( i <- 0 until vParams.vlen/128 ){
    res64(i) := 
      sum(16*i+15) << 120 +
      sum(16*i+14) << 112 +
      sum(16*i+13) << 104 +
      sum(16*i+12) << 96  +
      sum(16*i+11) << 88  +
      sum(16*i+10) << 80  +
      sum(16*i+9 ) << 72  +
      sum(16*i+8 ) << 64  +
      sum(16*i+7 ) << 56  +
      sum(16*i+6 ) << 48  +
      sum(16*i+5 ) << 40  +
      sum(16*i+4 ) << 32  +
      sum(16*i+3 ) << 24  +
      sum(16*i+2 ) << 16  +
      sum(16*i+1 ) << 8   +
      sum(16*i) +
      vmask.extract(i)

    cout128 := Cat( res128.map{ x => x.extract(128) }.reverse )
  }

    res := 
      Mux1H(Seq(
        (dVsew === "b000".U) -> Cat( res8.reverse ),
        (dVsew === "b001".U) -> Cat( res16.reverse ),
        (dVsew === "b010".U) -> Cat( res32.reverse ),
        (dVsew === "b011".U) -> Cat( res64.reverse ),
        (dVsew === "b100".U) -> Cat( res128.reverse ),
      ))

















  val is_usi = io.alu_iss_exe.bits.param.is_usi

  val op1 = io.alu_iss_exe.bits.param.dat.op1
  val op2 = io.alu_iss_exe.bits.param.dat.op2

  def cutTo32(in: UInt) = Cat(Fill( 32, in(31) ), in(31, 0))


  val adder_res = op1 + op2
  val adder_res_32w = cutTo32(adder_res)
  val alu_add_res = Mux(is_32w, adder_res_32w, adder_res)

  val slt_sign_res = Mux( op1.asSInt < op2.asSInt, 1.U, 0.U )
  val slt_unsi_res = Mux( op1 < op2, 1.U, 0.U )

  val alu_slt_res = Mux(is_usi, slt_unsi_res, slt_sign_res)

  val alu_xor_res = op1 ^ op2;
  val alu_or_res  = op1 | op2;
  val alu_and_res = op1 & op2;



  val shift_op2 = Mux(is_32w, op2(4,0), op2(5,0))

  val alu_sll_res  = Mux( is_32w, cutTo32( op1 << shift_op2), op1 << shift_op2 )

  val alu_srl_res  = Mux( is_32w, cutTo32(op1(31,0) >> shift_op2), op1 >> shift_op2)

  val sra_op1_128w = Mux( is_32w, Cat( Fill(96, op1(31)), op1(31,0) ), Cat( Fill(64, op1(63)), op1(63,0) ) )
  val alu_sra_res  = Mux( is_32w, cutTo32(sra_op1_128w >> shift_op2), sra_op1_128w >> shift_op2 )


  val res = Mux1H(Seq(
    io.alu_iss_exe.bits.fun.add -> alu_add_res,
    io.alu_iss_exe.bits.fun.slt -> alu_slt_res,
    io.alu_iss_exe.bits.fun.xor -> alu_xor_res,
    io.alu_iss_exe.bits.fun.or  -> alu_or_res,
    io.alu_iss_exe.bits.fun.and -> alu_and_res,
    io.alu_iss_exe.bits.fun.sll -> alu_sll_res,
    io.alu_iss_exe.bits.fun.srl -> alu_srl_res,
    io.alu_iss_exe.bits.fun.sra -> alu_sra_res,
  ))

  io.alu_iss_exe.ready := alu_exe_iwb_fifo.io.enq.fire

  alu_exe_iwb_fifo.io.enq.valid := io.alu_iss_exe.valid 
  alu_exe_iwb_fifo.io.enq.bits.res := res
  alu_exe_iwb_fifo.io.enq.bits.rd0 := io.alu_iss_exe.bits.param.rd0


}





