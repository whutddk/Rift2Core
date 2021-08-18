/*
* @Author: Ruige Lee
* @Date:   2021-04-12 16:52:15
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-15 16:36:05
*/

/*
  Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

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


package rift2Core.frontend

import chisel3._
import chisel3.util._
import chisel3.util.random._
import base._

import chisel3.experimental.ChiselEnum
import rift2Core.define._
import rift2Core.frontend._


class Info_preDecode extends Bundle {
  val is_jal = Bool()
  val is_jalr = Bool()
  val is_branch = Bool()
  val is_call = Bool()
  val is_return = Bool()
  val is_rvc = Bool()
  val is_fencei = Bool()
  val is_sfencevma = Bool()
  val imm = UInt(64.W)

  def is_pineline_cut = is_jal | is_jalr | is_branch | is_fencei | is_sfencevma
}

class Info_pd_bd extends Bundle {

  val info = new Info_preDecode
  val instr = UInt(32.W)
  val pc = UInt(64.W)
}


class PreDecode16() extends Module{
  val io = IO(new Bundle {
    val instr16 = Input( UInt(16.W) )

    val info = Output(new Info_preDecode)
  })

  io.info.is_rvc := true.B

  io.info.is_jal    := (io.instr16 === BitPat("b101???????????01"))
  io.info.is_jalr   := (io.instr16 === BitPat("b100??????0000010") & io.instr16(11,7) =/= 0.U)
  io.info.is_branch := (io.instr16 === BitPat("b11????????????01"))
  io.info.is_call   := (io.instr16 === BitPat("b1001?????0000010") & io.instr16(11,7) =/= 0.U)
  io.info.is_return := (io.instr16 === BitPat("b100000?010000010"))
  io.info.is_fencei := false.B
  io.info.is_sfencevma := false.B
  io.info.imm       := MuxCase( 0.U, Array(
              io.info.is_jal    -> Cat( Fill(52, io.instr16(12)), io.instr16(12), io.instr16(8), io.instr16(10,9), io.instr16(6), io.instr16(7), io.instr16(2), io.instr16(11), io.instr16(5,3), 0.U(1.W)),
              io.info.is_jalr   -> 0.U,
              io.info.is_branch -> Cat( Fill(55, io.instr16(12)), io.instr16(12), io.instr16(6,5), io.instr16(2), io.instr16(11,10), io.instr16(4,3), 0.U(1.W))
            ))

}



class PreDecode32() extends Module{
  val io = IO(new Bundle {
    val instr32 = Input( UInt(32.W) )

    val info = Output(new Info_preDecode)
  })

  io.info.is_rvc := false.B

  io.info.is_jal    := (io.instr32(6,0) === "b1101111".U)
  io.info.is_jalr   := (io.instr32(6,0) === "b1100111".U)
  io.info.is_branch := (io.instr32(6,0) === "b1100011".U)
  io.info.is_call   := ( io.info.is_jal | io.info.is_jalr ) & ( io.instr32(11,7) === BitPat("b00?01") ) //is 1 or 5
  io.info.is_return := io.info.is_jalr & ( io.instr32(19,15) === BitPat("b00?01") ) & (io.instr32(19,15) =/= io.instr32(11,7))
  io.info.is_fencei := ( io.instr32 === BitPat("b?????????????????001?????0001111") )
  io.info.is_sfencevma := ( io.instr32 === BitPat("b0001001??????????000000001110011") )
  io.info.imm       := MuxCase( 0.U, Array(
              io.info.is_jal    -> Cat( Fill(44, io.instr32(31)), io.instr32(19,12), io.instr32(20), io.instr32(30,21), 0.U(1.W) ),
              io.info.is_jalr   -> Cat( Fill(52, io.instr32(31)), io.instr32(31,20) ),
              io.info.is_branch -> Cat( Fill(52, io.instr32(31)), io.instr32(7), io.instr32(30,25), io.instr32(11,8), 0.U(1.W) )
            ))

}

/**
  * 
  * @todo lock the pineline when xRet comes
  */

class Predecode_ss extends Module with Superscalar{
  val io = IO(new Bundle {

    val if_pd = Vec(4, Flipped(new DecoupledIO(UInt(16.W)) ))
    val pd_bd = Vec(2, new DecoupledIO(new Info_pd_bd))

    val pc_pd = Flipped(new ValidIO( UInt(64.W) ))

  })

  val pd_bd_fifo = Module(new MultiPortFifo( new Info_pd_bd, 4, 2, 2 ))
  io.pd_bd <> pd_bd_fifo.io.deq

  val pd16 = for ( i <- 0 until 2 ) yield { val mdl = Module(new PreDecode16()); mdl }
  val pd32 = for ( i <- 0 until 2 ) yield { val mdl = Module(new PreDecode32()); mdl }
  val pc_qout = RegInit("h80000000".U(64.W))
  
  pd16(0).io.instr16 := io.if_pd(0).bits;                         pd16(1).io.instr16 := io.if_pd(idx_2nd).bits
  pd32(0).io.instr32 := Cat( io.if_pd(1).bits, io.if_pd(0).bits); pd32(1).io.instr32 := Cat( io.if_pd(idx_2nd+1.U).bits, io.if_pd(idx_2nd).bits)

  pd_bd_fifo.io.enq(0).bits.info := Mux( is_1st16, pd16(0).io.info, pd32(0).io.info ) //1st00 will not be care
  pd_bd_fifo.io.enq(1).bits.info := Mux( is_2nd16, pd16(1).io.info, pd32(1).io.info ) //2nd00 will not be care

  pd_bd_fifo.io.enq(0).bits.instr := Mux( is_1st16, io.if_pd(0).bits,     Cat( io.if_pd(1).bits, io.if_pd(0).bits) )
  pd_bd_fifo.io.enq(1).bits.instr := Mux( is_2nd16, io.if_pd(idx_2nd).bits, Cat( io.if_pd(idx_2nd+1.U).bits, io.if_pd(idx_2nd).bits) )

  pd_bd_fifo.io.enq(0).bits.pc    := pc_qout
  pd_bd_fifo.io.enq(1).bits.pc    := Mux( is_1st16, pc_qout + 2.U, pc_qout + 4.U)

  
  





  // val is_iq_ib_fifo_ack = Wire(Vec(2, Bool())); for ( i <- 0 until 2 ) yield is_iq_ib_fifo_ack(i) := iq_ib_fifo.is_enq_ack(i)


  def is_1st00 =	~is_1st16 & ~is_1st32	
  def is_1st16 = 	(io.if_pd(0).valid === true.B) & (io.if_pd(0).bits(1,0) =/= "b11".U)

  def is_1st32 = 	(io.if_pd(0).valid === true.B) &
          (io.if_pd(1).valid === true.B) &
          (io.if_pd(0).bits(1,0) === "b11".U)

  def idx_2nd = Mux( is_1st16, 1.U, 2.U )

  def is_2nd00 = 	(is_1st00) |
          (~is_2nd16 & ~is_2nd32)

  def is_2nd16 = 	(io.if_pd(idx_2nd).valid === true.B) & io.if_pd(idx_2nd).bits(1,0) =/= "b11".U
  def is_2nd32 = 	(io.if_pd(idx_2nd).valid === true.B) &
          (io.if_pd(idx_2nd+1.U).valid === true.B) &
          (io.if_pd(idx_2nd).bits(1,0) === "b11".U)

  
  def is_00p00 = is_2nd00 & is_1st00
  def is_00p16 = is_2nd00 & is_1st16
  def is_00p32 = is_2nd00 & is_1st32
  def is_16p16 = is_2nd16 & is_1st16
  def is_16p32 = is_2nd16 & is_1st32
  def is_32p16 = is_2nd32 & is_1st16
  def is_32p32 = is_2nd32 & is_1st32

  override def is_1st_solo = false.B
  override def is_2nd_solo = is_1st_solo & false.B

  
  pd_bd_fifo.io.enq(0).valid := ~is_1st00
  pd_bd_fifo.io.enq(1).valid := ~is_2nd00


  io.if_pd(0).ready := Mux1H(Seq(
        is_00p00 -> false.B,
        is_00p16 -> pd_bd_fifo.io.enq(0).fire,
        is_16p16 -> pd_bd_fifo.io.enq(0).fire,
        is_32p16 -> pd_bd_fifo.io.enq(0).fire,
        is_00p32 -> pd_bd_fifo.io.enq(0).fire,
        is_16p32 -> pd_bd_fifo.io.enq(0).fire,
        is_32p32 -> pd_bd_fifo.io.enq(0).fire
      ))


  io.if_pd(1).ready := Mux1H(Seq(
        is_00p00 -> false.B,
        is_00p16 -> false.B,
        is_16p16 -> pd_bd_fifo.io.enq(1).fire,
        is_32p16 -> pd_bd_fifo.io.enq(1).fire,
        is_00p32 -> pd_bd_fifo.io.enq(0).fire,
        is_16p32 -> pd_bd_fifo.io.enq(0).fire,
        is_32p32 -> pd_bd_fifo.io.enq(0).fire
  ))

  io.if_pd(2).ready := Mux1H(Seq(
        is_00p00 -> false.B,
        is_00p16 -> false.B,
        is_16p16 -> false.B,
        is_32p16 -> pd_bd_fifo.io.enq(1).fire,
        is_00p32 -> false.B,
        is_16p32 -> pd_bd_fifo.io.enq(1).fire,
        is_32p32 -> pd_bd_fifo.io.enq(1).fire
  ))

  io.if_pd(3).ready := Mux1H(Seq(
        is_00p00 -> false.B,
        is_00p16 -> false.B,
        is_16p16 -> false.B,
        is_32p16 -> false.B,
        is_00p32 -> false.B,
        is_16p32 -> false.B,
        is_32p32 -> pd_bd_fifo.io.enq(1).fire
  ))

  when( io.pc_pd.valid ) {
    pc_qout := io.pc_pd.bits
  }
  .otherwise {
    pc_qout := MuxCase(pc_qout, Array(
      io.if_pd(3).ready -> ( pc_qout + 8.U ),
      io.if_pd(2).ready -> ( pc_qout + 6.U ),
      io.if_pd(1).ready -> ( pc_qout + 4.U ),
      io.if_pd(0).ready -> ( pc_qout + 2.U )
    ))
  }



  pd_bd_fifo.io.flush := io.pc_pd.valid





}



