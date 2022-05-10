

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


package rift2Core.frontend

import chisel3._
import chisel3.util._
import chisel3.util.random._
import base._

import chisel3.experimental.ChiselEnum
import rift2Core.define._
import rift2Core.frontend._
import rift2Core.L1Cache._


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

  def is_req_btb  = is_jalr & ~is_return
  def is_req_ras  = is_return
  def is_req_bim  = is_branch
  def is_req_tage = is_branch
  def is_lock_pipe = is_fencei | is_sfencevma

  

}

// class Info_pd_bd extends Bundle {
//   val info = new Info_preDecode
//   val instr = UInt(32.W)
//   val pc = UInt(64.W)
// }


object PreDecode16( instr16: UInt ): Info_preDecode = {
  require( instr16.width == 16 )
  // val io = IO(new Bundle {
  //   val instr16 = Input( UInt(16.W) )

  //   val info = Output(new Info_preDecode)
  // })
  val info16 = Wire( new Info_preDecode )

  info16.is_rvc := true.B

  info16.is_jal    := (instr16 === BitPat("b101???????????01"))
  info16.is_jalr   := (instr16 === BitPat("b100??????0000010") & instr16(11,7) =/= 0.U)
  info16.is_branch := (instr16 === BitPat("b11????????????01"))
  info16.is_call   := (instr16 === BitPat("b1001?????0000010") & instr16(11,7) =/= 0.U)
  info16.is_return := (instr16 === BitPat("b100000?010000010"))
  info16.is_fencei := false.B
  info16.is_sfencevma := false.B
  info16.imm       :=
    Mux1H( Seq(
      info16.is_jal    -> Cat( Fill(52, instr16(12)), instr16(12), instr16(8), instr16(10,9), instr16(6), instr16(7), instr16(2), instr16(11), instr16(5,3), 0.U(1.W)),
      info16.is_jalr   -> 0.U,
      info16.is_branch -> Cat( Fill(55, instr16(12)), instr16(12), instr16(6,5), instr16(2), instr16(11,10), instr16(4,3), 0.U(1.W))
    ))
  return info16
}



object PreDecode32(instr32: UInt): Info_preDecode = {
  require( instr32.width == 32 )
  // val io = IO(new Bundle {
  //   val instr32 = Input( UInt(32.W) )

  //   val info = Output(new Info_preDecode)
  // })
  val info32 = Wire(new Info_preDecode)

  info32.is_rvc := false.B

  info32.is_jal    := (instr32(6,0) === "b1101111".U)
  info32.is_jalr   := (instr32(6,0) === "b1100111".U)
  info32.is_branch := (instr32(6,0) === "b1100011".U)
  info32.is_call   := ( info32.is_jal | info32.is_jalr ) & ( instr32(11,7) === BitPat("b00?01") ) //is 1 or 5
  info32.is_return := info32.is_jalr & ( instr32(19,15) === BitPat("b00?01") ) & (instr32(19,15) =/= instr32(11,7))
  info32.is_fencei := ( instr32 === BitPat("b?????????????????001?????0001111") )
  info32.is_sfencevma := ( instr32 === BitPat("b0001001??????????000000001110011") )
  info32.imm       :=
    Mux1H( Seq(
      info32.is_jal    -> Cat( Fill(44, instr32(31)), instr32(19,12), instr32(20), instr32(30,21), 0.U(1.W) ),
      info32.is_jalr   -> Cat( Fill(52, instr32(31)), instr32(31,20) ),
      info32.is_branch -> Cat( Fill(52, instr32(31)), instr32(7), instr32(30,25), instr32(11,8), 0.U(1.W) )
    ))
  return info32
}

/**
  * 
  * @todo lock the pineline when xRet comes
  */

class Predecode_ss extends Module with Superscalar{
  val io = IO(new Bundle {
    val if_cmm_shadow = Input(new Info_if_cmm)

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

  pd_bd_fifo.io.enq(0).bits.info := {

    val fault_info = Wire(new Info_preDecode)
      fault_info.is_jal       := false.B
      fault_info.is_jalr      := false.B
      fault_info.is_branch    := false.B
      fault_info.is_call      := false.B
      fault_info.is_return    := false.B
      fault_info.is_rvc       := true.B
      fault_info.is_fencei    := false.B
      fault_info.is_sfencevma := false.B
      fault_info.imm          := 0.U   

    Mux( io.if_cmm_shadow.is_access_fault | io.if_cmm_shadow.is_paging_fault,
      fault_info,
      Mux( is_1st16, pd16(0).io.info, pd32(0).io.info ) //1st00 will not be care         
    )
 
  }


  pd_bd_fifo.io.enq(1).bits.info := Mux( is_2nd16, pd16(1).io.info, pd32(1).io.info ) //2nd00 will not be care

  pd_bd_fifo.io.enq(0).bits.instr :=
    Mux( io.if_cmm_shadow.is_access_fault, "b1001110001000001".U,
      Mux( io.if_cmm_shadow.is_paging_fault, "b1001110001000101".U,
        Mux( is_1st16, io.if_pd(0).bits,     Cat( io.if_pd(1).bits, io.if_pd(0).bits) )
    ))
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

  
  pd_bd_fifo.io.enq(0).valid := ~is_1st00 | (io.if_cmm_shadow.is_access_fault | io.if_cmm_shadow.is_paging_fault)
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



