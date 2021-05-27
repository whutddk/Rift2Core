/*
* @Author: Ruige Lee
* @Date:   2021-03-25 17:55:52
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-29 15:09:55
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

package rift2Core.backend


import chisel3._
import chisel3.util._
import rift2Core.define._


abstract class Ele_issue(param: Instruction_param, phy: Reg_phy, log: Vec[UInt], files: Vec[UInt]) {

  def rs1_raw = param.rs1_raw
  def rs2_raw = param.rs2_raw
  def rs1_phy = phy.rs1
  def rs2_phy = phy.rs2

  def src1 = Mux((rs1_raw === 0.U), 0.U, files(rs1_phy))
  def src2 = Mux((rs2_raw === 0.U), 0.U, files(rs2_phy))

  // check if the rs is wrote back
  def is_rs1_ready: Bool = (log(rs1_phy) === 3.U) | (rs1_raw === 0.U)
  def is_rs2_ready: Bool = (log(rs2_phy) === 3.U) | (rs2_raw === 0.U)
  // check if an instruction is RAW clearence, each instruction has different rs requirement
  val is_clearRAW = Wire(Bool())

}


class Alu_issue(dpt_info: Alu_dpt_info, buf_valid: Bool, log: Vec[UInt], files: Vec[UInt]) extends Ele_issue(dpt_info.param, dpt_info.phy, log, files) {
  val alu_iss_info = Wire(new Alu_iss_info)


  is_clearRAW := Mux(
            buf_valid === false.B, false.B, 
              MuxCase(DontCare, Array(
                ( dpt_info.isa.lui   === true.B) -> true.B,
                ( dpt_info.isa.auipc === true.B) -> true.B,
                ( dpt_info.isa.addi  === true.B) -> (is_rs1_ready),
                ( dpt_info.isa.addiw === true.B) -> (is_rs1_ready),
                ( dpt_info.isa.slti  === true.B) -> (is_rs1_ready),
                ( dpt_info.isa.sltiu === true.B) -> (is_rs1_ready),
                ( dpt_info.isa.xori  === true.B) -> (is_rs1_ready),
                ( dpt_info.isa.ori   === true.B) -> (is_rs1_ready),
                ( dpt_info.isa.andi  === true.B) -> (is_rs1_ready),
                ( dpt_info.isa.slli  === true.B) -> (is_rs1_ready),
                ( dpt_info.isa.slliw === true.B) -> (is_rs1_ready),
                ( dpt_info.isa.srli  === true.B) -> (is_rs1_ready),
                ( dpt_info.isa.srliw === true.B) -> (is_rs1_ready),
                ( dpt_info.isa.srai  === true.B) -> (is_rs1_ready),
                ( dpt_info.isa.sraiw === true.B) -> (is_rs1_ready),
                ( dpt_info.isa.add   === true.B) -> (is_rs1_ready & is_rs2_ready),
                ( dpt_info.isa.addw  === true.B) -> (is_rs1_ready & is_rs2_ready),
                ( dpt_info.isa.sub   === true.B) -> (is_rs1_ready & is_rs2_ready),
                ( dpt_info.isa.subw  === true.B) -> (is_rs1_ready & is_rs2_ready),
                ( dpt_info.isa.sll   === true.B) -> (is_rs1_ready & is_rs2_ready),
                ( dpt_info.isa.sllw  === true.B) -> (is_rs1_ready & is_rs2_ready),
                ( dpt_info.isa.slt   === true.B) -> (is_rs1_ready & is_rs2_ready),
                ( dpt_info.isa.sltu  === true.B) -> (is_rs1_ready & is_rs2_ready),
                ( dpt_info.isa.xor   === true.B) -> (is_rs1_ready & is_rs2_ready),
                ( dpt_info.isa.srl   === true.B) -> (is_rs1_ready & is_rs2_ready),
                ( dpt_info.isa.srlw  === true.B) -> (is_rs1_ready & is_rs2_ready),
                ( dpt_info.isa.sra   === true.B) -> (is_rs1_ready & is_rs2_ready),
                ( dpt_info.isa.sraw  === true.B) -> (is_rs1_ready & is_rs2_ready),
                ( dpt_info.isa.or    === true.B) -> (is_rs1_ready & is_rs2_ready),
                ( dpt_info.isa.and   === true.B) -> (is_rs1_ready & is_rs2_ready),
              ))	
            )
  
  {
    alu_iss_info.fun.add := dpt_info.isa.is_fun_add
    alu_iss_info.fun.slt := dpt_info.isa.is_fun_slt
    alu_iss_info.fun.xor := dpt_info.isa.is_fun_xor
    alu_iss_info.fun.or  := dpt_info.isa.is_fun_or
    alu_iss_info.fun.and := dpt_info.isa.is_fun_and
    alu_iss_info.fun.sll := dpt_info.isa.is_fun_sll
    alu_iss_info.fun.srl := dpt_info.isa.is_fun_srl
    alu_iss_info.fun.sra := dpt_info.isa.is_fun_sra

    alu_iss_info.param.is_32w  := dpt_info.isa.is_32w
    alu_iss_info.param.is_usi  := dpt_info.isa.is_usi

    alu_iss_info.param.op1 := MuxCase(DontCare, Array(
                  dpt_info.isa.lui    -> 0.U,
                  dpt_info.isa.auipc  -> dpt_info.param.pc,
                  dpt_info.isa.addi   -> src1,
                  dpt_info.isa.addiw  -> src1,
                  dpt_info.isa.slti   -> src1,
                  dpt_info.isa.sltiu  -> src1,
                  dpt_info.isa.xori   -> src1,
                  dpt_info.isa.ori    -> src1,
                  dpt_info.isa.andi   -> src1,
                  dpt_info.isa.slli   -> src1,
                  dpt_info.isa.slliw  -> src1,
                  dpt_info.isa.srli   -> src1,
                  dpt_info.isa.srliw  -> src1,
                  dpt_info.isa.srai   -> src1,
                  dpt_info.isa.sraiw  -> src1,
                  dpt_info.isa.add    -> src1,
                  dpt_info.isa.addw   -> src1,
                  dpt_info.isa.sub    -> src1,
                  dpt_info.isa.subw   -> src1,
                  dpt_info.isa.sll    -> src1,
                  dpt_info.isa.sllw   -> src1,
                  dpt_info.isa.slt    -> src1,
                  dpt_info.isa.sltu   -> src1,
                  dpt_info.isa.xor    -> src1,
                  dpt_info.isa.srl    -> src1,
                  dpt_info.isa.srlw   -> src1,
                  dpt_info.isa.sra    -> src1,
                  dpt_info.isa.sraw   -> src1,
                  dpt_info.isa.or     -> src1,
                  dpt_info.isa.and    -> src1
    ))

    alu_iss_info.param.op2 := MuxCase(DontCare, Array(
                  dpt_info.isa.lui    -> dpt_info.param.imm,
                  dpt_info.isa.auipc  -> dpt_info.param.imm,
                  dpt_info.isa.addi   -> dpt_info.param.imm,
                  dpt_info.isa.addiw  -> dpt_info.param.imm,
                  dpt_info.isa.slti   -> dpt_info.param.imm,
                  dpt_info.isa.sltiu  -> dpt_info.param.imm,
                  dpt_info.isa.xori   -> dpt_info.param.imm,
                  dpt_info.isa.ori    -> dpt_info.param.imm,
                  dpt_info.isa.andi   -> dpt_info.param.imm,
                  dpt_info.isa.slli   -> dpt_info.param.imm(5,0),
                  dpt_info.isa.slliw  -> dpt_info.param.imm(5,0),
                  dpt_info.isa.srli   -> dpt_info.param.imm(5,0),
                  dpt_info.isa.srliw  -> dpt_info.param.imm(5,0),
                  dpt_info.isa.srai   -> dpt_info.param.imm(5,0),
                  dpt_info.isa.sraiw  -> dpt_info.param.imm(5,0),
                  dpt_info.isa.add    -> src2,
                  dpt_info.isa.addw   -> src2,
                  dpt_info.isa.sub    -> (~src2 + 1.U),
                  dpt_info.isa.subw   -> (~src2 + 1.U),
                  dpt_info.isa.sll    -> src2,
                  dpt_info.isa.sllw   -> src2,
                  dpt_info.isa.slt    -> src2,
                  dpt_info.isa.sltu   -> src2,
                  dpt_info.isa.xor    -> src2,
                  dpt_info.isa.srl    -> src2,
                  dpt_info.isa.srlw   -> src2,
                  dpt_info.isa.sra    -> src2,
                  dpt_info.isa.sraw   -> src2,
                  dpt_info.isa.or     -> src2,
                  dpt_info.isa.and    -> src2
    ))

    alu_iss_info.param.rd0_phy := dpt_info.phy.rd0
  }


}

class Bru_issue(dpt_info: Bru_dpt_info, buf_valid: Bool, log: Vec[UInt], files: Vec[UInt]) extends Ele_issue(dpt_info.param, dpt_info.phy, log, files) {
  val bru_iss_info = Wire(new Bru_iss_info)

  is_clearRAW := 
        Mux( buf_valid === false.B, false.B,
            MuxCase(DontCare, Array(
                dpt_info.isa.jal  -> true.B,
                dpt_info.isa.jalr -> is_rs1_ready,
                dpt_info.isa.beq  -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.bne  -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.blt  -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.bge  -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.bltu -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.bgeu -> (is_rs1_ready & is_rs2_ready)
            ))
          )

  {
    bru_iss_info.fun  := dpt_info.isa


    bru_iss_info.param.is_rvc   := dpt_info.param.is_rvc
    bru_iss_info.param.pc   := dpt_info.param.pc
    bru_iss_info.param.imm   := dpt_info.param.imm

    bru_iss_info.param.op1 := src1
    bru_iss_info.param.op2 := src2


    bru_iss_info.param.rd0_phy := dpt_info.phy.rd0
  }


}

class Lsu_issue (dpt_info: Lsu_dpt_info, buf_valid: Bool, log: Vec[UInt], files: Vec[UInt]) extends Ele_issue(dpt_info.param, dpt_info.phy, log, files) {
  val lsu_iss_info = Wire(new Lsu_iss_info)

  is_clearRAW := 
        Mux( buf_valid === false.B, false.B,
          MuxCase(false.B, Array(
                dpt_info.isa.lb        -> is_rs1_ready,
                dpt_info.isa.lh        -> is_rs1_ready,
                dpt_info.isa.lw        -> is_rs1_ready,
                dpt_info.isa.ld        -> is_rs1_ready,
                dpt_info.isa.lbu       -> is_rs1_ready,
                dpt_info.isa.lhu       -> is_rs1_ready,
                dpt_info.isa.lwu       -> is_rs1_ready,
                dpt_info.isa.sb        -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.sh        -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.sw        -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.sd        -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.fence     -> is_rs1_ready,
                dpt_info.isa.fence_i   -> is_rs1_ready,
                dpt_info.isa.lr_w      -> is_rs1_ready,
                dpt_info.isa.sc_w      -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amoswap_w -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amoadd_w  -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amoxor_w  -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amoand_w  -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amoor_w   -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amomin_w  -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amomax_w  -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amominu_w -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amomaxu_w -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.lr_d      -> is_rs1_ready,
                dpt_info.isa.sc_d      -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amoswap_d -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amoadd_d  -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amoxor_d  -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amoand_d  -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amoor_d   -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amomin_d  -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amomax_d  -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amominu_d -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.amomaxu_d -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.flw       -> is_rs1_ready,
                dpt_info.isa.fsw       -> (is_rs1_ready & is_rs2_ready),
                dpt_info.isa.fld       -> is_rs1_ready,
                dpt_info.isa.fsd       -> (is_rs1_ready & is_rs2_ready),				
              ))
            )

    lsu_iss_info.fun  := dpt_info.isa

    lsu_iss_info.param.op1 := (src1.asSInt + dpt_info.param.imm.asSInt()).asUInt()
    lsu_iss_info.param.op2 := src2


    lsu_iss_info.param.rd0_phy := dpt_info.phy.rd0
}

class Csr_issue (dpt_info: Csr_dpt_info, buf_valid: Bool, log: Vec[UInt], files: Vec[UInt]) extends Ele_issue(dpt_info.param, dpt_info.phy, log, files) {
  val csr_iss_info = Wire(new Csr_iss_info)

  is_clearRAW := 
        Mux( buf_valid === false.B, false.B, 
            MuxCase(false.B, Array(
              dpt_info.isa.rw  -> is_rs1_ready,
              dpt_info.isa.rs  -> is_rs1_ready,
              dpt_info.isa.rc  -> is_rs1_ready,
              dpt_info.isa.rwi -> true.B,
              dpt_info.isa.rsi -> true.B,
              dpt_info.isa.rci -> true.B
            ))
          )

  {
    csr_iss_info.fun.rc  := dpt_info.isa.rc | dpt_info.isa.rci
    csr_iss_info.fun.rs  := dpt_info.isa.rs | dpt_info.isa.rsi
    csr_iss_info.fun.rw  := dpt_info.isa.rw | dpt_info.isa.rwi

    csr_iss_info.param.op1  := 
      MuxCase(false.B, Array(
        dpt_info.isa.rw  -> src1,
        dpt_info.isa.rs  -> src1,
        dpt_info.isa.rc  -> src1,
        dpt_info.isa.rwi -> dpt_info.param.rs1_raw,
        dpt_info.isa.rsi -> dpt_info.param.rs1_raw,
        dpt_info.isa.rci -> dpt_info.param.rs1_raw
      ))

    csr_iss_info.param.op2  := dpt_info.param.imm


    csr_iss_info.param.rd0_phy := dpt_info.phy.rd0
  }

}

class Mul_issue (dpt_info: Mul_dpt_info, buf_valid: Bool, log: Vec[UInt], files: Vec[UInt]) extends Ele_issue(dpt_info.param, dpt_info.phy, log, files) {
  val mul_iss_info = Wire(new Mul_iss_info)

  is_clearRAW := Mux( buf_valid === false.B, false.B, (is_rs1_ready & is_rs2_ready))

  {
    mul_iss_info.fun  := dpt_info.isa

    mul_iss_info.param.op1  := src1
    mul_iss_info.param.op2  := src2

    mul_iss_info.param.rd0_phy := dpt_info.phy.rd0

  }
}

// class Fpu_issue (dpt_info: Fpu_dpt_info, buf_valid: Bool, log: Vec[Vec[UInt]], files: Vec[Vec[UInt]]) extends Ele_issue(dpt_info.param, dpt_info.rn, log, files) {
// 	val fpu_iss_info = Wire(new Fpu_iss_info)

// 	override def is_clearRAW = 
// 		if ( buf_valid == false.B ) {
// 			false.B
// 		}
// 		else {
// 		}

// }



class Issue() extends Module {
  val io = IO(new Bundle{
    val alu_dpt_iss = Flipped(new DecoupledIO(new Alu_dpt_info))
    val bru_dpt_iss = Flipped(new DecoupledIO(new Bru_dpt_info))
    val lsu_dpt_iss = Flipped(new DecoupledIO(new Lsu_dpt_info))
    val csr_dpt_iss = Flipped(new DecoupledIO(new Csr_dpt_info))
    val mul_dpt_iss = Flipped(new DecoupledIO(new Mul_dpt_info))
    // val fpu_dpt_iss = Flipped(new DecoupledIO(new Fpu_dpt_info))


    val alu_iss_exe = new DecoupledIO(new Alu_iss_info)
    val bru_iss_exe = new DecoupledIO(new Bru_iss_info)
    val lsu_iss_exe = new DecoupledIO(new Lsu_iss_info)
    val csr_iss_exe = new DecoupledIO(new Csr_iss_info)
    val mul_iss_exe = new DecoupledIO(new Mul_iss_info)

    val log = Vec(64, Input(UInt(2.W)) )
    val files = Vec(64, Input(UInt(64.W)))


    val flush = Input(Bool())
  })
    //dpt buf here

    //issue mux: issue fifo <> issue logic
    val alu_issue = new Alu_issue (io.alu_dpt_iss.bits, io.alu_dpt_iss.valid, io.log, io.files)
    val bru_issue = new Bru_issue (io.bru_dpt_iss.bits, io.bru_dpt_iss.valid, io.log, io.files)
    val lsu_issue = new Lsu_issue (io.lsu_dpt_iss.bits, io.lsu_dpt_iss.valid, io.log, io.files)
    val csr_issue = new Csr_issue (io.csr_dpt_iss.bits, io.csr_dpt_iss.valid, io.log, io.files)
    val mul_issue = new Mul_issue (io.mul_dpt_iss.bits, io.mul_dpt_iss.valid, io.log, io.files)

    //as a ping-pong buf
    val alu_iss_exe_fifo = Module(new Queue (new Alu_iss_info, 1, true, false))
    val bru_iss_exe_fifo = Module(new Queue (new Bru_iss_info, 2))
    val lsu_iss_exe_fifo = Module(new Queue (new Lsu_iss_info, 2))
    val csr_iss_exe_fifo = Module(new Queue (new Csr_iss_info, 2))
    val mul_iss_exe_fifo = Module(new Queue (new Mul_iss_info, 2))
  
    alu_iss_exe_fifo.reset := reset.asBool | io.flush
    bru_iss_exe_fifo.reset := reset.asBool | io.flush
    lsu_iss_exe_fifo.reset := reset.asBool | io.flush
    csr_iss_exe_fifo.reset := reset.asBool | io.flush
    mul_iss_exe_fifo.reset := reset.asBool | io.flush

    lazy val alu_iss_exe_ack = alu_iss_exe_fifo.io.enq.valid & alu_iss_exe_fifo.io.enq.ready
    lazy val bru_iss_exe_ack = bru_iss_exe_fifo.io.enq.valid & bru_iss_exe_fifo.io.enq.ready
    lazy val lsu_iss_exe_ack = lsu_iss_exe_fifo.io.enq.valid & lsu_iss_exe_fifo.io.enq.ready
    lazy val csr_iss_exe_ack = csr_iss_exe_fifo.io.enq.valid & csr_iss_exe_fifo.io.enq.ready
    lazy val mul_iss_exe_ack = mul_iss_exe_fifo.io.enq.valid & mul_iss_exe_fifo.io.enq.ready

    io.alu_dpt_iss.ready := alu_iss_exe_ack
    io.bru_dpt_iss.ready := bru_iss_exe_ack
    io.lsu_dpt_iss.ready := lsu_iss_exe_ack
    io.csr_dpt_iss.ready := csr_iss_exe_ack
    io.mul_dpt_iss.ready := mul_iss_exe_ack

    alu_iss_exe_fifo.io.enq.valid := alu_issue.is_clearRAW
    bru_iss_exe_fifo.io.enq.valid := bru_issue.is_clearRAW
    lsu_iss_exe_fifo.io.enq.valid := lsu_issue.is_clearRAW
    csr_iss_exe_fifo.io.enq.valid := csr_issue.is_clearRAW
    mul_iss_exe_fifo.io.enq.valid := mul_issue.is_clearRAW


    alu_iss_exe_fifo.io.enq.bits := alu_issue.alu_iss_info
    bru_iss_exe_fifo.io.enq.bits := bru_issue.bru_iss_info
    lsu_iss_exe_fifo.io.enq.bits := lsu_issue.lsu_iss_info
    csr_iss_exe_fifo.io.enq.bits := csr_issue.csr_iss_info
    mul_iss_exe_fifo.io.enq.bits := mul_issue.mul_iss_info




    alu_iss_exe_fifo.io.deq <> io.alu_iss_exe
    bru_iss_exe_fifo.io.deq <> io.bru_iss_exe
    lsu_iss_exe_fifo.io.deq <> io.lsu_iss_exe
    csr_iss_exe_fifo.io.deq <> io.csr_iss_exe
    mul_iss_exe_fifo.io.deq <> io.mul_iss_exe


}

