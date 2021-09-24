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

/**
  * read operator 
  * 
  * @note the chn is litmited to 4
  * @note alu req is 0,1,2
  * @note bru req is 0,1,2
  * @note lsu req is 0,1,2
  * @note csr req is 0,1
  * @note mul req is 0,2
  * 
  */ 

class read_op(files: Vec[UInt]) {

  val alu_chn_valid_i = Wire( UInt(2.W) )
  val bru_chn_valid_i = Wire( UInt(2.W) )
  val lsu_chn_valid_i = Wire( UInt(2.W) )
  val csr_chn_valid_i = Wire( UInt(1.W) )
  val mul_chn_valid_i = Wire( UInt(2.W) )

  val alu_chn_phy_i = Wire(Vec(2, UInt(6.W) ))
  val bru_chn_phy_i = Wire(Vec(2, UInt(6.W) ))
  val lsu_chn_phy_i = Wire(Vec(2, UInt(6.W) ))
  val csr_chn_phy_i = Wire(Vec(1, UInt(6.W) ))
  val mul_chn_phy_i = Wire(Vec(2, UInt(6.W) ))

  val alu_chn_valid_o = alu_chn_valid_i === 1.U | alu_chn_valid_i === 2.U
  val csr_chn_valid_o = csr_chn_valid_i === 1.U
  val bru_chn_valid_o = ((alu_chn_valid_i +& bru_chn_valid_i +& csr_chn_valid_i) <= 4.U) & bru_chn_valid_i =/= 0.U
  val lsu_chn_valid_o = ((alu_chn_valid_i +& bru_chn_valid_i +& lsu_chn_valid_i +& csr_chn_valid_i) <= 4.U) & lsu_chn_valid_i =/= 0.U
  val mul_chn_valid_o = ((alu_chn_valid_i +& bru_chn_valid_i +& lsu_chn_valid_i +& csr_chn_valid_i +& mul_chn_valid_i) <= 4.U) & mul_chn_valid_i =/= 0.U


  val phy = {
    val seq0_0 = (alu_chn_valid_i =/= 0.U)
    val seq0_1 = (alu_chn_valid_i === 0.U & csr_chn_valid_i =/= 0.U)
    val seq0_2 = (alu_chn_valid_i === 0.U & csr_chn_valid_i === 0.U & bru_chn_valid_i =/= 0.U)
    val seq0_3 = (alu_chn_valid_i === 0.U & csr_chn_valid_i === 0.U & bru_chn_valid_i === 0.U & lsu_chn_valid_i =/= 0.U)

    val seq0 = Seq(
      (alu_chn_valid_i =/= 0.U) -> alu_chn_phy_i(0),
      (alu_chn_valid_i === 0.U & csr_chn_valid_i =/= 0.U) -> csr_chn_phy_i(0),
      (alu_chn_valid_i === 0.U & csr_chn_valid_i === 0.U & bru_chn_valid_i =/= 0.U) -> bru_chn_phy_i(0),
      (alu_chn_valid_i === 0.U & csr_chn_valid_i === 0.U & bru_chn_valid_i === 0.U & lsu_chn_valid_i =/= 0.U) -> lsu_chn_phy_i(0)
    )

    val seq1_0 = (alu_chn_valid_i === 2.U)
    val seq1_1 = (alu_chn_valid_i === 1.U & csr_chn_valid_i === 1.U)
    val seq1_2 = (( (alu_chn_valid_i +& csr_chn_valid_i) === 0.U ) & bru_chn_valid_i === 2.U )
    val seq1_3 = (( (alu_chn_valid_i +& csr_chn_valid_i) === 1.U ) & bru_chn_valid_i =/= 0.U )
    val seq1_4 = (( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i) === 0.U ) & lsu_chn_valid_i === 2.U )
    val seq1_5 = (( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i) === 1.U ) & lsu_chn_valid_i =/= 0.U )

    val seq1 = Seq(
      (alu_chn_valid_i === 2.U) -> alu_chn_phy_i(1),
      (alu_chn_valid_i === 1.U & csr_chn_valid_i === 1.U) -> csr_chn_phy_i(0),
      (( (alu_chn_valid_i +& csr_chn_valid_i) === 0.U ) & bru_chn_valid_i === 2.U ) -> bru_chn_phy_i(1),
      (( (alu_chn_valid_i +& csr_chn_valid_i) === 1.U ) & bru_chn_valid_i =/= 0.U ) -> bru_chn_phy_i(0),
      (( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i) === 0.U ) & lsu_chn_valid_i === 2.U ) -> lsu_chn_phy_i(1),
      (( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i) === 1.U ) & lsu_chn_valid_i =/= 0.U ) -> lsu_chn_phy_i(0)
    )

    val seq2_0 = (alu_chn_valid_i === 2.U & csr_chn_valid_i === 1.U )
    val seq2_1 = (( (alu_chn_valid_i +& csr_chn_valid_i) === 1.U & bru_chn_valid_i === 2.U ) )
    val seq2_2 = (( (alu_chn_valid_i +& csr_chn_valid_i) === 2.U & bru_chn_valid_i =/= 0.U ) )
    val seq2_3 = (( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i) === 1.U & lsu_chn_valid_i === 2.U ) )
    val seq2_4 = (( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i) === 2.U & lsu_chn_valid_i =/= 0.U ) )
    val seq2_5 = (( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i +& lsu_chn_valid_i ) <= 2.U) & (mul_chn_valid_i === 2.U))

    val seq2 = Seq(
      (alu_chn_valid_i === 2.U & csr_chn_valid_i === 1.U ) -> csr_chn_phy_i(0),
      (( (alu_chn_valid_i +& csr_chn_valid_i) === 1.U & bru_chn_valid_i === 2.U ) ) -> bru_chn_phy_i(1),
      (( (alu_chn_valid_i +& csr_chn_valid_i) === 2.U & bru_chn_valid_i =/= 0.U ) ) -> bru_chn_phy_i(0),
      (( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i) === 1.U & lsu_chn_valid_i === 2.U ) ) -> lsu_chn_phy_i(1),
      (( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i) === 2.U & lsu_chn_valid_i =/= 0.U ) ) -> lsu_chn_phy_i(0),
      (( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i +& lsu_chn_valid_i ) <= 2.U) & (mul_chn_valid_i === 2.U)) -> mul_chn_phy_i(0)
    )

    val seq3_0 = ( (alu_chn_valid_i +& csr_chn_valid_i) === 2.U & bru_chn_valid_i === 2.U)
    val seq3_1 = ( (alu_chn_valid_i +& csr_chn_valid_i) === 3.U & bru_chn_valid_i === 1.U)
    val seq3_2 = ( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i) === 2.U & lsu_chn_valid_i === 2.U)
    val seq3_3 = ( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i) === 3.U & lsu_chn_valid_i === 1.U)
    val seq3_4 = (( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i +& lsu_chn_valid_i ) <= 2.U) & (mul_chn_valid_i === 2.U))

    val seq3 = Seq(
      ( (alu_chn_valid_i +& csr_chn_valid_i) === 2.U & bru_chn_valid_i === 2.U) -> bru_chn_phy_i(1),
      ( (alu_chn_valid_i +& csr_chn_valid_i) === 3.U & bru_chn_valid_i === 1.U) -> bru_chn_phy_i(0),
      ( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i) === 2.U & lsu_chn_valid_i === 2.U) -> lsu_chn_phy_i(1),
      ( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i) === 3.U & lsu_chn_valid_i === 1.U) -> lsu_chn_phy_i(0),
      ( (alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i +& lsu_chn_valid_i ) <= 2.U & (mul_chn_valid_i === 2.U)) -> mul_chn_phy_i(1)
    )

    assert( PopCount(seq0.map(_._1)) <= 1.U )
    assert( PopCount(seq1.map(_._1)) <= 1.U )
    assert( PopCount(seq2.map(_._1)) <= 1.U )
    assert( PopCount(seq3.map(_._1)) <= 1.U )


    VecInit( Mux1H(seq0), Mux1H(seq1), Mux1H(seq2), Mux1H(seq3))
  }
   
    





  val src = VecInit( for ( i <- 0 until 4 ) yield { files(phy(i)) } )

  val alu_chn_src_o = VecInit( src(0), src(1) )

  val csr_chn_src_o = {
    val usdge = alu_chn_valid_i
    Mux1H(Seq(
      (usdge === 0.U) -> VecInit( src(0) ),
      (usdge === 1.U) -> VecInit( src(1) ),
      (usdge === 2.U) -> VecInit( src(2) )
    ))
  }

  val bru_chn_src_o = {
    val usdge = alu_chn_valid_i +& csr_chn_valid_i

    Mux1H(Seq(
      (usdge === 0.U) -> VecInit( src(0), src(1) ),
      (usdge === 1.U) -> VecInit( src(1), src(2) ),
      (usdge === 2.U) -> VecInit( src(2), src(3) ),
      (usdge === 3.U) -> VecInit( src(3), DontCare )
    ))
  }
 
  val lsu_chn_src_o = {
    val usdge = alu_chn_valid_i +& csr_chn_valid_i +& bru_chn_valid_i

    Mux1H(Seq(
      (usdge === 0.U) -> VecInit( src(0), src(1) ),
      (usdge === 1.U) -> VecInit( src(1), src(2) ),
      (usdge === 2.U) -> VecInit( src(2), src(3) ),
      (usdge === 3.U) -> VecInit( src(3), DontCare )
    ))
  }


  val mul_chn_src_o = VecInit( src(2), src(3) )



  assert( alu_chn_valid_i < 3.U, "Assert Fail at read op, alu cannot req src3" )
  assert( bru_chn_valid_i < 3.U, "Assert Fail at read op, bru cannot req src3" )
  assert( lsu_chn_valid_i < 3.U, "Assert Fail at read op, lsu cannot req src3" )
  assert( mul_chn_valid_i < 3.U, "Assert Fail at read op, mul cannot req src3" )

}

abstract class Ele_issue(param: Instruction_param, phy: Reg_phy, log: Vec[UInt]) {

  val rs1_raw = param.rs1_raw
  val rs2_raw = param.rs2_raw
  val rs1_phy = phy.rs1
  val rs2_phy = phy.rs2


  /**
    * check if the rs(i) is x0
    */
  val is_rs1_x0 = rs1_raw === 0.U
  val is_rs2_x0 = rs2_raw === 0.U



  /**
    * check if the rs1 is wrote back
    */ 
  val is_rs1_ready = (log(rs1_phy) === 3.U) | is_rs1_x0

  /**
  * check if the rs2 is wrote back
  */ 
  val is_rs2_ready = (log(rs2_phy) === 3.U) | is_rs2_x0

  // check if an instruction is RAW clearence, each instruction has different rs requirement
  val is_clearRAW = Wire(Bool())

  val src1 = Wire(UInt(64.W))
  val src2 = Wire(UInt(64.W))

  val op_cnt = Wire(UInt(2.W))
}


class Alu_issue(dpt_info: Alu_dpt_info, buf_valid: Bool, log: Vec[UInt], op: read_op) extends Ele_issue(dpt_info.param, dpt_info.phy, log) {
  val alu_iss_info = Wire(new Alu_iss_info)

  is_clearRAW := Mux(
    buf_valid === false.B, false.B, 
      Mux1H(Seq(
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
        
        ( dpt_info.isa.wfi   === true.B) -> true.B,

      ))
    )

  op_cnt := {
    val nzero_cnt = PopCount( Cat(~is_rs1_x0, ~is_rs2_x0) )

    Mux1H(Seq(
      ( dpt_info.isa.lui   === true.B) -> 0.U,
      ( dpt_info.isa.auipc === true.B) -> 0.U,
      ( dpt_info.isa.addi  === true.B) -> Mux( is_rs1_x0, 0.U, 1.U ),
      ( dpt_info.isa.addiw === true.B) -> Mux( is_rs1_x0, 0.U, 1.U ),
      ( dpt_info.isa.slti  === true.B) -> Mux( is_rs1_x0, 0.U, 1.U ),
      ( dpt_info.isa.sltiu === true.B) -> Mux( is_rs1_x0, 0.U, 1.U ),
      ( dpt_info.isa.xori  === true.B) -> Mux( is_rs1_x0, 0.U, 1.U ),
      ( dpt_info.isa.ori   === true.B) -> Mux( is_rs1_x0, 0.U, 1.U ),
      ( dpt_info.isa.andi  === true.B) -> Mux( is_rs1_x0, 0.U, 1.U ),
      ( dpt_info.isa.slli  === true.B) -> Mux( is_rs1_x0, 0.U, 1.U ),
      ( dpt_info.isa.slliw === true.B) -> Mux( is_rs1_x0, 0.U, 1.U ),
      ( dpt_info.isa.srli  === true.B) -> Mux( is_rs1_x0, 0.U, 1.U ),
      ( dpt_info.isa.srliw === true.B) -> Mux( is_rs1_x0, 0.U, 1.U ),
      ( dpt_info.isa.srai  === true.B) -> Mux( is_rs1_x0, 0.U, 1.U ),
      ( dpt_info.isa.sraiw === true.B) -> Mux( is_rs1_x0, 0.U, 1.U ),
      ( dpt_info.isa.add   === true.B) -> nzero_cnt,
      ( dpt_info.isa.addw  === true.B) -> nzero_cnt,
      ( dpt_info.isa.sub   === true.B) -> nzero_cnt,
      ( dpt_info.isa.subw  === true.B) -> nzero_cnt,
      ( dpt_info.isa.sll   === true.B) -> nzero_cnt,
      ( dpt_info.isa.sllw  === true.B) -> nzero_cnt,
      ( dpt_info.isa.slt   === true.B) -> nzero_cnt,
      ( dpt_info.isa.sltu  === true.B) -> nzero_cnt,
      ( dpt_info.isa.xor   === true.B) -> nzero_cnt,
      ( dpt_info.isa.srl   === true.B) -> nzero_cnt,
      ( dpt_info.isa.srlw  === true.B) -> nzero_cnt,
      ( dpt_info.isa.sra   === true.B) -> nzero_cnt,
      ( dpt_info.isa.sraw  === true.B) -> nzero_cnt,
      ( dpt_info.isa.or    === true.B) -> nzero_cnt,
      ( dpt_info.isa.and   === true.B) -> nzero_cnt,
      ( dpt_info.isa.wfi   === true.B) -> 0.U,
    ))	

  }


  op.alu_chn_phy_i(0) := Mux(~is_rs1_x0, dpt_info.phy.rs1, dpt_info.phy.rs2)
  op.alu_chn_phy_i(1) := dpt_info.phy.rs2
          
  src1 := Mux(~is_rs1_x0, op.alu_chn_src_o(0), 0.U )
  src2 := Mux(~is_rs2_x0, Mux(~is_rs1_x0, op.alu_chn_src_o(1), op.alu_chn_src_o(0)), 0.U )


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

    alu_iss_info.param.op1 :=
      Mux1H(Seq(
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
        dpt_info.isa.and    -> src1,

        dpt_info.isa.wfi    -> 0.U,

    ))

    alu_iss_info.param.op2 :=
      Mux1H(Seq(
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
        dpt_info.isa.and    -> src2,

        dpt_info.isa.wfi    -> 0.U
    ))

    alu_iss_info.param.rd0_phy := dpt_info.phy.rd0
  }


}

class Bru_issue(dpt_info: Bru_dpt_info, buf_valid: Bool, log: Vec[UInt], op: read_op) extends Ele_issue(dpt_info.param, dpt_info.phy, log) {
  val bru_iss_info = Wire(new Bru_iss_info)

  is_clearRAW := 
    Mux( buf_valid === false.B, false.B,
        Mux1H(Seq(
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

  op_cnt := {
    val nzero_cnt = PopCount( Cat(~is_rs1_x0, ~is_rs2_x0) )
    Mux1H(Seq(
      dpt_info.isa.jal  -> 0.U,
      dpt_info.isa.jalr -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.beq  -> nzero_cnt,
      dpt_info.isa.bne  -> nzero_cnt,
      dpt_info.isa.blt  -> nzero_cnt,
      dpt_info.isa.bge  -> nzero_cnt,
      dpt_info.isa.bltu -> nzero_cnt,
      dpt_info.isa.bgeu -> nzero_cnt
    ))
  }


  op.bru_chn_phy_i(0) := Mux(~is_rs1_x0, dpt_info.phy.rs1, dpt_info.phy.rs2)
  op.bru_chn_phy_i(1) := dpt_info.phy.rs2
          
  src1 := Mux(~is_rs1_x0, op.bru_chn_src_o(0), 0.U )
  src2 := Mux(~is_rs2_x0, Mux(~is_rs1_x0, op.bru_chn_src_o(1), op.bru_chn_src_o(0)), 0.U )


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

class Lsu_issue (dpt_info: Lsu_dpt_info, buf_valid: Bool, log: Vec[UInt], op: read_op) extends Ele_issue(dpt_info.param, dpt_info.phy, log) {
  val lsu_iss_info = Wire(new Lsu_iss_info)

  is_clearRAW := 
    Mux( buf_valid === false.B, false.B,
      Mux1H(Seq(
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
        dpt_info.isa.sfence_vma -> (is_rs1_ready & is_rs2_ready),
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
        dpt_info.isa.fsd       -> (is_rs1_ready & is_rs2_ready)
      ))
    )

  op_cnt := {
    val nzero_cnt = PopCount( Cat(~is_rs1_x0, ~is_rs2_x0) )
    Mux1H(Seq(
      dpt_info.isa.lb        -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.lh        -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.lw        -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.ld        -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.lbu       -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.lhu       -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.lwu       -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.sb        -> nzero_cnt,
      dpt_info.isa.sh        -> nzero_cnt,
      dpt_info.isa.sw        -> nzero_cnt,
      dpt_info.isa.sd        -> nzero_cnt,
      dpt_info.isa.fence     -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.fence_i   -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.sfence_vma -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.lr_w      -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.sc_w      -> nzero_cnt,
      dpt_info.isa.amoswap_w -> nzero_cnt,
      dpt_info.isa.amoadd_w  -> nzero_cnt,
      dpt_info.isa.amoxor_w  -> nzero_cnt,
      dpt_info.isa.amoand_w  -> nzero_cnt,
      dpt_info.isa.amoor_w   -> nzero_cnt,
      dpt_info.isa.amomin_w  -> nzero_cnt,
      dpt_info.isa.amomax_w  -> nzero_cnt,
      dpt_info.isa.amominu_w -> nzero_cnt,
      dpt_info.isa.amomaxu_w -> nzero_cnt,
      dpt_info.isa.lr_d      -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.sc_d      -> nzero_cnt,
      dpt_info.isa.amoswap_d -> nzero_cnt,
      dpt_info.isa.amoadd_d  -> nzero_cnt,
      dpt_info.isa.amoxor_d  -> nzero_cnt,
      dpt_info.isa.amoand_d  -> nzero_cnt,
      dpt_info.isa.amoor_d   -> nzero_cnt,
      dpt_info.isa.amomin_d  -> nzero_cnt,
      dpt_info.isa.amomax_d  -> nzero_cnt,
      dpt_info.isa.amominu_d -> nzero_cnt,
      dpt_info.isa.amomaxu_d -> nzero_cnt,
      dpt_info.isa.flw       -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.fsw       -> nzero_cnt,
      dpt_info.isa.fld       -> Mux( is_rs1_x0, 0.U, 1.U ),
      dpt_info.isa.fsd       -> nzero_cnt
    ))
  }


  op.lsu_chn_phy_i(0) := Mux(~is_rs1_x0, dpt_info.phy.rs1, dpt_info.phy.rs2)
  op.lsu_chn_phy_i(1) := dpt_info.phy.rs2
          
  src1 := Mux(~is_rs1_x0, op.lsu_chn_src_o(0), 0.U )
  src2 := Mux(~is_rs2_x0, Mux(~is_rs1_x0, op.lsu_chn_src_o(1), op.lsu_chn_src_o(0)), 0.U )

  lsu_iss_info.fun  := dpt_info.isa

  lsu_iss_info.param.op1 := (src1.asSInt + dpt_info.param.imm.asSInt()).asUInt()
  lsu_iss_info.param.op2 := src2


  lsu_iss_info.param.rd0_phy := dpt_info.phy.rd0
}

class Csr_issue (dpt_info: Csr_dpt_info, buf_valid: Bool, log: Vec[UInt], op: read_op) extends Ele_issue(dpt_info.param, dpt_info.phy, log) {
  val csr_iss_info = Wire(new Csr_iss_info)

  is_clearRAW := 
    Mux( buf_valid === false.B, false.B, 
      Mux1H(Seq(
        dpt_info.isa.rw  -> is_rs1_ready,
        dpt_info.isa.rs  -> is_rs1_ready,
        dpt_info.isa.rc  -> is_rs1_ready,
        dpt_info.isa.rwi -> true.B,
        dpt_info.isa.rsi -> true.B,
        dpt_info.isa.rci -> true.B
      ))
    )

  op_cnt := {
      val nzero_cnt = PopCount( Cat(~is_rs1_x0, ~is_rs2_x0) )
      Mux1H(Seq(
        dpt_info.isa.rw  -> Mux( is_rs1_x0, 0.U, 1.U ),
        dpt_info.isa.rs  -> Mux( is_rs1_x0, 0.U, 1.U ),
        dpt_info.isa.rc  -> Mux( is_rs1_x0, 0.U, 1.U ),
        dpt_info.isa.rwi -> 0.U,
        dpt_info.isa.rsi -> 0.U,
        dpt_info.isa.rci -> 0.U
      ))
    }


  op.csr_chn_phy_i(0) := dpt_info.phy.rs1
          
  src1 := Mux(~is_rs1_x0, op.csr_chn_src_o(0), 0.U )
  src2 := DontCare


  {
    csr_iss_info.fun.rc  := dpt_info.isa.rc | dpt_info.isa.rci
    csr_iss_info.fun.rs  := dpt_info.isa.rs | dpt_info.isa.rsi
    csr_iss_info.fun.rw  := dpt_info.isa.rw | dpt_info.isa.rwi

    csr_iss_info.param.op1  := 
      Mux1H(Seq(
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

class Mul_issue (dpt_info: Mul_dpt_info, buf_valid: Bool, log: Vec[UInt], op: read_op) extends Ele_issue(dpt_info.param, dpt_info.phy, log) {
  val mul_iss_info = Wire(new Mul_iss_info)

  is_clearRAW := Mux( buf_valid === false.B, false.B, (is_rs1_ready & is_rs2_ready))

  op_cnt := PopCount( Cat(~is_rs1_x0, ~is_rs2_x0) )


  op.mul_chn_phy_i(0) := Mux(~is_rs1_x0, dpt_info.phy.rs1, dpt_info.phy.rs2)
  op.mul_chn_phy_i(1) := dpt_info.phy.rs2
          
  src1 := Mux(~is_rs1_x0, op.mul_chn_src_o(0), 0.U )
  src2 := Mux(~is_rs2_x0, Mux(~is_rs1_x0, op.mul_chn_src_o(1), op.mul_chn_src_o(0)), 0.U )



  {
    mul_iss_info.fun  := dpt_info.isa

    mul_iss_info.param.op1  := src1
    mul_iss_info.param.op2  := src2

    mul_iss_info.param.rd0_phy := dpt_info.phy.rd0
  }
}



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

    val op = new read_op(io.files)

    //issue mux: issue fifo <> issue logic
    val alu_issue = new Alu_issue (io.alu_dpt_iss.bits, io.alu_dpt_iss.valid, io.log, op)
    val bru_issue = new Bru_issue (io.bru_dpt_iss.bits, io.bru_dpt_iss.valid, io.log, op)
    val lsu_issue = new Lsu_issue (io.lsu_dpt_iss.bits, io.lsu_dpt_iss.valid, io.log, op)
    val csr_issue = new Csr_issue (io.csr_dpt_iss.bits, io.csr_dpt_iss.valid, io.log, op)
    val mul_issue = new Mul_issue (io.mul_dpt_iss.bits, io.mul_dpt_iss.valid, io.log, op)

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

    io.alu_dpt_iss.ready := alu_iss_exe_fifo.io.enq.fire
    io.bru_dpt_iss.ready := bru_iss_exe_fifo.io.enq.fire
    io.lsu_dpt_iss.ready := lsu_iss_exe_fifo.io.enq.fire
    io.csr_dpt_iss.ready := csr_iss_exe_fifo.io.enq.fire
    io.mul_dpt_iss.ready := mul_iss_exe_fifo.io.enq.fire

    alu_iss_exe_fifo.io.enq.valid := alu_issue.is_clearRAW & (alu_issue.op_cnt === 0.U | op.alu_chn_valid_o)
    bru_iss_exe_fifo.io.enq.valid := bru_issue.is_clearRAW & (bru_issue.op_cnt === 0.U | op.bru_chn_valid_o)
    lsu_iss_exe_fifo.io.enq.valid := lsu_issue.is_clearRAW & (lsu_issue.op_cnt === 0.U | op.lsu_chn_valid_o)
    csr_iss_exe_fifo.io.enq.valid := csr_issue.is_clearRAW & (csr_issue.op_cnt === 0.U | op.csr_chn_valid_o)
    mul_iss_exe_fifo.io.enq.valid := mul_issue.is_clearRAW & (mul_issue.op_cnt === 0.U | op.mul_chn_valid_o)

    op.alu_chn_valid_i := Mux(alu_issue.is_clearRAW & alu_iss_exe_fifo.io.enq.ready, alu_issue.op_cnt, 0.U)
    op.bru_chn_valid_i := Mux(bru_issue.is_clearRAW & bru_iss_exe_fifo.io.enq.ready, bru_issue.op_cnt, 0.U)
    op.lsu_chn_valid_i := Mux(lsu_issue.is_clearRAW & lsu_iss_exe_fifo.io.enq.ready, lsu_issue.op_cnt, 0.U)
    op.csr_chn_valid_i := Mux(csr_issue.is_clearRAW & csr_iss_exe_fifo.io.enq.ready, csr_issue.op_cnt, 0.U)
    op.mul_chn_valid_i := Mux(mul_issue.is_clearRAW & mul_iss_exe_fifo.io.enq.ready, mul_issue.op_cnt, 0.U)




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

