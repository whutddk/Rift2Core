
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
import base._

import rift2Core.privilege._
import rift2Chip._
import org.chipsalliance.cde.config._

//read vector Operator
//check commit for vstore
//ignore vstart vl 
//split execute unit
abstract class VecPreIssueBase()(implicit p: Parameters) extends RiftModule{
  class VecPreIssueIO extends Bundle{
    val enq = Vec(rnChn, Flipped(new DecoupledIO(new Dpt_info)))
    val deq = Vec(rnChn, Flipped(new DecoupledIO(new Dpt_info)))

    val molloc = Decoupled(new UInt(5.W)) //molloc at read op
    val readOp = Flipped(Vec( 32, Valid(UInt( (vParams.vlen).W )) ) )
  }

  val io: VecPreIssueIO = IO(new VecPreIssueIO)

  val vecDptInfo = Wire( new Dpt_info )

  val vpuExeInfo = Wire( Vec( vParams.vlen/8, new Dpt_info ) )
  val vlsExeInfo = Wire( Vec( vParams.vlen/8, new Dpt_info ) )

  val vop = Wire( Vec(4, UInt((vParams.vlen).W)) )



}

trait VecPreIssueReadOP{ this: VecPreIssueBase =>
  val idx = Seq( vecDptInfo.phy.vm0, vecDptInfo.phy.rs1, vecDptInfo.phy.rs2, vecDptInfo.phy.rs3 )

  vop(0) := io.readOp(idx(0)).bits
  vop(1) := io.readOp(idx(1)).bits
  vop(2) := io.readOp(idx(2)).bits
  vop(3) := io.readOp(idx(3)).bits
}

trait VecPreIssueVlsSpliter{ this: VecPreIssueBase =>

  val mask8  = Wire( UInt((vParams.vlen/8 ).W))
  val mask16 = Wire( UInt((vParams.vlen/16).W))
  val mask32 = Wire( UInt((vParams.vlen/32).W))
  val mask64 = Wire( UInt((vParams.vlen/64).W))

  mask8  := vop(0)(vParams.vlen/8-1,  0) | Fill( vParams.vlen/8,  ori.vAttach.get.vm )
  mask16 := vop(0)(vParams.vlen/16-1, 0) | Fill( vParams.vlen/16, ori.vAttach.get.vm )
  mask32 := vop(0)(vParams.vlen/32-1, 0) | Fill( vParams.vlen/32, ori.vAttach.get.vm )
  mask64 := vop(0)(vParams.vlen/64-1, 0) | Fill( vParams.vlen/64, ori.vAttach.get.vm )

  for( i <- 0 until vParams.vlen/8 ){
    vlsExeInfo(i).lsu_isa    := vecDptInfo.lsu_isa
    vlsExeInfo(i).alu_isa    := 0.U.asTypeOf(new Alu_isa)
    vlsExeInfo(i).bru_isa    := 0.U.asTypeOf(new Bru_isa)
    vlsExeInfo(i).csr_isa    := 0.U.asTypeOf(new Csr_isa)
    vlsExeInfo(i).mul_isa    := 0.U.asTypeOf(new Mul_isa)
    vlsExeInfo(i).privil_isa := 0.U.asTypeOf(new Privil_isa)
    vlsExeInfo(i).fpu_isa    := 0.U.asTypeOf(new Fpu_isa)
    vlsExeInfo(i).vectorIsa  := 0.U.asTypeOf(new VectorIsa)

    vlsExeInfo(i).param.is_rvc  := false.B
    vlsExeInfo(i).param.pc      := DontCare
    vlsExeInfo(i).param.imm     := vecDptInfo.param.imm
    vlsExeInfo(i).param.rm      := vecDptInfo.param.rm
    vlsExeInfo(i).param.raw.vm0 :=
    vlsExeInfo(i).param.raw.rs1 :=
    vlsExeInfo(i).param.raw.rs2 :=
    vlsExeInfo(i).param.raw.rs3 :=

    vlsExeInfo(i).vAttach.get.eleIdx := 

    vlsExeInfo(i).vAttach.get.nf     := vecDptInfo.vAttach.get.nf
    vlsExeInfo(i).vAttach.get.vm     := vecDptInfo.vAttach.get.vm

    vlsExeInfo(i).vAttach.get.lmulSel  := vecDptInfo.vAttach.get.lmulSel
    vlsExeInfo(i).vAttach.get.nfSel    := vecDptInfo.vAttach.get.nfSel
    vlsExeInfo(i).vAttach.get.widenSel := vecDptInfo.vAttach.get.widenSel

    vlsExeInfo(i).vAttach.get.vtype   := vecDptInfo.vAttach.get.vtype

  }
//       when( ~sbInfo.isRequested(i) ){
//         require( vParams.lsuEntry <= maxRegNum )
//         require( vParams.vlen/8   <= maxRegNum )
//         val eleIdx = i.U

//         sbInfo.splitInfo(i).param.rd0            := eleIdx
//         sbInfo.splitInfo(i).vAttach.get.entrySel := bufIdx
//         sbInfo.splitInfo(i).param.dat.op3 := DontCare
//         sbInfo.splitInfo(i).param.dat.op4 := DontCare

//         when( ori.fun.vle | iss.fun.vlm | iss.fun.vleNff | iss.fun.vleNff ){
//           println("Warning, vlm should be take care before load in vlsu")
//           sbInfo.splitInfo(i).fun.lbu := ori.vAttach.get.vWidth === "b000".U
//           sbInfo.splitInfo(i).fun.lhu := ori.vAttach.get.vWidth === "b001".U
//           sbInfo.splitInfo(i).fun.lwu := ori.vAttach.get.vWidth === "b010".U
//           sbInfo.splitInfo(i).fun.ld  := ori.vAttach.get.vWidth === "b011".U

//           sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + adjAddr( i, ori.vAttach.get.nf, ori.vAttach.get.vWidth )
//           sbInfo.splitInfo(i).param.dat.op2 := DontCare

//           sbInfo.isExeReady := true.B
//         }.elsewhen( iss.fun.vse | iss.fun.vsm ){
//           println("Warning, vsm should be take care before load in vlsu")
//           sbInfo.splitInfo(i).fun.sb := ori.vAttach.get.vWidth === "b000".U
//           sbInfo.splitInfo(i).fun.sh := ori.vAttach.get.vWidth === "b001".U
//           sbInfo.splitInfo(i).fun.sw := ori.vAttach.get.vWidth === "b010".U
//           sbInfo.splitInfo(i).fun.sd := ori.vAttach.get.vWidth === "b011".U

//           sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + adjAddr( i, ori.vAttach.get.nf, ori.vAttach.get.vWidth )
//           sbInfo.splitInfo(i).param.dat.op2 := adjVS3Ele( ori.param.dat.op3, i, ori.vAttach.get.nf, vsew = ori.vAttach.get.vWidth )

//           sbInfo.isExeReady := false.B
//         }.elsewhen( iss.fun.vlse ){
//           sbInfo.splitInfo(i).fun.lbu := ori.vAttach.get.vWidth === "b000".U
//           sbInfo.splitInfo(i).fun.lhu := ori.vAttach.get.vWidth === "b001".U
//           sbInfo.splitInfo(i).fun.lwu := ori.vAttach.get.vWidth === "b010".U
//           sbInfo.splitInfo(i).fun.ld  := ori.vAttach.get.vWidth === "b011".U
           
//           sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + adjAddr( i, ori.vAttach.get.nf, ori.vAttach.get.vWidth ) + ori.param.dat.op2(63,0) * i.U
//           sbInfo.splitInfo(i).param.dat.op2 := DontCare   

//           sbInfo.isExeReady := true.B           
//         }.elsewhen( iss.fun.vsse ){
//           sbInfo.splitInfo(i).fun.sb := ori.vAttach.get.vWidth === "b000".U
//           sbInfo.splitInfo(i).fun.sh := ori.vAttach.get.vWidth === "b001".U
//           sbInfo.splitInfo(i).fun.sw := ori.vAttach.get.vWidth === "b010".U
//           sbInfo.splitInfo(i).fun.sd := ori.vAttach.get.vWidth === "b011".U
           
//           sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + adjAddr( i, ori.vAttach.get.nf, ori.vAttach.get.vWidth ) + ori.param.dat.op2(63,0) * i.U
//           sbInfo.splitInfo(i).param.dat.op2 := adjVSEle( ori.param.dat.op3, i, ori.vAttach.get.nf, vsew = ori.vAttach.get.vWidth )

//           sbInfo.isExeReady := false.B
//         }.elsewhen( iss.fun.vloxei ){
//           sbInfo.splitInfo(i).fun.lbu := ori.vAttach.get.vWidth === "b000".U
//           sbInfo.splitInfo(i).fun.lhu := ori.vAttach.get.vWidth === "b001".U
//           sbInfo.splitInfo(i).fun.lwu := ori.vAttach.get.vWidth === "b010".U
//           sbInfo.splitInfo(i).fun.ld  := ori.vAttach.get.vWidth === "b011".U
           
//           sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + adjAddr( i, ori.vAttach.get.nf, ori.vAttach.get.vWidth ) + adjVSEle( ori.param.dat.op2, i, "b000".U, ori.vAttach.get.vsew ) 
//           sbInfo.splitInfo(i).param.dat.op2 := DontCare

//           sbInfo.isExeReady := true.B
//         }.elsewhen( iss.fun.vsoxei ){
//           sbInfo.splitInfo(i).fun.sb := ori.vAttach.get.vWidth === "b000".U
//           sbInfo.splitInfo(i).fun.sh := ori.vAttach.get.vWidth === "b001".U
//           sbInfo.splitInfo(i).fun.sw := ori.vAttach.get.vWidth === "b010".U
//           sbInfo.splitInfo(i).fun.sd := ori.vAttach.get.vWidth === "b011".U
           
//           sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + adjAddr( i, ori.vAttach.get.nf, ori.vAttach.get.vWidth ) + adjVSEle( ori.param.dat.op2, i, "b000".U, ori.vAttach.get.vsew ) 
//           sbInfo.splitInfo(i).param.dat.op2 := adjVSEle( ori.param.dat.op3, i, ori.vAttach.get.nf, ori.vAttach.get.vsew )

//           sbInfo.isExeReady := false.B
//         }.elsewhen( iss.fun.vlNreN ){

//           sbInfo.splitInfo(i).fun.lbu := true.B//ori.param.vWidth === "b000".U
//           // sbInfo.splitInfo(i).fun.lhu := ori.param.vWidth === "b001".U
//           // sbInfo.splitInfo(i).fun.lwu := ori.param.vWidth === "b010".U
//           // sbInfo.splitInfo(i).fun.ld := ori.param.vWidth === "b011".U

//           sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + ((ori.vAttach.get.nf + 1.U) << log2Ceil(vParams.vlen/8)) + adjAddr( i, nf = "b000".U, vWidth = "b000".U )
//           sbInfo.splitInfo(i).param.dat.op2 := DontCare

//           sbInfo.isExeReady := true.B
//         }.elsewhen( iss.fun.vsNr ){

//           sbInfo.splitInfo(i).fun.sb := true.B//ori.param.vWidth === "b000".U
//           // sbInfo.splitInfo(i).fun.sh := ori.param.vWidth === "b001".U
//           // sbInfo.splitInfo(i).fun.sw := ori.param.vWidth === "b010".U
//           // sbInfo.splitInfo(i).fun.sd := ori.param.vWidth === "b011".U

//           sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + ((ori.vAttach.get.nf + 1.U) << log2Ceil(vParams.vlen/8)) + adjAddr( i, nf = "b000".U, vWidth = "b000".U )
//           sbInfo.splitInfo(i).param.dat.op2 := adjVSEle( ori.param.dat.op3, i, nf = "b000".U, vsew = "b000".U )
//           sbInfo.isExeReady := false.B
//         }.otherwise{
//           assert(false.B, "Assert Failed, a none Vector LSU instr is push into the VScoreboard!")
//         }

//       }
}


trait VecPreIssueVpuSpliter{ this: VecPreIssueBase =>

}



trait VecPreIssueMux{ this: VecPreIssueBase =>

}





class ecPreIssue()(implicit p: Parameters) extends VecPreIssueBase
with VecPreIssueMux
with VecPreIssueReadOP
with VecPreIssueSpliter
