
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
import rift2Chip._
import chipsalliance.rocketchip.config._

class ReadOp_Rsp_Bundle(dw: Int)(implicit p: Parameters) extends RiftBundle{
  val phy = UInt((log2Ceil(regNum)).W)
  val op  = UInt(dw.W)
}



abstract class DptBase ()(implicit p: Parameters) extends RiftModule with HasFPUParameters{
  def dptEntry = 16
  val io = IO(new Bundle{

    val dptReq = Vec(rnChn, Flipped(new DecoupledIO(new Dpt_info)))

    val alu_iss_exe = Vec(aluNum, new DecoupledIO(new Alu_iss_info))
    val mul_iss_exe = Vec(mulNum max 1, new DecoupledIO(new Mul_iss_info))
    val bru_iss_exe = new DecoupledIO(new Bru_iss_info)
    val csr_iss_exe = new DecoupledIO(new Csr_iss_info)
    val lsu_iss_exe = new DecoupledIO(new Lsu_iss_info)
    val fpu_iss_exe = Vec(fpuNum max 1, new DecoupledIO(new Fpu_iss_info))

    val irgLog = Input( Vec(regNum, UInt(2.W)) )
    val frgLog = Input( Vec(regNum, UInt(2.W)) )

    val irgReq = Valid( Vec( opChn, UInt((log2Ceil(regNum)).W) ) )
    val frgReq = Valid( Vec( opChn, UInt((log2Ceil(regNum)).W) ) )    

    val irgRsp = Flipped( Vec( opChn, new ReadOp_Rsp_Bundle(64) ) )
    val frgRsp = Flipped( Vec( opChn, new ReadOp_Rsp_Bundle(65) ) )

    val flush = Input(Bool())
  }
}


abstract class DptBoard()(implicit p: Parameters) extends DptBase {
  val bufValidDnxt = Wire( rnChn, Vec( dptEntry, Bool() ) )
  val bufValid     = RegInit( VecInit( Seq.fill(dptEntry){false.B} ))
  val bufInfo      = Reg( Vec( dptEntry, new Dpt_info              ))
  val bufReqNum    = Wire( Vec( dptEntry, Vec(3, UInt((log2Ceil(regNum)).W)) ))
  val isBufFop     = Reg( Vec( dptEntry, Vec( 3, Bool()    )) )
  val isOpReady    = Reg( Vec( dptEntry, Vec( 3, Bool()    )) )
  val bufOperator  = Reg( Vec( dptEntry, Vec( 3, UInt(65.W))) )
  val entrySel     = for( i <- 0 until rnChn ) yield {
    if( i == 0 ) {
      bufValid.indexWhere( (x:Bool) => (x === false.B) )
    } else {
      bufValidDnxt(i-1).indexWhere( (x:Bool) => (x === false.B) )
    }
  }

  bufValid := bufValidDnxt( rnChn-1 )

  for( i <- 0 until rnChn ) {
    io.dptReq(i).ready := PopCount( bufValid.map{ x => (x === false.B) } ) >= i.U

    when( io.dptReq(i).fire ) {
      if ( i == 0 ) {
        bufValidDnxt(i) := bufValid
      } else {
        bufValidDnxt(i) := bufValidDnxt(i-1)
      }
      bufValidDnxt(i)(entrySel(i)) := true.B
      bufInfo(entrySel(i)) := io.dptReq(i)

    }
  }

  for( i <- 0 until rnChn ) {
    when( io.dptReq(i).fire ) {
      if ( hasFpu ) {
        when( io.dptReq(i).bits.lsu_isa.is_fst ) {
          isBufFop(entrySel(i))(0) := false.B
          isBufFop(entrySel(i))(1) := true.B
          isBufFop(entrySel(i))(2) := false.B
        } .elsewhen( io.dptReq(i).bits.fpu_isa.is_fop ) {
          isBufFop(entrySel(i))(0) := true.B
          isBufFop(entrySel(i))(1) := true.B
          isBufFop(entrySel(i))(2) := true.B
        } .otherwise {
          isBufFop(entrySel(i))(0) := false.B
          isBufFop(entrySel(i))(1) := false.B
          isBufFop(entrySel(i))(2) := false.B
        }
      } else {
        isBufFop(entrySel(i))(0) := false.B
        isBufFop(entrySel(i))(1) := false.B
        isBufFop(entrySel(i))(2) := false.B
      }
    }
  }


  for( i <- 0 until rnChn ) {
    when( io.dptReq(i).fire ) {
      when( io.dptReq(i).bits.phy.rs1 === (regNum-1).U ) {
        isOpReady (entrySel(i))(0) := true.B
        bufOperator(entrySel(i))(0) := 0.U
      }
      when( io.dptReq(i).bits.phy.rs2 === (regNum-1).U ) {
        isOpReady (entrySel(i))(1) := true.B
        bufOperator(entrySel(i))(1) := 0.U
      }
      when( io.dptReq(i).bits.phy.rs3 === (regNum-1).U ) {
        isOpReady (entrySel(i))(2) := true.B
        bufOperator(entrySel(i))(2) := 0.U
      }
    }
  }

  for( i <- 0 until dptEntry ){
    bufReqNum(i)(0) := bufInfo(i).phy.rs1
    bufReqNum(i)(1) := bufInfo(i).phy.rs2
    bufReqNum(i)(2) := bufInfo(i).phy.rs3
  }

}


abstract class DptAgeMatrix()(implicit p: Parameters) extends DptBoard {

  val ageMatrixR = Wire( Vec( dptEntry, Vec(dptEntry, Bool()) ) )
  val ageMatrixW = Reg( Vec( dptEntry, Vec(dptEntry, Bool()) ) )



  for ( i <- 0 until dptEntry ) {
    for( j <- 0 until i ) {
      ageMatrixR(i)(j) := ~ageMatrixW(j)(i)
    }
    ageMatrixR(i)(i) := DontCare
    for( j <- i until dptEntry ) {
      ageMatrixR(i)(j) :=  ageMatrixW(i)(j)
    }
  }

  for( i <- 0 until rnChn ) {
    when( io.dptReq(i).fire ) {
      ageMatrixW(entrySel(i)).map{ x => {x := true.B} }
      ageMatrixW.map{ x => {x(entrySel(i)) := false.B} }
    }
  }

  def MatrixMask( matrixIn: Seq[Seq[Bool]], maskCond: Seq[Bool] ): Seq[Seq[Bool]]= {
    require( matrixIn.length    == dptEntry )
    require( matrixIn(0).length == dptEntry )
    require( maskCond.length    == dptEntry )

    val matrixOut = Wire( Vec( dptEntry, Vec(dptEntry, Bool() ) )
    for ( i <- 0 until dptEntry ){
      for ( j <- 0 until dptEntry ){
        matrixOut(i)(j) := (matrixIn(i)(j) & ~maskCond(j)) | maskCond(i)
      }
    }
    return matrixOut
  }
}

trait DptReadIOp { this: DptAgeMatrix =>
  val ropNum = Wire( rop_chn, UInt((log2Ceil(regNum)).W) )


  /** Whether this rs can request operator reading */
  val isIOpReqReady = Wire( Vec( dptEntry, Vec( 2, Bool() ))) )

  for( i <- 0 until dptEntry ) {
    isIOpReqReady(i)(0) := 
      io.irgLog(bufInfo(i).phy.rs1) === "b11".U & //reg-log is ready
      ~isOpReady(i)(0) & ~io.irgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(0))}}.reduce(_|_) //pending and non-rsp in same cycle
    
    isIOpReqReady(i)(1) :=
      io.irgLog(bufInfo(i).phy.rs2) === "b11".U & //reg-log is ready
      ~isOpReady(i)(1) & ~io.irgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(1))}}.reduce(_|_) //pending and non-rsp in same cycle
  }

  /** Who is the highest priority to read operator in each chn */
  val selMatrixRS1 = Wire( Vec( rop_chn, Vec( dptEntry, Vec(dptEntry, Bool() ) ) ) )
  val selMatrixRS2 = Wire( Vec( rop_chn, Vec( dptEntry, Vec(dptEntry, Bool() ) ) ) )
  val maskCondSelRS1 = Wire( Vec( rop_chn, Vec( dptEntry, Bool() ) ) )
  val maskCondSelRS2 = Wire( Vec( rop_chn, Vec( dptEntry, Bool() ) ) )

  for( chn <- 0 until rop_chn ){
    if( chn == 0 ){
      for ( i <- 0 until dptEntry ){
        maskCondSelRS1(chn)(i) := ~bufValid(i) | ~isIOpReqReady(i)(0)
        maskCondSelRS2(chn)(i) := ~bufValid(i) | ~isIOpReqReady(i)(1)
      }
    } else {
      for ( i <- 0 until dptEntry ){
        maskCondSelRS1(chn)(i) := maskCondSelRS1(chn-1)(i) | (bufReqNum(i)(0) === ropNum(chn-1))
        maskCondSelRS2(chn)(i) := maskCondSelRS2(chn-1)(i) | (bufReqNum(i)(1) === ropNum(chn-1))
      }
    }
    selMatrixRS1(chn) := MatrixMask( ageMatrixR, maskCondSelRS1(chn) )
    selMatrixRS2(chn) := MatrixMask( ageMatrixR, maskCondSelRS2(chn) )

    assert(
      selMatrixRS1(chn).forall( (x: Seq[Bool]) => x.forall( y => (y === true.B) ) ) |
      PopCount( selMatrixRS1(chn).map{(x: Seq[Bool]) => x.forall(y => (y === false.B))} ) === 1.U
    )

    assert(
      selMatrixRS2(chn).forall( (x: Seq[Bool]) => x.forall( y => (y === true.B) ) ) |
      PopCount( selMatrixRS2(chn).map{(x: Seq[Bool]) => x.forall(y => (y === false.B))} ) === 1.U
    )
  }




  for( chn <- 0 until rop_chn ){
    val isRS1NoneReq = selMatrixRS1(chn).map{x => x.map{ y => y === true.B }.reduce(_&_)}.readuce(_&_) //all ture
    val isRS2NoneReq = selMatrixRS2(chn).map{x => x.map{ y => y === true.B }.reduce(_&_)}.readuce(_&_) //all ture

    val selRS1 = 
      Mux1H(( 0 until dptEntry ).map{ i => { (selMatrixRS1(chn)(i).forall( y => (y === false.B) )) -> bufReqNum(i)(0) } }) //index a row which all zero
    val selRS2 = 
      Mux1H(( 0 until dptEntry ).map{ i => { (selMatrixRS2(chn)(i).forall( y => (y === false.B) )) -> bufReqNum(i)(1) } }) //index a row which all zero

    if( chn % 2 == 0 ){
      ropNum(chn) := Mux( ~isRS1NoneReq, selRS1, Mux( ~isRS2NoneReq, selRS2, (regNum-1).U ))
    } else {
      ropNum(chn) := Mux( ~isRS2NoneReq, selRS2, Mux( ~isRS1NoneReq, selRS1, (regNum-1).U ))
    }

    io.irgReq(chn).valid := ~isRS1NoneReq & ~isRS2NoneReq
    io.irgReq(chn).bits  := ropNum(chn)
  }


}

trait DptReadFOp { this: DptAgeMatrix =>

}


abstract class IssueBase()(implicit p: Parameters) extends DptAgeMatrix with DptReadIOp with DptReadFOp




trait IssLoadIOp { this: IssueBase =>
  for( chn <- 0 until rop_chn ){
    when( io.irgRsp(chn).valid ){
      for( i <- 0 until dptEntry ){
        for( rs <- 0 until 2 ){
          when( bufValid(i) & (bufReqNum(i)(rs) === io.irgRsp(chn).bits.phy) & ~isBufFop(i) ){
            when( isOpReady(i)(rs) === true.B ) { printf(s"Warning, re-request op at chn $chn, Entry $i, rs( $rs )") }
            isOpReady(i)(rs)   := true.B
            bufOperator(i)(rs) := io.irgRsp(chn).bits.op            
          }
        }
      }
    }    
  }

}

trait IssLoadFOp { this: IssueBase =>

}

abstract class IssueSel()(implicit p: Parameters) extends IssueBase with IssLoadIOp with IssLoadFOp{
  val postIsOpReady = Wire( Vec( dptEntry, Vec(3, Bool())) )
  val postBufOperator = Wire( Vec( dptEntry, Vec(3, UInt(65.W))) )

  for( i <- 0 until dptEntry ){
    postIsOpReady(i)(0)   := MuxCase( isOpReady,    io.irgRsp.map{x => { (x.valid & (x.bits.phy === bufReqNum(i)(0))) -> true.B    } }  )
    postIsOpReady(i)(1)   := MuxCase( isOpReady,    io.irgRsp.map{x => { (x.valid & (x.bits.phy === bufReqNum(i)(1))) -> true.B    } }  )
    postIsOpReady(i)(2)   := MuxCase( isOpReady,    io.irgRsp.map{x => { (x.valid & (x.bits.phy === bufReqNum(i)(2))) -> true.B    } }  )

    postBufOperator(i)(0) := MuxCase( bufOperator , io.irgRsp.map{x => { (x.valid & (x.bits.phy === bufReqNum(i)(0))) -> x.bits.op } }  )
    postBufOperator(i)(1) := MuxCase( bufOperator , io.irgRsp.map{x => { (x.valid & (x.bits.phy === bufReqNum(i)(1))) -> x.bits.op } }  )
    postBufOperator(i)(2) := MuxCase( bufOperator , io.irgRsp.map{x => { (x.valid & (x.bits.phy === bufReqNum(i)(2))) -> x.bits.op } }  )
  }


}

trait IssSelAlu{ this: IssueSel =>
  def aluNum = 1

  def Pkg_alu_iss(idx: Int): Alu_iss_info = {

    val res = Wire(new Alu_iss_info)
    val src1 = bufOperator(idx)(0)
    val src2 = bufOperator(idx)(1)
    res.fun.add := bufInfo(idx).alu_isa.is_fun_add
    res.fun.slt := bufInfo(idx).alu_isa.is_fun_slt
    res.fun.xor := bufInfo(idx).alu_isa.is_fun_xor
    res.fun.or  := bufInfo(idx).alu_isa.is_fun_or
    res.fun.and := bufInfo(idx).alu_isa.is_fun_and
    res.fun.sll := bufInfo(idx).alu_isa.is_fun_sll
    res.fun.srl := bufInfo(idx).alu_isa.is_fun_srl
    res.fun.sra := bufInfo(idx).alu_isa.is_fun_sra

    res.param.is_32w  := bufInfo(idx).alu_isa.is_32w
    res.param.is_usi  := bufInfo(idx).alu_isa.is_usi


    res.param.dat.op1 :=
      Mux1H(Seq(
        bufInfo(idx).alu_isa.lui    -> 0.U,  bufInfo(idx).alu_isa.auipc  -> extVaddr(bufInfo(idx).param.pc, vlen),
        bufInfo(idx).alu_isa.addi   -> src1, bufInfo(idx).alu_isa.addiw  -> src1,
        bufInfo(idx).alu_isa.slti   -> src1, bufInfo(idx).alu_isa.sltiu  -> src1,
        bufInfo(idx).alu_isa.xori   -> src1, bufInfo(idx).alu_isa.ori    -> src1,
        bufInfo(idx).alu_isa.andi   -> src1, bufInfo(idx).alu_isa.slli   -> src1,
        bufInfo(idx).alu_isa.slliw  -> src1, bufInfo(idx).alu_isa.srli   -> src1,
        bufInfo(idx).alu_isa.srliw  -> src1, bufInfo(idx).alu_isa.srai   -> src1,
        bufInfo(idx).alu_isa.sraiw  -> src1, bufInfo(idx).alu_isa.add    -> src1,
        bufInfo(idx).alu_isa.addw   -> src1, bufInfo(idx).alu_isa.sub    -> src1,
        bufInfo(idx).alu_isa.subw   -> src1, bufInfo(idx).alu_isa.sll    -> src1,
        bufInfo(idx).alu_isa.sllw   -> src1, bufInfo(idx).alu_isa.slt    -> src1,
        bufInfo(idx).alu_isa.sltu   -> src1, bufInfo(idx).alu_isa.xor    -> src1,
        bufInfo(idx).alu_isa.srl    -> src1, bufInfo(idx).alu_isa.srlw   -> src1,
        bufInfo(idx).alu_isa.sra    -> src1, bufInfo(idx).alu_isa.sraw   -> src1,
        bufInfo(idx).alu_isa.or     -> src1, bufInfo(idx).alu_isa.and    -> src1,

        bufInfo(idx).alu_isa.wfi    -> 0.U,

    ))

    res.param.dat.op2 :=
      Mux1H(Seq(
        bufInfo(idx).alu_isa.lui    -> bufInfo(idx).param.imm,      bufInfo(idx).alu_isa.auipc  -> bufInfo(idx).param.imm,
        bufInfo(idx).alu_isa.addi   -> bufInfo(idx).param.imm,      bufInfo(idx).alu_isa.addiw  -> bufInfo(idx).param.imm,
        bufInfo(idx).alu_isa.slti   -> bufInfo(idx).param.imm,      bufInfo(idx).alu_isa.sltiu  -> bufInfo(idx).param.imm,
        bufInfo(idx).alu_isa.xori   -> bufInfo(idx).param.imm,      bufInfo(idx).alu_isa.ori    -> bufInfo(idx).param.imm,
        bufInfo(idx).alu_isa.andi   -> bufInfo(idx).param.imm,      bufInfo(idx).alu_isa.slli   -> bufInfo(idx).param.imm(5,0),
        bufInfo(idx).alu_isa.slliw  -> bufInfo(idx).param.imm(5,0), bufInfo(idx).alu_isa.srli   -> bufInfo(idx).param.imm(5,0),
        bufInfo(idx).alu_isa.srliw  -> bufInfo(idx).param.imm(5,0), bufInfo(idx).alu_isa.srai   -> bufInfo(idx).param.imm(5,0),
        bufInfo(idx).alu_isa.sraiw  -> bufInfo(idx).param.imm(5,0), bufInfo(idx).alu_isa.add    -> src2,
        bufInfo(idx).alu_isa.addw   -> src2,                        bufInfo(idx).alu_isa.sub    -> (~src2 + 1.U),
        bufInfo(idx).alu_isa.subw   -> (~src2 + 1.U),               bufInfo(idx).alu_isa.sll    -> src2,
        bufInfo(idx).alu_isa.sllw   -> src2,                        bufInfo(idx).alu_isa.slt    -> src2,
        bufInfo(idx).alu_isa.sltu   -> src2,                        bufInfo(idx).alu_isa.xor    -> src2,
        bufInfo(idx).alu_isa.srl    -> src2,                        bufInfo(idx).alu_isa.srlw   -> src2,
        bufInfo(idx).alu_isa.sra    -> src2,                        bufInfo(idx).alu_isa.sraw   -> src2,
        bufInfo(idx).alu_isa.or     -> src2,                        bufInfo(idx).alu_isa.and    -> src2,

        bufInfo(idx).alu_isa.wfi    -> 0.U
    ))
    res.param.dat.op3 := 0.U
    res.param.rd0 := bufInfo(idx).phy.rd0
    return res
  }


  val aluIssIdx = Wire( Vec( aluNum, UInt((log2Ceil(dptEntry)).W) ) )
  val aluIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_alu_iss(i) }
  val aluIssFifo = Module(new MultiPortFifo( new Alu_iss_info, aw = ( if(!isMinArea) 4 else log2Ceil(aluNum) ), in = aluNum, out = aluNum ))

  val aluIssMatrix   = Wire( Vec(aluNum, Vec( dptEntry, Vec(dptEntry, Bool() ) )) )
  val maskCondAluIss = Wire( Vec(aluNum, Vec( dptEntry, Bool()) ))


  for( iss <- 0 until aluNum ){
    if( iss == 0 ) {  
      for( i <- 0 until dptEntry ) {
        maskCondAluIss(iss)(i) := 
          ~bufValid(i) |
          ~bufInfo(i).alu_isa.is_alu |
          ~(postIsOpReady(i)(0) & postIsOpReady(i)(1))
      }
    } else {
      for( i <- 0 until dptEntry ) {
        maskCondAluIss(iss)(i) := maskCond(iss-1)(i) | (i.U === aluIssIdx(iss-1))
      }
    }
    aluIssMatrix(iss) := MatrixMask( ageMatrixR, maskCondAluIss(iss) )
    
    assert(
      aluIssMatrix(iss).forall( (x: Seq[Bool]) => x.forall( y => (y === true.B) ) ) |
      PopCount( aluIssMatrix(iss).map{(x: Seq[Bool]) => x.forall(y => (y === false.B))} ) === 1.U
    )
  }
  
  for( iss <- 0 until aluNum ){
    aluIssFifo.io.enq(iss).valid := ~aluIssMatrix(iss).map{x => x.map{ y => y === true.B }.reduce(_&_)}.readuce(_&_) //all ture
    aluIssIdx(iss) := aluIssMatrix(iss).indexWhere( (x: Seq[Bool]) => x.forall( y => (y === false.B) ) ) //index a row which all zero
    aluIssFifo.io.enq(iss).bits  := 
      Mux1H( ( 0 until dptEntry ).map{ i => { (aluIssMatrix(iss)(i).forall( y => ( y === false.B ) )) -> aluIssInfo(i) } } )

    for( i <- 0 until dptEntry ) {
      when( aluIssFifo.io.enq(iss).fire & aluIssIdx(iss) === i.U ) {
        bufValid(i) := false.B
        assert( postIsOpReady(i)(0) & postIsOpReady(i)(1) )
        assert( bufValid(i) )
        assert( bufInfo(i).alu_isa.is_alu )
      }
    }

  }
  aluIssFifo.io.deq <> io.alu_iss_exe

}

trait IssSelMul{ this: IssueSel =>
  def mulNum = 1

  def Pkg_mul_iss(idx: Int): Mul_iss_info = {
    val res = Wire(new Mul_iss_info)
    res.fun := bufInfo(idx).mul_isa
 
    res.param.dat.op1 := bufOperator(idx)(0)
    res.param.dat.op2 := bufOperator(idx)(1)
    res.param.rd0     := bufInfo(idx).phy.rd0
    return res
  }

  if ( mulNum != 0 ) {
    val mulIssIdx  = Wire( Vec(mulNum, UInt((log2Ceil(dptEntry)).W) )  )
    val mulIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_mul_iss(i) }
    val mulIssFifo = Module(new MultiPortFifo( new Mul_iss_info, aw = ( if(!isMinArea) 4 else log2Ceil(mulNum) ) ), in = mulNum, out = mulNum )

    val mulIssMatrix   = Wire( Vec(mulNum, Vec( dptEntry, Vec(dptEntry, Bool() ) )) )
    val maskCondMulIss = Wire( Vec(mulNum, Vec( dptEntry, Bool()) ))

    for( iss <- 0 until mulNum ){
      if( iss == 0 ) {  
        for( i <- 0 until dptEntry ) {
          maskCondMulIss(iss)(i) := 
            ~bufValid(i) |
            ~bufInfo.mul_isa.is_mulDiv |
            ~(postIsOpReady(i)(0) & postIsOpReady(i)(1))
        }
      } else {
        for( i <- 0 until dptEntry ) {
          maskCondMulIss(iss)(i) := maskCond(iss-1)(i) | (i.U === mulIssIdx(iss-1))
        }
      }
      mulIssMatrix(iss) := MatrixMask( ageMatrixR, maskCondMulIss(iss) )
      
      assert(
        mulIssMatrix(iss).forall( (x: Seq[Bool]) => x.forall( y => (y === true.B) ) ) |
        PopCount( mulIssMatrix(iss).map{(x: Seq[Bool]) => x.forall(y => (y === false.B))} ) === 1.U
      )
    }
    
    for( iss <- 0 until mulNum ){
      mulIssFifo.io.enq(iss).valid := ~mulIssMatrix(iss).map{x => x.map{ y => y === true.B }.reduce(_&_)}.readuce(_&_) //all ture
      mulIssIdx(iss) := mulIssMatrix(iss).indexWhere( (x: Seq[Bool]) => x.forall( y => (y === false.B) ) ) //index a row which all zero
      mulIssFifo.io.enq(iss).bits  := 
        Mux1H( ( 0 until dptEntry ).map{ i => { (mulIssMatrix(iss)(i).forall( y => ( y === false.B ) )) -> mulIssInfo(i) } } )

      for( i <- 0 until dptEntry ) {
        when( mulIssFifo.io.enq(iss).fire & mulIssIdx(iss) === i.U ) {
          bufValid(i) := false.B
          assert( postIsOpReady(i)(0) & postIsOpReady(i)(1) )
          assert( bufValid(i) )
          assert( bufInfo(i).mul_isa.is_mulDiv )
        }
      }
    }
    mulIssFifo.io.deq <> io.mul_iss_exe
  } else {
    io.mul_iss_exe(0).valid := false.B
    io.mul_iss_exe(0).bits  := DontCare
  }



}

trait IssSelBru{ this: IssueSel =>

  def Pkg_bru_iss(idx: Int): Bru_iss_info = {
    val res = Wire(new Bru_iss_info)
    res.fun           := bufInfo(idx).bru_isa
    res.param.is_rvc  := bufInfo(idx).param.is_rvc
    res.param.pc      := extVaddr(bufInfo(idx).param.pc, vlen)
    res.param.imm     := bufInfo(idx).param.imm
    res.param.dat.op1 := bufOperator(idx)(0)
    res.param.dat.op2 := bufOperator(idx)(1)
    res.param.rd0     := bufInfo(idx).phy.rd0

    return res
  }

  val bruIssIdx = Wire( UInt((log2Ceil(dptEntry)).W) )
  val bruIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_bru_iss(i) }
  val bruIssFifo = Module(new Queue( new Bru_iss_info, ( if(!isMinArea) 4 else 2 ) ) )

  val bruIssMatrix   = Wire( Vec( dptEntry, Vec(dptEntry, Bool() ) )) 
  val maskCondBruIss = Wire( Vec( dptEntry, Bool()) )

  //only oldest instr will be selected
  for( i <- 0 until dptEntry ) {
    maskCondBruIss(i) := 
      ~bufValid(i) |
      ~bufInfo.bru_isa.is_bru
  }

  bruIssMatrix := MatrixMask( ageMatrixR, maskCondBruIss )
  
  assert(
    bruIssMatrix.forall( (x: Seq[Bool]) => x.forall( y => (y === true.B) ) ) |
    PopCount( bruIssMatrix.map{(x: Seq[Bool]) => x.forall(y => (y === false.B))} ) === 1.U
  )

  bruIssIdx := bruIssMatrix.indexWhere( (x: Seq[Bool]) => x.forall( y => (y === false.B) ) ) //index a row which all zero

  bruIssFifo.io.enq.valid := 
    ( 0 until dptEntry ).map{ i => { bruIssMatrix(i).forall( x => (x === false.B) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) } }.reduce(_|_)

  bruIssFifo.io.enq.bits  := 
    Mux1H( ( 0 until dptEntry ).map{ i => { (bruIssMatrix(i).forall( y => ( y === false.B ) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) ) -> bruIssInfo(i) } } )

  for( i <- 0 until dptEntry ) {
    when( bruIssFifo.io.enq.fire & bruIssIdx === i.U ) {
      bufValid(i) := false.B
      assert( postIsOpReady(i)(0) & postIsOpReady(i)(1) )
      assert( bufValid(i) )
      assert( bufInfo(i).bru_isa.is_bru )
    }
  }

  bruIssFifo.io.deq <> io.bru_iss_exe

}

trait IssSelCsr{ this: IssueSel =>

  def Pkg_csr_iss(idx: Int): Csr_iss_info = {
    val res = Wire(new Csr_iss_info)

    res.fun.rc  := bufInfo(idx).csr_isa.rc | bufInfo(idx).csr_isa.rci
    res.fun.rs  := bufInfo(idx).csr_isa.rs | bufInfo(idx).csr_isa.rsi
    res.fun.rw  := bufInfo(idx).csr_isa.rw | bufInfo(idx).csr_isa.rwi

    res.param.dat.op1 := 
      Mux1H(Seq(
        bufInfo(idx).csr_isa.rw  -> bufOperator(idx)(0), bufInfo(idx).csr_isa.rwi -> bufInfo(idx).param.raw.rs1,
        bufInfo(idx).csr_isa.rs  -> bufOperator(idx)(0), bufInfo(idx).csr_isa.rsi -> bufInfo(idx).param.raw.rs1,
        bufInfo(idx).csr_isa.rc  -> bufOperator(idx)(0), bufInfo(idx).csr_isa.rci -> bufInfo(idx).param.raw.rs1
      ))

    res.param.dat.op2 := bufInfo(idx).param.imm
    res.param.dat.op3 := 0.U
    res.param.rd0     := bufInfo(idx).phy.rd0

    return res
  }

  val csrIssIdx = Wire( UInt((log2Ceil(dptEntry)).W) )
  val csrIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_csr_iss(i) }
  val csrIssFifo = Module(new Queue( new Csr_iss_info, ( if(!isMinArea) 4 else 2 ) ) )

  val csrIssMatrix   = Wire( Vec( dptEntry, Vec(dptEntry, Bool() ) )) 
  val maskCondCsrIss = Wire( Vec( dptEntry, Bool()) )

  //only oldest instr will be selected
  for( i <- 0 until dptEntry ) {
    maskCondCsrIss(i) := 
      ~bufValid(i) |
      ~bufInfo.csr_isa.is_csr
  }

  csrIssMatrix := MatrixMask( ageMatrixR, maskCondCsrIss )
  
  assert(
    csrIssMatrix.forall( (x: Seq[Bool]) => x.forall( y => (y === true.B) ) ) |
    PopCount( csrIssMatrix.map{(x: Seq[Bool]) => x.forall(y => (y === false.B))} ) === 1.U
  )

  csrIssIdx := csrIssMatrix.indexWhere( (x: Seq[Bool]) => x.forall( y => (y === false.B) ) ) //index a row which all zero

  csrIssFifo.io.enq.valid := 
    ( 0 until dptEntry ).map{ i => { csrIssMatrix(i).forall( x => (x === false.B) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) } }.reduce(_|_)

  csrIssFifo.io.enq.bits  := 
    Mux1H( ( 0 until dptEntry ).map{ i => { (csrIssMatrix(i).forall( y => ( y === false.B ) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) ) -> bruIssInfo(i) } } )

  for( i <- 0 until dptEntry ) {
    when( csrIssFifo.io.enq.fire & csrIssIdx === i.U ) {
      bufValid(i) := false.B
      assert( postIsOpReady(i)(0) & postIsOpReady(i)(1) )
      assert( bufValid(i) )
      assert( bufInfo(i).csr_isa.is_csr )
    }
  }

  csrIssFifo.io.deq <> io.csr_iss_exe
}

trait IssSelLsu{ this: IssueSel =>

  def Pkg_lsu_iss( idx: Int ): Lsu_iss_info = {
    val res = Wire(new Lsu_iss_info)

    res.fun := bufInfo(idx).lsu_isa

    res.param.dat.op1 := 
      Mux( (bufInfo(idx).lsu_isa.is_lrsc | bufInfo(idx).lsu_isa.is_amo), bufOperator(idx)(0),  (bufOperator(idx)(0).asSInt + bufInfo(idx).param.imm.asSInt()).asUInt() )
    res.param.dat.op2 :=
      Mux(
        bufInfo(idx).lsu_isa.is_fst,
        ieee(unbox(bufOperator(idx)(1), 1.U, None), t = FType.D),
        bufOperator(idx)(1)
      )
    res.param.dat.op3 := 0.U

    res.param.rd0 := bufInfo(idx).phy.rd0

    return res
  }
  val lsuIssIdx = Wire( UInt((log2Ceil(dptEntry)).W) )
  val lsuIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_lsu_iss(i) }
  val lsuIssFifo = Module(new Queue( new Lsu_iss_info, ( if(!isMinArea) 4 else 2 ) ) )

  val lsuIssMatrix   = Wire( Vec( dptEntry, Vec(dptEntry, Bool() ) )) 
  val maskCondLsuIss = Wire( Vec( dptEntry, Bool()) )

  //only oldest instr will be selected
  for( i <- 0 until dptEntry ) {
    maskCondLsuIss(i) := 
      ~bufValid(i) |
      ~bufInfo.lsu_isa.is_lsu
  }

  lsuIssMatrix := MatrixMask( ageMatrixR, maskCondLsuIss )
  
  assert(
    lsuIssMatrix.forall( (x: Seq[Bool]) => x.forall( y => (y === true.B) ) ) |
    PopCount( lsuIssMatrix.map{(x: Seq[Bool]) => x.forall(y => (y === false.B))} ) === 1.U
  )

  lsuIssIdx := lsuIssMatrix.indexWhere( (x: Seq[Bool]) => x.forall( y => (y === false.B) ) ) //index a row which all zero

  lsuIssFifo.io.enq.valid := 
    ( 0 until dptEntry ).map{ i => { lsuIssMatrix(i).forall( x => (x === false.B) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) } }.reduce(_|_)

  lsuIssFifo.io.enq.bits  := 
    Mux1H( ( 0 until dptEntry ).map{ i => { (lsuIssMatrix(i).forall( y => ( y === false.B ) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) ) -> bruIssInfo(i) } } )

  for( i <- 0 until dptEntry ) {
    when( lsuIssFifo.io.enq.fire & lsuIssIdx === i.U ) {
      bufValid(i) := false.B
      assert( postIsOpReady(i)(0) & postIsOpReady(i)(1) )
      assert( bufValid(i) )
      assert( bufInfo(i).lsu_isa.is_lsu )
    }
  }

  lsuIssFifo.io.deq <> io.lsu_iss_exe
}

trait IssSelFpu{ this: IssueSel =>
  def fpuNum = 1
  require( fpuNum <= 1, s"fpu is not support out-of-order in this Vesrion!" )

  if( fpuNum != 0 ){
    val fpuIssIdx = Wire( Vec( fpuNum, UInt((log2Ceil(dptEntry)).W) ))
    val fpuIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_fpu_iss(i) }
    val fpuIssFifo = Module(new MultiPortFifo( new Fpu_iss_info, aw = ( if(!isMinArea) 4 else log2Ceil(fpuNum) ), in = fpuNum, out = fpuNum ))

    val fpuIssMatrix   = Wire( Vec(fpuNum, Vec( dptEntry, Vec(dptEntry, Bool() ) )) )
    val maskCondFpuIss = Wire( Vec(fpuNum, Vec( dptEntry, Bool()) ))


    for( iss <- 0 until fpuNum ){
      if( iss == 0 ) {  
        for( i <- 0 until dptEntry ) {
          maskCondFpuIss(iss)(i) := 
            ~bufValid(i) |
            ~bufInfo.fpu_isa.is_fpu |
            ~(postIsOpReady(i)(0) & postIsOpReady(i)(1) & postIsOpReady(i)(2))
        }
      } else {
        for( i <- 0 until dptEntry ) {
          maskCondFpuIss(iss)(i) := maskCond(iss-1)(i) | (i.U === fpuIssIdx(iss-1))
        }
      }
      fpuIssMatrix(iss) := MatrixMask( ageMatrixR, maskCondFpuIss(iss) )
      
      assert(
        fpuIssMatrix(iss).forall( (x: Seq[Bool]) => x.forall( y => (y === true.B) ) ) |
        PopCount( fpuIssMatrix(iss).map{(x: Seq[Bool]) => x.forall(y => (y === false.B))} ) === 1.U
      )
    }
    
    for( iss <- 0 until fpuNum ){
      fpuIssFifo.io.enq(iss).valid := ~fpuIssMatrix(iss).map{x => x.map{ y => y === true.B }.reduce(_&_)}.readuce(_&_) //all ture
      fpuIssIdx(iss) := fpuIssMatrix(iss).indexWhere( (x: Seq[Bool]) => x.forall( y => (y === false.B) ) ) //index a row which all zero
      fpuIssFifo.io.enq(iss).bits  := 
        Mux1H( ( 0 until dptEntry ).map{ i => { (fpuIssMatrix(iss)(i).forall( y => ( y === false.B ) )) -> fpuIssInfo(i) } } )

      for( i <- 0 until dptEntry ) {
        when( fpuIssFifo.io.enq(iss).fire & fpuIssIdx(iss) === i.U ) {
          bufValid(i) := false.B
          assert( postIsOpReady(i)(0) & postIsOpReady(i)(1) )
          assert( bufValid(i) )
          assert( bufInfo(i).fpu_isa.is_fpu )
        }
      }
    }
    fpuIssFifo.io.deq <> io.fpu_iss_exe    
  } else {
    io.fpu_iss_exe(0).valid := false.B
    io.fpu_iss_exe(0).bits  := DontCare
  }

}

class Issue()(implicit p: Parameters) extends IssueSel with IssSelAlu with IssSelMul with IssSelBru with IssSelCsr with IssSelLsu with IssSelFpu{
  
}

