
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

import rift2Core.privilege._
import rift2Chip._
import chipsalliance.rocketchip.config._





abstract class DptBase ()(implicit p: Parameters) extends RiftModule with HasFPUParameters{
  // def dptEntry = 16

  val io = IO(new Bundle{

    val dptReq = Vec(rnChn, Flipped(new DecoupledIO(new Dpt_info)))

    val alu_iss_exe = Vec(aluNum, new DecoupledIO(new Alu_iss_info))
    val mul_iss_exe = Vec(mulNum max 1, new DecoupledIO(new Mul_iss_info))
    val bru_iss_exe = new DecoupledIO(new Bru_iss_info)
    val csr_iss_exe = new DecoupledIO(new Csr_iss_info)
    val lsu_iss_exe = new DecoupledIO(new Lsu_iss_info)
    val fpu_iss_exe = Vec(fpuNum max 1, new DecoupledIO(new Fpu_iss_info))

    // val vpu_iss_exe = new DecoupledIO(new Vpu_iss_info)

    val irgLog = Input( Vec(xRegNum, UInt(2.W)) )
    val frgLog = Input( Vec(fRegNum, UInt(2.W)) )
    val vrgLog = Input( Vec(vRegNum, UInt(2.W)) )

    val irgReq = Vec( opChn, Valid( UInt((log2Ceil(xRegNum)).W) ) )
    val frgReq = Vec( opChn, Valid( UInt((log2Ceil(fRegNum)).W) ) )    
    val vrgReq = Vec( vParams.opChn, Valid( UInt((log2Ceil(vRegNum)).W) ) )    

    val irgRsp = Flipped( Vec( opChn, Valid(new ReadOp_Rsp_Bundle(64)) ) )
    val frgRsp = Flipped( Vec( opChn, Valid(new ReadOp_Rsp_Bundle(65)) ) )
    val vrgRsp = Flipped( Vec( vParams.opChn, Valid(new ReadOp_Rsp_Bundle(vParams.vlen)) ) )

    val csrfiles = Input(new CSR_Bundle)
    val csrIsReady = Input(new CSR_LOG_Bundle)

    val flush = Input(Bool())
  })
}


abstract class DptBoard()(implicit p: Parameters) extends DptBase {
  val bufValidDnxt = Wire( Vec(rnChn, Vec( dptEntry, Bool() ) ))
  val bufValid     = RegInit( VecInit( Seq.fill(dptEntry){false.B} ))
  val bufInfo      = Reg(  Vec( dptEntry, new Dpt_info              ))
  val bufReqNum    = Wire( Vec( dptEntry, Vec( 5, UInt((log2Ceil(maxRegNum)).W)) ))
  val isBufXop     = Reg(  Vec( dptEntry, Vec( 2, Bool()    )) )
  val isBufFop     = Reg(  Vec( dptEntry, Vec( 3, Bool()    )) )
  val isBufVop     = Reg(  Vec( dptEntry, Vec( 5, Bool()    )) )
  val isOpReady    = Reg(  Vec( dptEntry, Vec( 5, Bool()    )) )
  val bufOperator  = Reg(  Vec( dptEntry, Vec( 5, UInt(vParams.vlen.W))) )

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


    if ( i == 0 ) {
      bufValidDnxt(i) := bufValid
    } else {
      bufValidDnxt(i) := bufValidDnxt(i-1)
    }

    when( io.flush ) {
      bufValidDnxt(i) := 0.U.asTypeOf(bufValidDnxt(i))
    } .elsewhen( io.dptReq(i).fire ) {
      bufValidDnxt(i)(entrySel(i)) := true.B
      bufInfo(entrySel(i)) := io.dptReq(i).bits
    }
  }

  for( i <- 0 until rnChn ) {
    when( io.dptReq(i).fire ) {

      when( io.dptReq(i).bits.isRS1 ) { isBufXop(entrySel(i))(0) := true.B }
      .otherwise{ isBufXop(entrySel(i))(0) := false.B }
      when( io.dptReq(i).bits.isRS2 ) { isBufXop(entrySel(i))(1) := true.B }
      .otherwise{ isBufXop(entrySel(i))(1) := false.B }
      when( io.dptReq(i).bits.isFS1 ) { isBufFop(entrySel(i))(0) := true.B }
      .otherwise{ isBufFop(entrySel(i))(0) := false.B }
      when( io.dptReq(i).bits.isFS2 ) { isBufFop(entrySel(i))(1) := true.B }
      .otherwise{ isBufFop(entrySel(i))(1) := false.B }
      when( io.dptReq(i).bits.isFS3 ) { isBufFop(entrySel(i))(2) := true.B }
      .otherwise{ isBufFop(entrySel(i))(2) := false.B }
      when( io.dptReq(i).bits.isVS1 ) { isBufVop(entrySel(i))(0) := true.B }
      .otherwise{ isBufVop(entrySel(i))(0) := false.B }
      when( io.dptReq(i).bits.isVS2 ) { isBufVop(entrySel(i))(1) := true.B }
      .otherwise{ isBufVop(entrySel(i))(1) := false.B }
      when( io.dptReq(i).bits.isVS3 ) { isBufVop(entrySel(i))(2) := true.B }
      .otherwise{ isBufVop(entrySel(i))(2) := false.B }
      when( io.dptReq(i).bits.isVS4 ) { isBufVop(entrySel(i))(3) := true.B }
      .otherwise{ isBufVop(entrySel(i))(3) := false.B }
      when( io.dptReq(i).bits.isVMS ) { isBufVop(entrySel(i))(4) := true.B }
      .otherwise{ isBufVop(entrySel(i))(4) := false.B }      

                        // //default status
                        // isBufXop(entrySel(i))(0) := true.B
                        // isBufXop(entrySel(i))(1) := true.B

                        // isBufFop(entrySel(i))(0) := false.B
                        // isBufFop(entrySel(i))(1) := false.B
                        // isBufFop(entrySel(i))(2) := false.B

                        // isBufVop(entrySel(i))(0) := false.B
                        // isBufVop(entrySel(i))(1) := false.B
                        // isBufVop(entrySel(i))(2) := false.B
                        // isBufVop(entrySel(i))(3) := false.B

                        // //override
                        // if( hasVector ){
                        //   when(io.dptReq(i).bits.vectorIsa.isLookUpFS1){
                        //     isBufXop(entrySel(i))(0) := false.B
                        //     isBufFop(entrySel(i))(0) := true.B
                        //     isBufVop(entrySel(i))(0) := false.B
                        //   }
                        //   when( io.dptReq(i).bits.vectorIsa.isLookUpVS1 ){
                        //     isBufXop(entrySel(i))(0) := false.B
                        //     isBufFop(entrySel(i))(0) := false.B
                        //     isBufVop(entrySel(i))(0) := true.B
                        //   }
                        //   when( io.dptReq(i).bits.vectorIsa.isLookUpVS2 ){
                        //     isBufXop(entrySel(i))(1) := false.B
                        //     isBufFop(entrySel(i))(1) := false.B
                        //     isBufVop(entrySel(i))(1) := true.B
                        //   }
                        //   when( io.dptReq(i).bits.vectorIsa.isLookUpVS3 ){
                        //     isBufFop(entrySel(i))(2) := false.B
                        //     isBufVop(entrySel(i))(2) := true.B
                        //   }
                        //   when( io.dptReq(i).bits.vectorIsa.isLookUpVS2P | io.dptReq(i).bits.vectorIsa.isLookUpVS3P ){
                        //     isBufVop(entrySel(i))(3) := true.B
                        //   }
                        // } 
                        // if ( fpuNum > 0 ) {
                        //   when( io.dptReq(i).bits.lsu_isa.is_fst ) {
                        //     isBufXop(entrySel(i))(0) := true.B
                        //     isBufXop(entrySel(i))(1) := false.B

                        //     isBufFop(entrySel(i))(0) := false.B
                        //     isBufFop(entrySel(i))(1) := true.B
                        //     isBufFop(entrySel(i))(2) := false.B

                        //     isBufVop(entrySel(i))(0) := false.B
                        //     isBufVop(entrySel(i))(1) := false.B
                        //     isBufVop(entrySel(i))(2) := false.B
                        //     isBufVop(entrySel(i))(3) := false.B
                        //   }
                        //   when( io.dptReq(i).bits.fpu_isa.is_fop ) {
                        //     isBufXop(entrySel(i))(0) := false.B
                        //     isBufXop(entrySel(i))(1) := false.B

                        //     isBufFop(entrySel(i))(0) := true.B
                        //     isBufFop(entrySel(i))(1) := true.B
                        //     isBufFop(entrySel(i))(2) := true.B

                        //     isBufVop(entrySel(i))(0) := false.B
                        //     isBufVop(entrySel(i))(1) := false.B
                        //     isBufVop(entrySel(i))(2) := false.B
                        //     isBufVop(entrySel(i))(3) := false.B
                        //   }
                        // }
    }
  }



  for( i <- 0 until rnChn ) {
    when( io.dptReq(i).fire ) {
      when( io.dptReq(i).bits.phy.rs1 === 0.U ) {
        isOpReady (entrySel(i))(0) := true.B
        bufOperator(entrySel(i))(0) := 0.U
      } .otherwise {
        isOpReady (entrySel(i))(0) := false.B
      }
      when( io.dptReq(i).bits.phy.rs2 === 0.U ) {
        isOpReady (entrySel(i))(1) := true.B
        bufOperator(entrySel(i))(1) := 0.U
      } .otherwise{
        isOpReady (entrySel(i))(1) := false.B
      }
      when( io.dptReq(i).bits.phy.rs3 === 0.U ) {
        isOpReady (entrySel(i))(2) := true.B
        bufOperator(entrySel(i))(2) := 0.U
      } .otherwise {
        isOpReady (entrySel(i))(2) := false.B
      }
      when( io.dptReq(i).bits.phy.rs4 === 0.U ) {
        isOpReady (entrySel(i))(3) := true.B
        bufOperator(entrySel(i))(3) := 0.U
      } .otherwise {
        isOpReady (entrySel(i))(3) := false.B
      }
      when( io.dptReq(i).bits.phy.rs5 === 0.U ) {
        isOpReady (entrySel(i))(4) := true.B
        bufOperator(entrySel(i))(4) := 0.U
      } .otherwise {
        isOpReady (entrySel(i))(4) := false.B
      }
    }
  }

  for( i <- 0 until dptEntry ){
    dontTouch(bufReqNum)
    bufReqNum(i)(0) := bufInfo(i).phy.rs1
    bufReqNum(i)(1) := bufInfo(i).phy.rs2
    bufReqNum(i)(2) := bufInfo(i).phy.rs3
    bufReqNum(i)(3) := bufInfo(i).phy.rs4
    bufReqNum(i)(4) := bufInfo(i).phy.rs5
  }

}


abstract class DptAgeMatrix()(implicit p: Parameters) extends DptBoard {


  val ageMatrixR = Wire( Vec( dptEntry, Vec(dptEntry, Bool()) ) )
  val ageMatrixW = Reg( Vec( dptEntry, Vec(dptEntry, Bool()) ) )
  // dontTouch(ageMatrixR)

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

  val ageMatrixPostR  = Wire( Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool()) ) )
  for( i <- 0 until dptEntry+rnChn ) {
    ageMatrixPostR(i)(i) := DontCare
    for( j <- 0 until dptEntry+rnChn ) {
      if( i < dptEntry  && j < dptEntry  )  ageMatrixPostR(i)(j) := ageMatrixR(i)(j)
      if( i < dptEntry  && j >= dptEntry )  ageMatrixPostR(i)(j) := false.B
      if( i >= dptEntry && j < dptEntry  )  ageMatrixPostR(i)(j) := true.B
      if( i >= dptEntry && j >= dptEntry ) {
        if( i < j ) ageMatrixPostR(i)(j) := false.B
        if( i > j ) ageMatrixPostR(i)(j) := true.B
      }
    }
  }


  def MatrixMask( matrixIn: Vec[Vec[Bool]], maskCond: Vec[Bool] ): Vec[Vec[Bool]]= {
    val len = matrixIn.length
    require( matrixIn.length    == len )
    require( matrixIn(0).length == len )
    require( maskCond.length    == len )

    val matrixOut = Wire( Vec( len, Vec(len, Bool()) ))
    for ( i <- 0 until len ){
      for ( j <- 0 until len ){
        matrixOut(i)(j) := (matrixIn(i)(j) & ~maskCond(j)) | maskCond(i)
      }
    }
    return matrixOut
  }
}

trait DptReadIOp { this: DptAgeMatrix =>
  val rIOpNum = Wire( Vec( opChn, UInt((log2Ceil(xRegNum)).W)) )


  /** Whether this rs can request operator reading */
  val canIOpReq     = Wire( Vec( dptEntry, Vec( 2, Bool() )))


  for( i <- 0 until dptEntry ) {
    canIOpReq(i)(0) := 
      io.irgLog(bufInfo(i).phy.rs1) === "b11".U & //reg-log is ready
      isBufXop(i)(0) & //is a Xop req
      ~isOpReady(i)(0) & ~io.irgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(0))}}.reduce(_|_) //pending and non-rsp in same cycle
    
    canIOpReq(i)(1) :=
      io.irgLog(bufInfo(i).phy.rs2) === "b11".U & //reg-log is ready
      isBufXop(i)(1) & //is a Xop req
      ~isOpReady(i)(1) & ~io.irgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(1))}}.reduce(_|_) //pending and non-rsp in same cycle
  }

  val canIOpPostReq = Wire( Vec( rnChn, Vec( 2, Bool() )) )

  for( i <- 0 until rnChn ) {
    canIOpPostReq(i)(0) := 
      // io.dptReq(i).fire &
      io.irgLog(io.dptReq(i).bits.phy.rs1) === "b11".U & //reg-log is ready
      io.dptReq(i).bits.phy.rs1 =/= 0.U &
      io.dptReq(i).bits.isRS1
    
    canIOpPostReq(i)(1) :=
      // io.dptReq(i).fire &
      io.irgLog(io.dptReq(i).bits.phy.rs2) === "b11".U & //reg-log is ready
      io.dptReq(i).bits.phy.rs2 =/= 0.U &
       io.dptReq(i).bits.isRS2
  }



  /** Who is the highest priority to read operator in each chn */
  val selMatrixIRS1 = Wire( Vec( opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
  val selMatrixIRS2 = Wire( Vec( opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
  val maskCondSelIRS1 = Wire( Vec( opChn, Vec( dptEntry+rnChn, Bool() ) ) )
  val maskCondSelIRS2 = Wire( Vec( opChn, Vec( dptEntry+rnChn, Bool() ) ) )



  for( chn <- 0 until opChn ){
    if( chn == 0 ){
      for ( i <- 0 until dptEntry ){
        maskCondSelIRS1(chn)(i) := ~bufValid(i) | ~canIOpReq(i)(0)
        maskCondSelIRS2(chn)(i) := ~bufValid(i) | ~canIOpReq(i)(1)
      }
      for ( i <- 0 until rnChn ){
        maskCondSelIRS1(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canIOpPostReq(i)(0)
        maskCondSelIRS2(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canIOpPostReq(i)(1)
      }
    } else {
      for ( i <- 0 until dptEntry ){
        maskCondSelIRS1(chn)(i) := maskCondSelIRS1(chn-1)(i) | (bufReqNum(i)(0) === rIOpNum(chn-1))
        maskCondSelIRS2(chn)(i) := maskCondSelIRS2(chn-1)(i) | (bufReqNum(i)(1) === rIOpNum(chn-1))
      }
      for( i <- 0 until rnChn ){
        maskCondSelIRS1(chn)(dptEntry+i) := maskCondSelIRS1(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs1 === rIOpNum(chn-1))
        maskCondSelIRS2(chn)(dptEntry+i) := maskCondSelIRS2(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs2 === rIOpNum(chn-1))        
      }
    }
    selMatrixIRS1(chn) := MatrixMask( ageMatrixPostR, maskCondSelIRS1(chn) )
    selMatrixIRS2(chn) := MatrixMask( ageMatrixPostR, maskCondSelIRS2(chn) )

    assert(
      selMatrixIRS1(chn).forall( (x: Vec[Bool]) => x.forall{(y: Bool) => (y === true.B)} ) |
      PopCount( selMatrixIRS1(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
    )

    assert(
      selMatrixIRS2(chn).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B)} ) |
      PopCount( selMatrixIRS2(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
    )
  }



  val isIRS1NoneReq = Wire( Vec(opChn, Bool()) )
  val isIRS2NoneReq = Wire( Vec(opChn, Bool()) )



  val selIRS1 = Wire( Vec(opChn, UInt((log2Ceil(xRegNum)).W) ) )
  val selIRS2 = Wire( Vec(opChn, UInt((log2Ceil(xRegNum)).W) ) )

  for( chn <- 0 until opChn ){
    isIRS1NoneReq(chn) := selMatrixIRS1(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
    isIRS2NoneReq(chn) := selMatrixIRS2(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture

    selIRS1(chn) :=
      Mux1H(
        ( 0 until dptEntry ).map{ i => { (selMatrixIRS1(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(0) } } ++
        ( 0 until rnChn    ).map{ i => { (selMatrixIRS1(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs1 }}
      ) //index a row which all zero
    selIRS2(chn) :=
      Mux1H(
        ( 0 until dptEntry ).map{ i => { (selMatrixIRS2(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(1) } } ++
        ( 0 until rnChn    ).map{ i => { (selMatrixIRS2(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs2 }}
      ) //index a row which all zero

    if( chn % 2 == 0 ){
      rIOpNum(chn) := Mux( ~isIRS1NoneReq(chn), selIRS1(chn), Mux( ~isIRS2NoneReq(chn), selIRS2(chn), 0.U ))
    } else {
      rIOpNum(chn) := Mux( ~isIRS2NoneReq(chn), selIRS2(chn), Mux( ~isIRS1NoneReq(chn), selIRS1(chn), 0.U ))
    }

    io.irgReq(chn).valid := ~isIRS1NoneReq(chn) | ~isIRS2NoneReq(chn)
    io.irgReq(chn).bits  := rIOpNum(chn)
  }




}

trait DptReadFOp { this: DptAgeMatrix =>
  val rFOpNum = Wire( Vec( opChn, UInt((log2Ceil(fRegNum)).W)) )


  /** Whether this rs can request operator reading */
  val canFOpReq = Wire( Vec( dptEntry, Vec( 3, Bool() )))

  for( i <- 0 until dptEntry ) {
    canFOpReq(i)(0) := 
      io.frgLog(bufInfo(i).phy.rs1) === "b11".U & //reg-log is ready
      isBufFop(i)(0) & //is a fop req
      ~isOpReady(i)(0) & ~io.frgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(0))}}.reduce(_|_) //pending and non-rsp in same cycle

    canFOpReq(i)(1) :=
      io.frgLog(bufInfo(i).phy.rs2) === "b11".U & //reg-log is ready
      isBufFop(i)(1) & //is a fop req
      ~isOpReady(i)(1) & ~io.frgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(1))}}.reduce(_|_) //pending and non-rsp in same cycle

    canFOpReq(i)(2) :=
      io.frgLog(bufInfo(i).phy.rs3) === "b11".U & //reg-log is ready
      isBufFop(i)(2) & //is a fop req
      ~isOpReady(i)(2) & ~io.frgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(2))}}.reduce(_|_) //pending and non-rsp in same cycle
  }

  val canFOpPostReq = Wire( Vec( rnChn, Vec( 3, Bool() )) )

  for( i <- 0 until rnChn ) {
    canFOpPostReq(i)(0) := 
      // io.dptReq(i).fire &
      io.frgLog(io.dptReq(i).bits.phy.rs1) === "b11".U & //reg-log is ready
      io.dptReq(i).bits.phy.rs1 =/= 0.U &
      io.dptReq(i).bits.isFS1
    
    canFOpPostReq(i)(1) :=
      // io.dptReq(i).fire &
      io.frgLog(io.dptReq(i).bits.phy.rs2) === "b11".U & //reg-log is ready
      io.dptReq(i).bits.phy.rs2 =/= 0.U &
      io.dptReq(i).bits.isFS2

    canFOpPostReq(i)(2) :=
      // io.dptReq(i).fire &
      io.frgLog(io.dptReq(i).bits.phy.rs3) === "b11".U & //reg-log is ready
      io.dptReq(i).bits.phy.rs3 =/= 0.U &
      io.dptReq(i).bits.isFS3
  }


  /** Who is the highest priority to read operator in each chn */
  val selMatrixFRS1   = Wire( Vec( opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
  val selMatrixFRS2   = Wire( Vec( opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
  val selMatrixFRS3   = Wire( Vec( opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
  val maskCondSelFRS1 = Wire( Vec( opChn, Vec( dptEntry+rnChn, Bool() ) ) )
  val maskCondSelFRS2 = Wire( Vec( opChn, Vec( dptEntry+rnChn, Bool() ) ) )
  val maskCondSelFRS3 = Wire( Vec( opChn, Vec( dptEntry+rnChn, Bool() ) ) )



  for( chn <- 0 until opChn ){
    if( chn == 0 ){
      for ( i <- 0 until dptEntry ){
        maskCondSelFRS1(chn)(i) := ~bufValid(i) | ~canFOpReq(i)(0)
        maskCondSelFRS2(chn)(i) := ~bufValid(i) | ~canFOpReq(i)(1)
        maskCondSelFRS3(chn)(i) := ~bufValid(i) | ~canFOpReq(i)(2)
      }
      for ( i <- 0 until rnChn ){
        maskCondSelFRS1(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canFOpPostReq(i)(0)
        maskCondSelFRS2(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canFOpPostReq(i)(1)
        maskCondSelFRS3(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canFOpPostReq(i)(2)
      }
    } else {
      for ( i <- 0 until dptEntry ){
        maskCondSelFRS1(chn)(i) := maskCondSelFRS1(chn-1)(i) | (bufReqNum(i)(0) === rFOpNum(chn-1))
        maskCondSelFRS2(chn)(i) := maskCondSelFRS2(chn-1)(i) | (bufReqNum(i)(1) === rFOpNum(chn-1))
        maskCondSelFRS3(chn)(i) := maskCondSelFRS3(chn-1)(i) | (bufReqNum(i)(2) === rFOpNum(chn-1))
      }
      for( i <- 0 until rnChn ){
        maskCondSelFRS1(chn)(dptEntry+i) := maskCondSelFRS1(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs1 === rFOpNum(chn-1))
        maskCondSelFRS2(chn)(dptEntry+i) := maskCondSelFRS2(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs2 === rFOpNum(chn-1))        
        maskCondSelFRS3(chn)(dptEntry+i) := maskCondSelFRS3(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs3 === rFOpNum(chn-1))        
      }
    }
    selMatrixFRS1(chn) := MatrixMask( ageMatrixPostR, maskCondSelFRS1(chn) )
    selMatrixFRS2(chn) := MatrixMask( ageMatrixPostR, maskCondSelFRS2(chn) )
    selMatrixFRS3(chn) := MatrixMask( ageMatrixPostR, maskCondSelFRS3(chn) )

    assert(
      selMatrixFRS1(chn).forall( (x: Vec[Bool]) => x.forall{(y: Bool) => (y === true.B)} ) |
      PopCount( selMatrixFRS1(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
    )

    assert(
      selMatrixFRS2(chn).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B)} ) |
      PopCount( selMatrixFRS2(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
    )

    assert(
      selMatrixFRS3(chn).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B)} ) |
      PopCount( selMatrixFRS3(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
    )
  }



  val isFRS1NoneReq = Wire( Vec(opChn, Bool()) )
  val isFRS2NoneReq = Wire( Vec(opChn, Bool()) )
  val isFRS3NoneReq = Wire( Vec(opChn, Bool()) )
  // dontTouch(isFRS1NoneReq)
  // dontTouch(isFRS2NoneReq)
  val selFRS1 = Wire( Vec(opChn,  UInt((log2Ceil(fRegNum)).W) ) )
  val selFRS2 = Wire( Vec(opChn,  UInt((log2Ceil(fRegNum)).W) ) )
  val selFRS3 = Wire( Vec(opChn,  UInt((log2Ceil(fRegNum)).W) ) )

  for( chn <- 0 until opChn ){
    isFRS1NoneReq(chn) := selMatrixFRS1(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
    isFRS2NoneReq(chn) := selMatrixFRS2(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
    isFRS3NoneReq(chn) := selMatrixFRS3(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture

    selFRS1(chn) :=
      Mux1H(
        ( 0 until dptEntry ).map{ i => { (selMatrixFRS1(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(0) } } ++
        ( 0 until rnChn    ).map{ i => { (selMatrixFRS1(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs1 }}
      ) //index a row which all zero
    selFRS2(chn) :=
      Mux1H(
        ( 0 until dptEntry ).map{ i => { (selMatrixFRS2(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(1) } } ++
        ( 0 until rnChn    ).map{ i => { (selMatrixFRS2(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs2 }}

      ) //index a row which all zero
    selFRS3(chn) :=
      Mux1H(
        ( 0 until dptEntry ).map{ i => { (selMatrixFRS3(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(2) } } ++
        ( 0 until rnChn    ).map{ i => { (selMatrixFRS3(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs3 }}
      ) //index a row which all zero

    if( chn % 3 == 0 ){
      rFOpNum(chn) := Mux( ~isFRS1NoneReq(chn), selFRS1(chn), Mux( ~isFRS2NoneReq(chn), selFRS2(chn), Mux( ~isFRS3NoneReq(chn), selFRS3(chn), 0.U) ))
    } else if( chn % 3 == 1 ){
      rFOpNum(chn) := Mux( ~isFRS2NoneReq(chn), selFRS2(chn), Mux( ~isFRS3NoneReq(chn), selFRS3(chn), Mux( ~isFRS1NoneReq(chn), selFRS1(chn), 0.U) ))
    } else {
      rFOpNum(chn) := Mux( ~isFRS3NoneReq(chn), selFRS3(chn), Mux( ~isFRS1NoneReq(chn), selFRS1(chn), Mux( ~isFRS2NoneReq(chn), selFRS2(chn), 0.U) ))
    }


    io.frgReq(chn).valid := ~isFRS1NoneReq(chn) | ~isFRS2NoneReq(chn) | ~isFRS3NoneReq(chn)
    io.frgReq(chn).bits  := rFOpNum(chn)
  }


}

trait DptReadVOp { this: DptAgeMatrix =>
  val rVOpNum = Wire( Vec( vParams.opChn, UInt((log2Ceil(vRegNum)).W)) )


  /** Whether this vs can request operator reading */
  val canVOpReq = Wire( Vec( dptEntry, Vec( 5, Bool() )))

  for( i <- 0 until dptEntry ) {
    canVOpReq(i)(0) := 
      io.vrgLog(bufInfo(i).phy.rs1) === "b11".U & //reg-log is ready
      isBufVop(i)(0) & //is a vop req
      ~isOpReady(i)(0) & ~io.vrgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(0))}}.reduce(_|_) //pending and non-rsp in same cycle

    canVOpReq(i)(1) :=
      io.vrgLog(bufInfo(i).phy.rs2) === "b11".U & //reg-log is ready
      isBufVop(i)(1) & //is a vop req
      ~isOpReady(i)(1) & ~io.vrgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(1))}}.reduce(_|_) //pending and non-rsp in same cycle

    canVOpReq(i)(2) :=
      io.vrgLog(bufInfo(i).phy.rs3) === "b11".U & //reg-log is ready
      isBufVop(i)(2) & //is a vop req
      ~isOpReady(i)(2) & ~io.vrgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(2))}}.reduce(_|_) //pending and non-rsp in same cycle
    
    canVOpReq(i)(3) :=
      io.vrgLog(bufInfo(i).phy.rs4) === "b11".U & //reg-log is ready
      isBufVop(i)(3) & //is a vop req
      ~isOpReady(i)(3) & ~io.vrgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(3))}}.reduce(_|_) //pending and non-rsp in same cycle

    canVOpReq(i)(4) :=
      io.vrgLog(bufInfo(i).phy.rs5) === "b11".U & //reg-log is ready
      isBufVop(i)(4) & //is a vop req
      ~isOpReady(i)(4) & ~io.vrgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(4))}}.reduce(_|_) //pending and non-rsp in same cycle

  }

  val canVOpPostReq = Wire( Vec( rnChn, Vec( 5, Bool() )) )

  for( i <- 0 until rnChn ) {
    canVOpPostReq(i)(0) := 
      // io.dptReq(i).fire &
      io.vrgLog(io.dptReq(i).bits.phy.rs1) === "b11".U & //reg-log is ready
      io.dptReq(i).bits.phy.rs1 =/= 0.U &
      io.dptReq(i).bits.isVS1
    
    canVOpPostReq(i)(1) :=
      // io.dptReq(i).fire &
      io.vrgLog(io.dptReq(i).bits.phy.rs2) === "b11".U & //reg-log is ready
      io.dptReq(i).bits.phy.rs2 =/= 0.U &
      io.dptReq(i).bits.isVS2

    canVOpPostReq(i)(2) :=
      // io.dptReq(i).fire &
      io.vrgLog(io.dptReq(i).bits.phy.rs3) === "b11".U & //reg-log is ready
      io.dptReq(i).bits.phy.rs3 =/= 0.U &
      io.dptReq(i).bits.isVS3

    canVOpPostReq(i)(3) :=
      // io.dptReq(i).fire &
      io.vrgLog(io.dptReq(i).bits.phy.rs4) === "b11".U & //reg-log is ready
      io.dptReq(i).bits.phy.rs4 =/= 0.U &
      io.dptReq(i).bits.isVS4

    canVOpPostReq(i)(4) :=
      // io.dptReq(i).fire &
      io.vrgLog(io.dptReq(i).bits.phy.rs5) === "b11".U & //reg-log is ready
      io.dptReq(i).bits.phy.rs5 =/= 0.U &
      io.dptReq(i).bits.isVMS
  }


  /** Who is the highest priority to read operator in each chn */
  val selMatrixVRS1   = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
  val selMatrixVRS2   = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
  val selMatrixVRS3   = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
  val selMatrixVRS4   = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
  val selMatrixVRS5   = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
  val maskCondSelVRS1 = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Bool() ) ) )
  val maskCondSelVRS2 = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Bool() ) ) )
  val maskCondSelVRS3 = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Bool() ) ) )
  val maskCondSelVRS4 = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Bool() ) ) )
  val maskCondSelVRS5 = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Bool() ) ) )



  for( chn <- 0 until vParams.opChn ){
    if( chn == 0 ){
      for ( i <- 0 until dptEntry ){
        maskCondSelVRS1(chn)(i) := ~bufValid(i) | ~canVOpReq(i)(0)
        maskCondSelVRS2(chn)(i) := ~bufValid(i) | ~canVOpReq(i)(1)
        maskCondSelVRS3(chn)(i) := ~bufValid(i) | ~canVOpReq(i)(2)
        maskCondSelVRS4(chn)(i) := ~bufValid(i) | ~canVOpReq(i)(3)
        maskCondSelVRS5(chn)(i) := ~bufValid(i) | ~canVOpReq(i)(4)
      }
      for ( i <- 0 until rnChn ){
        maskCondSelVRS1(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canVOpPostReq(i)(0)
        maskCondSelVRS2(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canVOpPostReq(i)(1)
        maskCondSelVRS3(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canVOpPostReq(i)(2)
        maskCondSelVRS4(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canVOpPostReq(i)(3)
        maskCondSelVRS5(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canVOpPostReq(i)(4)
      }
    } else {
      for ( i <- 0 until dptEntry ){
        maskCondSelVRS1(chn)(i) := maskCondSelVRS1(chn-1)(i) | (bufReqNum(i)(0) === rVOpNum(chn-1))
        maskCondSelVRS2(chn)(i) := maskCondSelVRS2(chn-1)(i) | (bufReqNum(i)(1) === rVOpNum(chn-1))
        maskCondSelVRS3(chn)(i) := maskCondSelVRS3(chn-1)(i) | (bufReqNum(i)(2) === rVOpNum(chn-1))
        maskCondSelVRS4(chn)(i) := maskCondSelVRS4(chn-1)(i) | (bufReqNum(i)(3) === rVOpNum(chn-1))
        maskCondSelVRS5(chn)(i) := maskCondSelVRS5(chn-1)(i) | (bufReqNum(i)(4) === rVOpNum(chn-1))
      }
      for( i <- 0 until rnChn ){
        maskCondSelVRS1(chn)(dptEntry+i) := maskCondSelVRS1(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs1 === rVOpNum(chn-1))
        maskCondSelVRS2(chn)(dptEntry+i) := maskCondSelVRS2(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs2 === rVOpNum(chn-1))
        maskCondSelVRS3(chn)(dptEntry+i) := maskCondSelVRS3(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs3 === rVOpNum(chn-1))
        maskCondSelVRS4(chn)(dptEntry+i) := maskCondSelVRS4(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs4 === rVOpNum(chn-1))
        maskCondSelVRS5(chn)(dptEntry+i) := maskCondSelVRS5(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs5 === rVOpNum(chn-1))
      }
    }
    selMatrixVRS1(chn) := MatrixMask( ageMatrixPostR, maskCondSelVRS1(chn) )
    selMatrixVRS2(chn) := MatrixMask( ageMatrixPostR, maskCondSelVRS2(chn) )
    selMatrixVRS3(chn) := MatrixMask( ageMatrixPostR, maskCondSelVRS3(chn) )
    selMatrixVRS4(chn) := MatrixMask( ageMatrixPostR, maskCondSelVRS4(chn) )
    selMatrixVRS5(chn) := MatrixMask( ageMatrixPostR, maskCondSelVRS5(chn) )

    assert(
      selMatrixVRS1(chn).forall( (x: Vec[Bool]) => x.forall{(y: Bool) => (y === true.B)} ) |
      PopCount( selMatrixVRS1(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
    )

    assert(
      selMatrixVRS2(chn).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B)} ) |
      PopCount( selMatrixVRS2(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
    )

    assert(
      selMatrixVRS3(chn).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B)} ) |
      PopCount( selMatrixVRS3(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
    )

    assert(
      selMatrixVRS4(chn).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B)} ) |
      PopCount( selMatrixVRS4(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
    )

    assert(
      selMatrixVRS5(chn).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B)} ) |
      PopCount( selMatrixVRS5(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
    )
  }


  val isVRS1NoneReq = Wire( Vec( vParams.opChn, Bool()) )
  val isVRS2NoneReq = Wire( Vec( vParams.opChn, Bool()) )
  val isVRS3NoneReq = Wire( Vec( vParams.opChn, Bool()) )
  val isVRS4NoneReq = Wire( Vec( vParams.opChn, Bool()) )
  val isVRS5NoneReq = Wire( Vec( vParams.opChn, Bool()) )

  val selVRS1 = Wire( Vec(vParams.opChn,  UInt((log2Ceil(vRegNum)).W) ) )
  val selVRS2 = Wire( Vec(vParams.opChn,  UInt((log2Ceil(vRegNum)).W) ) )
  val selVRS3 = Wire( Vec(vParams.opChn,  UInt((log2Ceil(vRegNum)).W) ) )
  val selVRS4 = Wire( Vec(vParams.opChn,  UInt((log2Ceil(vRegNum)).W) ) )
  val selVRS5 = Wire( Vec(vParams.opChn,  UInt((log2Ceil(vRegNum)).W) ) )

  for( chn <- 0 until vParams.opChn ){
    isVRS1NoneReq(chn) := selMatrixVRS1(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
    isVRS2NoneReq(chn) := selMatrixVRS2(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
    isVRS3NoneReq(chn) := selMatrixVRS3(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
    isVRS4NoneReq(chn) := selMatrixVRS4(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
    isVRS5NoneReq(chn) := selMatrixVRS5(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture

    selVRS1(chn) :=
      Mux1H(
        ( 0 until dptEntry ).map{ i => { (selMatrixVRS1(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(0) } } ++
        ( 0 until rnChn    ).map{ i => { (selMatrixVRS1(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs1 }}
      ) //index a row which all zero
    selVRS2(chn) :=
      Mux1H(
        ( 0 until dptEntry ).map{ i => { (selMatrixVRS2(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(1) } } ++
        ( 0 until rnChn    ).map{ i => { (selMatrixVRS2(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs2 }}
      ) //index a row which all zero
    selVRS3(chn) :=
      Mux1H(
        ( 0 until dptEntry ).map{ i => { (selMatrixVRS3(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(2) } } ++
        ( 0 until rnChn    ).map{ i => { (selMatrixVRS3(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs3 }}
      ) //index a row which all zero
    selVRS4(chn) :=
      Mux1H(
        ( 0 until dptEntry ).map{ i => { (selMatrixVRS4(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(3) } } ++
        ( 0 until rnChn    ).map{ i => { (selMatrixVRS4(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs4 }}
      ) //index a row which all zero
    selVRS5(chn) :=
      Mux1H(
        ( 0 until dptEntry ).map{ i => { (selMatrixVRS5(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(3) } } ++
        ( 0 until rnChn    ).map{ i => { (selMatrixVRS5(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs5 }}
      ) //index a row which all zero

    if( chn % 5 == 0 ){
      rVOpNum(chn) := Mux( ~isVRS1NoneReq(chn), selVRS1(chn), Mux( ~isVRS2NoneReq(chn), selVRS2(chn), Mux( ~isVRS3NoneReq(chn), selVRS3(chn), Mux( ~isVRS4NoneReq(chn), selVRS4(chn), Mux( ~isVRS5NoneReq(chn), selVRS5(chn), 0.U) )) ) )
    } else if( chn % 5 == 1 ){
      rVOpNum(chn) := Mux( ~isVRS2NoneReq(chn), selVRS2(chn), Mux( ~isVRS3NoneReq(chn), selVRS3(chn), Mux( ~isVRS4NoneReq(chn), selVRS4(chn), Mux( ~isVRS5NoneReq(chn), selVRS5(chn), Mux( ~isVRS1NoneReq(chn), selVRS1(chn), 0.U) )) ) )
    } else if( chn % 5 == 2 ){
      rVOpNum(chn) := Mux( ~isVRS3NoneReq(chn), selVRS3(chn), Mux( ~isVRS4NoneReq(chn), selVRS4(chn), Mux( ~isVRS5NoneReq(chn), selVRS5(chn), Mux( ~isVRS1NoneReq(chn), selVRS1(chn), Mux( ~isVRS2NoneReq(chn), selVRS2(chn), 0.U) )) ) )
    } else if( chn % 5 == 3 ){
      rVOpNum(chn) := Mux( ~isVRS4NoneReq(chn), selVRS4(chn), Mux( ~isVRS5NoneReq(chn), selVRS5(chn), Mux( ~isVRS1NoneReq(chn), selVRS1(chn), Mux( ~isVRS2NoneReq(chn), selVRS2(chn), Mux( ~isVRS3NoneReq(chn), selVRS3(chn), 0.U) )) ) )
    } else {
      rVOpNum(chn) := Mux( ~isVRS5NoneReq(chn), selVRS5(chn), Mux( ~isVRS1NoneReq(chn), selVRS1(chn), Mux( ~isVRS2NoneReq(chn), selVRS2(chn), Mux( ~isVRS3NoneReq(chn), selVRS3(chn), Mux( ~isVRS4NoneReq(chn), selVRS4(chn), 0.U) )) ) )
    }


    io.vrgReq(chn).valid := ~isVRS1NoneReq(chn) | ~isVRS2NoneReq(chn) | ~isVRS3NoneReq(chn) | ~isVRS4NoneReq(chn) | ~isVRS5NoneReq(chn)
    io.vrgReq(chn).bits  := rVOpNum(chn)
  }


}

abstract class IssueBase()(implicit p: Parameters) extends DptAgeMatrix with DptReadIOp with DptReadFOp with DptReadVOp




trait IssLoadIOp { this: IssueBase =>
  for( chn <- 0 until opChn ){
    when( io.irgRsp(chn).valid ){
      for( i <- 0 until dptEntry ){
        for( rs <- 0 until 2 ){
          when( bufValid(i) & (bufReqNum(i)(rs) === io.irgRsp(chn).bits.phy) & isBufXop(i)(rs) ){
            when( isOpReady(i)(rs) === true.B ) { /*printf(s"Warning, re-request op at chn $chn, Entry $i, rs( $rs )")*/ }
            isOpReady(i)(rs)   := true.B
            bufOperator(i)(rs) := io.irgRsp(chn).bits.op
          }
        }
      }
    }    
  }

}

trait IssLoadFOp { this: IssueBase =>
  for( chn <- 0 until opChn ){
    when( io.frgRsp(chn).valid ){
      for( i <- 0 until dptEntry ){
        for( rs <- 0 until 3 ){
          when( bufValid(i) & (bufReqNum(i)(rs) === io.frgRsp(chn).bits.phy) & isBufFop(i)(rs) ){
            when( isOpReady(i)(rs) === true.B ) { /*printf(s"Warning, re-request op at chn $chn, Entry $i, rs( $rs )")*/ }
            isOpReady(i)(rs)   := true.B
            bufOperator(i)(rs) := io.frgRsp(chn).bits.op
          }
        }
      }
    }    
  }
}

trait IssLoadVOp { this: IssueBase =>
  for( chn <- 0 until vParams.opChn ){
    when( io.vrgRsp(chn).valid ){
      for( i <- 0 until dptEntry ){
        for( rs <- 0 until 5 ){
          when( bufValid(i) & (bufReqNum(i)(rs) === io.vrgRsp(chn).bits.phy) & isBufVop(i)(rs) ){
            when( isOpReady(i)(rs) === true.B ) { /*printf(s"Warning, re-request op at chn $chn, Entry $i, rs( $rs )")*/ }
            isOpReady(i)(rs)   := true.B
            bufOperator(i)(rs) := io.vrgRsp(chn).bits.op
          }
        }
      }
    }    
  }
}

trait IssLoadCsr { this: IssueBase =>

  val isCSRRReady = 
    for( i <- 0 until dptEntry ) yield {
      val csrSel = bufInfo(i).param.imm(11,0)
      val phySel = bufInfo(i).csrr( log2Ceil(4)-1, 0 )

      ((csrSel === "hf11".U) & ( io.csrIsReady.mvendorid(phySel)      === true.B)) |
      ((csrSel === "hf12".U) & ( io.csrIsReady.marchid(  phySel)        === true.B)) |
      ((csrSel === "hf13".U) & ( io.csrIsReady.mimpid(   phySel)         === true.B)) |
      ((csrSel === "hf14".U) & ( io.csrIsReady.mhartid(  phySel)        === true.B)) |
      ((csrSel === "h300".U) & ( io.csrIsReady.mstatus.asUInt(phySel) === true.B)) |
      ((csrSel === "h301".U) & ( io.csrIsReady.misa(   phySel)           === true.B)) |
      ((csrSel === "h302".U) & ( io.csrIsReady.medeleg(phySel)        === true.B)) |
      ((csrSel === "h303".U) & ( io.csrIsReady.mideleg(phySel)        === true.B)) |
      ((csrSel === "h304".U) & ( io.csrIsReady.mie(    phySel)            === true.B)) |
      ((csrSel === "h305".U) & ( io.csrIsReady.mtvec(  phySel)          === true.B)) |
      ((csrSel === "h306".U) & ( io.csrIsReady.mcounteren(phySel)     === true.B)) |
      ((csrSel === "h340".U) & ( io.csrIsReady.mscratch(phySel)       === true.B)) |
      ((csrSel === "h341".U) & ( io.csrIsReady.mepc(phySel)           === true.B)) |
      ((csrSel === "h342".U) & ( io.csrIsReady.mcause(phySel)         === true.B)) |
      ((csrSel === "h343".U) & ( io.csrIsReady.mtval(phySel)          === true.B)) |
      ((csrSel === "h344".U) & ( io.csrIsReady.mip(phySel)            === true.B)) |
      ((csrSel === "h34A".U) & ( io.csrIsReady.mtinst(  phySel)         === true.B)) |
      ((csrSel === "h34B".U) & ( io.csrIsReady.mtval2(  phySel)         === true.B)) |
      ((csrSel === "hB00".U) & ( io.csrIsReady.mcycle(  phySel)         === true.B)) |
      ((csrSel === "hB02".U) & ( io.csrIsReady.minstret(phySel)       === true.B)) |
      ((csrSel === "h100".U) & ( io.csrIsReady.sstatus(phySel)        === true.B)) |
      // ((csrSel === "h102".U) & ( io.csrIsReady.sedeleg(phySel)        === true.B)) |
      // ((csrSel === "h103".U) & ( io.csrIsReady.sideleg(phySel)        === true.B)) |
      ((csrSel === "h104".U) & ( io.csrIsReady.sie(       phySel)            === true.B)) |
      ((csrSel === "h105".U) & ( io.csrIsReady.stvec(     phySel)          === true.B)) |
      ((csrSel === "h106".U) & ( io.csrIsReady.scounteren(phySel)     === true.B)) |
      ((csrSel === "h140".U) & ( io.csrIsReady.sscratch(  phySel)       === true.B)) |
      ((csrSel === "h141".U) & ( io.csrIsReady.sepc(      phySel)           === true.B)) |
      ((csrSel === "h142".U) & ( io.csrIsReady.scause(    phySel)         === true.B)) |
      ((csrSel === "h143".U) & ( io.csrIsReady.stval(     phySel)          === true.B)) |
      ((csrSel === "h144".U) & ( io.csrIsReady.sip(       phySel)            === true.B)) |
      ((csrSel === "h180".U) & ( io.csrIsReady.satp(      phySel)           === true.B)) |
      ((csrSel === "h7A0".U) & ( io.csrIsReady.tselect(   phySel)        === true.B)) |
      ((csrSel === "h7A1".U) & ( io.csrIsReady.tdata1(    phySel)         === true.B)) |
      ((csrSel === "h7A2".U) & ( io.csrIsReady.tdata2(    phySel)         === true.B)) |
      ((csrSel === "h7A3".U) & ( io.csrIsReady.tdata3(    phySel)         === true.B)) |
      ((csrSel === "h7B0".U) & ( io.csrIsReady.dcsr(      phySel)           === true.B)) |
      ((csrSel === "h7B1".U) & ( io.csrIsReady.dpc(       phySel)            === true.B)) |
      ((csrSel === "h7B2".U) & ( io.csrIsReady.dscratch0( phySel)      === true.B)) |
      ((csrSel === "h7B3".U) & ( io.csrIsReady.dscratch1( phySel)      === true.B)) |
      ((csrSel === "h7B4".U) & ( io.csrIsReady.dscratch2( phySel)      === true.B)) |
      ((csrSel === "h001".U) & ( io.csrIsReady.fflags(    phySel)         === true.B)) |
      ((csrSel === "h002".U) & ( io.csrIsReady.frm(       phySel)            === true.B)) |
      ((csrSel === "h003".U) & ( io.csrIsReady.fcsr(      phySel)           === true.B)) |
      ((csrSel === "h320".U) & ( io.csrIsReady.mcountinhibit(phySel)  === true.B)) |
      ((csrSel === "h3A0".U) & ( io.csrIsReady.pmpcfg0(   phySel)        === true.B)) |
      ((csrSel === "h3A2".U) & ( io.csrIsReady.pmpcfg2(   phySel)        === true.B)) |
      ((csrSel === "h3A4".U) & ( io.csrIsReady.pmpcfg4(   phySel)        === true.B)) |
      ((csrSel === "h3A6".U) & ( io.csrIsReady.pmpcfg6(   phySel)        === true.B)) |
      ((csrSel === "h3A8".U) & ( io.csrIsReady.pmpcfg8(   phySel)        === true.B)) |
      ((csrSel === "h3AA".U) & ( io.csrIsReady.pmpcfg10(  phySel)       === true.B)) |
      ((csrSel === "h3AC".U) & ( io.csrIsReady.pmpcfg12(  phySel)       === true.B)) |
      ((csrSel === "h3AE".U) & ( io.csrIsReady.pmpcfg14(  phySel)       === true.B)) |
      ((csrSel === "h3B0".U) & (io.csrIsReady.pmpaddr0(   phySel)        === true.B)) |
      ((csrSel === "h3B1".U) & (io.csrIsReady.pmpaddr1(   phySel)        === true.B)) |
      ((csrSel === "h3B2".U) & (io.csrIsReady.pmpaddr2(   phySel)        === true.B)) |
      ((csrSel === "h3B3".U) & (io.csrIsReady.pmpaddr3(   phySel)        === true.B)) |
      ((csrSel === "h3B4".U) & (io.csrIsReady.pmpaddr4(   phySel)        === true.B)) |
      ((csrSel === "h3B5".U) & (io.csrIsReady.pmpaddr5(   phySel)        === true.B)) |
      ((csrSel === "h3B6".U) & (io.csrIsReady.pmpaddr6(   phySel)        === true.B)) |
      ((csrSel === "h3B7".U) & (io.csrIsReady.pmpaddr7(   phySel)        === true.B)) |
      ((csrSel === "h3B8".U) & (io.csrIsReady.pmpaddr8(   phySel)        === true.B)) |
      ((csrSel === "h3B9".U) & (io.csrIsReady.pmpaddr9(   phySel)        === true.B)) |
      ((csrSel === "h3BA".U) & (io.csrIsReady.pmpaddr10(  phySel)       === true.B)) |
      ((csrSel === "h3BB".U) & (io.csrIsReady.pmpaddr11(  phySel)       === true.B)) |
      ((csrSel === "h3BC".U) & (io.csrIsReady.pmpaddr12(  phySel)       === true.B)) |
      ((csrSel === "h3BD".U) & (io.csrIsReady.pmpaddr13(  phySel)       === true.B)) |
      ((csrSel === "h3BE".U) & (io.csrIsReady.pmpaddr14(  phySel)       === true.B)) |
      ((csrSel === "h3BF".U) & (io.csrIsReady.pmpaddr15(  phySel)       === true.B)) |
      ((csrSel === "h3C0".U) & (io.csrIsReady.pmpaddr16(  phySel)       === true.B)) |
      ((csrSel === "h3C1".U) & (io.csrIsReady.pmpaddr17(  phySel)       === true.B)) |
      ((csrSel === "h3C2".U) & (io.csrIsReady.pmpaddr18(  phySel)       === true.B)) |
      ((csrSel === "h3C3".U) & (io.csrIsReady.pmpaddr19(  phySel)       === true.B)) |
      ((csrSel === "h3C4".U) & (io.csrIsReady.pmpaddr20(  phySel)       === true.B)) |
      ((csrSel === "h3C5".U) & (io.csrIsReady.pmpaddr21(  phySel)       === true.B)) |
      ((csrSel === "h3C6".U) & (io.csrIsReady.pmpaddr22(  phySel)       === true.B)) |
      ((csrSel === "h3C7".U) & (io.csrIsReady.pmpaddr23(  phySel)       === true.B)) |
      ((csrSel === "h3C8".U) & (io.csrIsReady.pmpaddr24(  phySel)       === true.B)) |
      ((csrSel === "h3C9".U) & (io.csrIsReady.pmpaddr25(  phySel)       === true.B)) |
      ((csrSel === "h3CA".U) & (io.csrIsReady.pmpaddr26(  phySel)       === true.B)) |
      ((csrSel === "h3CB".U) & (io.csrIsReady.pmpaddr27(  phySel)       === true.B)) |
      ((csrSel === "h3CC".U) & (io.csrIsReady.pmpaddr28(  phySel)       === true.B)) |
      ((csrSel === "h3CD".U) & (io.csrIsReady.pmpaddr29(  phySel)       === true.B)) |
      ((csrSel === "h3CE".U) & (io.csrIsReady.pmpaddr30(  phySel)       === true.B)) |
      ((csrSel === "h3CF".U) & (io.csrIsReady.pmpaddr31(  phySel)       === true.B)) |
      ((csrSel === "h3D0".U) & (io.csrIsReady.pmpaddr32(  phySel)       === true.B)) |
      ((csrSel === "h3D1".U) & (io.csrIsReady.pmpaddr33(  phySel)       === true.B)) |
      ((csrSel === "h3D2".U) & (io.csrIsReady.pmpaddr34(  phySel)       === true.B)) |
      ((csrSel === "h3D3".U) & (io.csrIsReady.pmpaddr35(  phySel)       === true.B)) |
      ((csrSel === "h3D4".U) & (io.csrIsReady.pmpaddr36(  phySel)       === true.B)) |
      ((csrSel === "h3D5".U) & (io.csrIsReady.pmpaddr37(  phySel)       === true.B)) |
      ((csrSel === "h3D6".U) & (io.csrIsReady.pmpaddr38(  phySel)       === true.B)) |
      ((csrSel === "h3D7".U) & (io.csrIsReady.pmpaddr39(  phySel)       === true.B)) |
      ((csrSel === "h3D8".U) & (io.csrIsReady.pmpaddr40(  phySel)       === true.B)) |
      ((csrSel === "h3D9".U) & (io.csrIsReady.pmpaddr41(  phySel)       === true.B)) |
      ((csrSel === "h3DA".U) & (io.csrIsReady.pmpaddr42(  phySel)       === true.B)) |
      ((csrSel === "h3DB".U) & (io.csrIsReady.pmpaddr43(  phySel)       === true.B)) |
      ((csrSel === "h3DC".U) & (io.csrIsReady.pmpaddr44(  phySel)       === true.B)) |
      ((csrSel === "h3DD".U) & (io.csrIsReady.pmpaddr45(  phySel)       === true.B)) |
      ((csrSel === "h3DE".U) & (io.csrIsReady.pmpaddr46(  phySel)       === true.B)) |
      ((csrSel === "h3DF".U) & (io.csrIsReady.pmpaddr47(  phySel)       === true.B)) |
      ((csrSel === "h3E0".U) & (io.csrIsReady.pmpaddr48(  phySel)       === true.B)) |
      ((csrSel === "h3E1".U) & (io.csrIsReady.pmpaddr49(  phySel)       === true.B)) |
      ((csrSel === "h3E2".U) & (io.csrIsReady.pmpaddr50(  phySel)       === true.B)) |
      ((csrSel === "h3E3".U) & (io.csrIsReady.pmpaddr51(  phySel)       === true.B)) |
      ((csrSel === "h3E4".U) & (io.csrIsReady.pmpaddr52(  phySel)       === true.B)) |
      ((csrSel === "h3E5".U) & (io.csrIsReady.pmpaddr53(  phySel)       === true.B)) |
      ((csrSel === "h3E6".U) & (io.csrIsReady.pmpaddr54(  phySel)       === true.B)) |
      ((csrSel === "h3E7".U) & (io.csrIsReady.pmpaddr55(  phySel)       === true.B)) |
      ((csrSel === "h3E8".U) & (io.csrIsReady.pmpaddr56(  phySel)       === true.B)) |
      ((csrSel === "h3E9".U) & (io.csrIsReady.pmpaddr57(  phySel)       === true.B)) |
      ((csrSel === "h3EA".U) & (io.csrIsReady.pmpaddr58(  phySel)       === true.B)) |
      ((csrSel === "h3EB".U) & (io.csrIsReady.pmpaddr59(  phySel)       === true.B)) |
      ((csrSel === "h3EC".U) & (io.csrIsReady.pmpaddr60(  phySel)       === true.B)) |
      ((csrSel === "h3ED".U) & (io.csrIsReady.pmpaddr61(  phySel)       === true.B)) |
      ((csrSel === "h3EE".U) & (io.csrIsReady.pmpaddr62(  phySel)       === true.B)) |
      ((csrSel === "h3EF".U) & (io.csrIsReady.pmpaddr63(  phySel)       === true.B)) |
      ((csrSel === "hB03".U) & (io.csrIsReady.mhpmcounter3(phySel)    === true.B)) |
      ((csrSel === "hB04".U) & (io.csrIsReady.mhpmcounter4(phySel)    === true.B)) |
      ((csrSel === "hB05".U) & (io.csrIsReady.mhpmcounter5(phySel)    === true.B)) |
      ((csrSel === "hB06".U) & (io.csrIsReady.mhpmcounter6(phySel)    === true.B)) |
      ((csrSel === "hB07".U) & (io.csrIsReady.mhpmcounter7(phySel)    === true.B)) |
      ((csrSel === "hB08".U) & (io.csrIsReady.mhpmcounter8(phySel)    === true.B)) |
      ((csrSel === "hB09".U) & (io.csrIsReady.mhpmcounter9(phySel)    === true.B)) |
      ((csrSel === "hB0A".U) & (io.csrIsReady.mhpmcounter10(phySel)   === true.B)) |
      ((csrSel === "hB0B".U) & (io.csrIsReady.mhpmcounter11(phySel)   === true.B)) |
      ((csrSel === "hB0C".U) & (io.csrIsReady.mhpmcounter12(phySel)   === true.B)) |
      ((csrSel === "hB0D".U) & (io.csrIsReady.mhpmcounter13(phySel)   === true.B)) |
      ((csrSel === "hB0E".U) & (io.csrIsReady.mhpmcounter14(phySel)   === true.B)) |
      ((csrSel === "hB0F".U) & (io.csrIsReady.mhpmcounter15(phySel)   === true.B)) |
      ((csrSel === "hB10".U) & (io.csrIsReady.mhpmcounter16(phySel)   === true.B)) |
      ((csrSel === "hB11".U) & (io.csrIsReady.mhpmcounter17(phySel)   === true.B)) |
      ((csrSel === "hB12".U) & (io.csrIsReady.mhpmcounter18(phySel)   === true.B)) |
      ((csrSel === "hB13".U) & (io.csrIsReady.mhpmcounter19(phySel)   === true.B)) |
      ((csrSel === "hB14".U) & (io.csrIsReady.mhpmcounter20(phySel)   === true.B)) |
      ((csrSel === "hB15".U) & (io.csrIsReady.mhpmcounter21(phySel)   === true.B)) |
      ((csrSel === "hB16".U) & (io.csrIsReady.mhpmcounter22(phySel)   === true.B)) |
      ((csrSel === "hB17".U) & (io.csrIsReady.mhpmcounter23(phySel)   === true.B)) |
      ((csrSel === "hB18".U) & (io.csrIsReady.mhpmcounter24(phySel)   === true.B)) |
      ((csrSel === "hB19".U) & (io.csrIsReady.mhpmcounter25(phySel)   === true.B)) |
      ((csrSel === "hB1A".U) & (io.csrIsReady.mhpmcounter26(phySel)   === true.B)) |
      ((csrSel === "hB1B".U) & (io.csrIsReady.mhpmcounter27(phySel)   === true.B)) |
      ((csrSel === "hB1C".U) & (io.csrIsReady.mhpmcounter28(phySel)   === true.B)) |
      ((csrSel === "hB1D".U) & (io.csrIsReady.mhpmcounter29(phySel)   === true.B)) |
      ((csrSel === "hB1E".U) & (io.csrIsReady.mhpmcounter30(phySel)   === true.B)) |
      ((csrSel === "hB1F".U) & (io.csrIsReady.mhpmcounter31(phySel)   === true.B)) |
      ((csrSel === "h323".U) & (io.csrIsReady.mhpmevent3(   phySel)    === true.B)) |
      ((csrSel === "h324".U) & (io.csrIsReady.mhpmevent4(   phySel)    === true.B)) |
      ((csrSel === "h325".U) & (io.csrIsReady.mhpmevent5(   phySel)    === true.B)) |
      ((csrSel === "h326".U) & (io.csrIsReady.mhpmevent6(   phySel)    === true.B)) |
      ((csrSel === "h327".U) & (io.csrIsReady.mhpmevent7(   phySel)    === true.B)) |
      ((csrSel === "h328".U) & (io.csrIsReady.mhpmevent8(   phySel)    === true.B)) |
      ((csrSel === "h329".U) & (io.csrIsReady.mhpmevent9(   phySel)    === true.B)) |
      ((csrSel === "h32A".U) & (io.csrIsReady.mhpmevent10(  phySel)   === true.B)) |
      ((csrSel === "h32B".U) & (io.csrIsReady.mhpmevent11(  phySel)   === true.B)) |
      ((csrSel === "h32C".U) & (io.csrIsReady.mhpmevent12(  phySel)   === true.B)) |
      ((csrSel === "h32D".U) & (io.csrIsReady.mhpmevent13(  phySel)   === true.B)) |
      ((csrSel === "h32E".U) & (io.csrIsReady.mhpmevent14(  phySel)   === true.B)) |
      ((csrSel === "h32F".U) & (io.csrIsReady.mhpmevent15(  phySel)   === true.B)) |
      ((csrSel === "h330".U) & (io.csrIsReady.mhpmevent16(  phySel)   === true.B)) |
      ((csrSel === "h331".U) & (io.csrIsReady.mhpmevent17(  phySel)   === true.B)) |
      ((csrSel === "h332".U) & (io.csrIsReady.mhpmevent18(  phySel)   === true.B)) |
      ((csrSel === "h333".U) & (io.csrIsReady.mhpmevent19(  phySel)   === true.B)) |
      ((csrSel === "h334".U) & (io.csrIsReady.mhpmevent20(  phySel)   === true.B)) |
      ((csrSel === "h335".U) & (io.csrIsReady.mhpmevent21(  phySel)   === true.B)) |
      ((csrSel === "h336".U) & (io.csrIsReady.mhpmevent22(  phySel)   === true.B)) |
      ((csrSel === "h337".U) & (io.csrIsReady.mhpmevent23(  phySel)   === true.B)) |
      ((csrSel === "h338".U) & (io.csrIsReady.mhpmevent24(  phySel)   === true.B)) |
      ((csrSel === "h339".U) & (io.csrIsReady.mhpmevent25(  phySel)   === true.B)) |
      ((csrSel === "h33A".U) & (io.csrIsReady.mhpmevent26(  phySel)   === true.B)) |
      ((csrSel === "h33B".U) & (io.csrIsReady.mhpmevent27(  phySel)   === true.B)) |
      ((csrSel === "h33C".U) & (io.csrIsReady.mhpmevent28(  phySel)   === true.B)) |
      ((csrSel === "h33D".U) & (io.csrIsReady.mhpmevent29(  phySel)   === true.B)) |
      ((csrSel === "h33E".U) & (io.csrIsReady.mhpmevent30(  phySel)   === true.B)) |
      ((csrSel === "h33F".U) & (io.csrIsReady.mhpmevent31(  phySel)   === true.B)) |
      ((csrSel === "hC00".U) & (io.csrIsReady.cycle(phySel)    === true.B)) |
      ((csrSel === "hC01".U) & (io.csrIsReady.time(phySel)    === true.B)) |
      ((csrSel === "hC02".U) & (io.csrIsReady.instret(phySel)    === true.B)) |
      ((csrSel === "hC03".U) & (io.csrIsReady.hpmcounter3(phySel)    === true.B)) |
      ((csrSel === "hC04".U) & (io.csrIsReady.hpmcounter4(phySel)    === true.B)) |
      ((csrSel === "hC05".U) & (io.csrIsReady.hpmcounter5(phySel)    === true.B)) |
      ((csrSel === "hC06".U) & (io.csrIsReady.hpmcounter6(phySel)    === true.B)) |
      ((csrSel === "hC07".U) & (io.csrIsReady.hpmcounter7(phySel)    === true.B)) |
      ((csrSel === "hC08".U) & (io.csrIsReady.hpmcounter8(phySel)    === true.B)) |
      ((csrSel === "hC09".U) & (io.csrIsReady.hpmcounter9(phySel)    === true.B)) |
      ((csrSel === "hC0A".U) & (io.csrIsReady.hpmcounter10(phySel)   === true.B)) |
      ((csrSel === "hC0B".U) & (io.csrIsReady.hpmcounter11(phySel)   === true.B)) |
      ((csrSel === "hC0C".U) & (io.csrIsReady.hpmcounter12(phySel)   === true.B)) |
      ((csrSel === "hC0D".U) & (io.csrIsReady.hpmcounter13(phySel)   === true.B)) |
      ((csrSel === "hC0E".U) & (io.csrIsReady.hpmcounter14(phySel)   === true.B)) |
      ((csrSel === "hC0F".U) & (io.csrIsReady.hpmcounter15(phySel)   === true.B)) |
      ((csrSel === "hC10".U) & (io.csrIsReady.hpmcounter16(phySel)   === true.B)) |
      ((csrSel === "hC11".U) & (io.csrIsReady.hpmcounter17(phySel)   === true.B)) |
      ((csrSel === "hC12".U) & (io.csrIsReady.hpmcounter18(phySel)   === true.B)) |
      ((csrSel === "hC13".U) & (io.csrIsReady.hpmcounter19(phySel)   === true.B)) |
      ((csrSel === "hC14".U) & (io.csrIsReady.hpmcounter20(phySel)   === true.B)) |
      ((csrSel === "hC15".U) & (io.csrIsReady.hpmcounter21(phySel)   === true.B)) |
      ((csrSel === "hC16".U) & (io.csrIsReady.hpmcounter22(phySel)   === true.B)) |
      ((csrSel === "hC17".U) & (io.csrIsReady.hpmcounter23(phySel)   === true.B)) |
      ((csrSel === "hC18".U) & (io.csrIsReady.hpmcounter24(phySel)   === true.B)) |
      ((csrSel === "hC19".U) & (io.csrIsReady.hpmcounter25(phySel)   === true.B)) |
      ((csrSel === "hC1A".U) & (io.csrIsReady.hpmcounter26(phySel)   === true.B)) |
      ((csrSel === "hC1B".U) & (io.csrIsReady.hpmcounter27(phySel)   === true.B)) |
      ((csrSel === "hC1C".U) & (io.csrIsReady.hpmcounter28(phySel)   === true.B)) |
      ((csrSel === "hC1D".U) & (io.csrIsReady.hpmcounter29(phySel)   === true.B)) |
      ((csrSel === "hC1E".U) & (io.csrIsReady.hpmcounter30(phySel)   === true.B)) |
      ((csrSel === "hC1F".U) & (io.csrIsReady.hpmcounter31(phySel)   === true.B))
    }

  val isFRMRReady =
    for( i <- 0 until dptEntry ) yield {
      val phySel = bufInfo(i).csrr( log2Ceil(4)-1, 0 )
      io.csrIsReady.frm(phySel) === true.B
    }
}


abstract class IssueSel()(implicit p: Parameters) extends IssueBase
with IssLoadIOp
with IssLoadFOp
with IssLoadVOp
with IssLoadCsr{
  val postIsOpReady = Wire( Vec( dptEntry, Vec(5, Bool())) )
  val postBufOperator = Wire( Vec( dptEntry, Vec(5, UInt(vParams.vlen.W))) )

  for( i <- 0 until dptEntry ){
    postIsOpReady(i)(0)   := MuxCase( isOpReady(i)(0),    io.irgRsp.map{x => { ( isBufXop(i)(0) & x.valid & (x.bits.phy === bufReqNum(i)(0))) -> true.B }} ++ io.frgRsp.map{x => { ( isBufFop(i)(0) & x.valid & (x.bits.phy === bufReqNum(i)(0))) -> true.B }} ++ io.vrgRsp.map{x => { ( isBufVop(i)(0) & x.valid & (x.bits.phy === bufReqNum(i)(0))) -> true.B }}    )
    postIsOpReady(i)(1)   := MuxCase( isOpReady(i)(1),    io.irgRsp.map{x => { ( isBufXop(i)(1) & x.valid & (x.bits.phy === bufReqNum(i)(1))) -> true.B }} ++ io.frgRsp.map{x => { ( isBufFop(i)(1) & x.valid & (x.bits.phy === bufReqNum(i)(1))) -> true.B }} ++ io.vrgRsp.map{x => { ( isBufVop(i)(1) & x.valid & (x.bits.phy === bufReqNum(i)(1))) -> true.B }}    )
    postIsOpReady(i)(2)   := MuxCase( isOpReady(i)(2),    io.frgRsp.map{x => { ( isBufFop(i)(2) & x.valid & (x.bits.phy === bufReqNum(i)(2))) -> true.B }} ++ io.vrgRsp.map{x => { ( isBufVop(i)(2) & x.valid & (x.bits.phy === bufReqNum(i)(2))) -> true.B }}    )
    postIsOpReady(i)(3)   := MuxCase( isOpReady(i)(3),    io.vrgRsp.map{x => { ( isBufVop(i)(3) & x.valid & (x.bits.phy === bufReqNum(i)(3))) -> true.B }})
    postIsOpReady(i)(4)   := MuxCase( isOpReady(i)(4),    io.vrgRsp.map{x => { ( isBufVop(i)(4) & x.valid & (x.bits.phy === bufReqNum(i)(4))) -> true.B }})

    postBufOperator(i)(0) := MuxCase( bufOperator(i)(0) , io.irgRsp.map{x => { ( isBufXop(i)(0) & x.valid & (x.bits.phy === bufReqNum(i)(0))) -> x.bits.op }} ++ io.frgRsp.map{x => { ( isBufFop(i)(0) & x.valid & (x.bits.phy === bufReqNum(i)(0))) -> x.bits.op }} ++ io.vrgRsp.map{x => { ( isBufVop(i)(0) & x.valid & (x.bits.phy === bufReqNum(i)(0))) -> x.bits.op }} )
    postBufOperator(i)(1) := MuxCase( bufOperator(i)(1) , io.irgRsp.map{x => { ( isBufXop(i)(1) & x.valid & (x.bits.phy === bufReqNum(i)(1))) -> x.bits.op }} ++ io.frgRsp.map{x => { ( isBufFop(i)(1) & x.valid & (x.bits.phy === bufReqNum(i)(1))) -> x.bits.op }} ++ io.vrgRsp.map{x => { ( isBufVop(i)(1) & x.valid & (x.bits.phy === bufReqNum(i)(1))) -> x.bits.op }}  )
    postBufOperator(i)(2) := MuxCase( bufOperator(i)(2) , io.frgRsp.map{x => { ( isBufFop(i)(2) & x.valid & (x.bits.phy === bufReqNum(i)(2))) -> x.bits.op }} ++ io.vrgRsp.map{x => { ( isBufVop(i)(2) & x.valid & (x.bits.phy === bufReqNum(i)(2))) -> x.bits.op }}  )
    postBufOperator(i)(3) := MuxCase( bufOperator(i)(3) , io.vrgRsp.map{x => { ( isBufVop(i)(3) & x.valid & (x.bits.phy === bufReqNum(i)(3))) -> x.bits.op }}  )
    postBufOperator(i)(4) := MuxCase( bufOperator(i)(4) , io.vrgRsp.map{x => { ( isBufVop(i)(4) & x.valid & (x.bits.phy === bufReqNum(i)(4))) -> x.bits.op }}  )
  

  }

}

trait IssSelAlu{ this: IssueSel =>


  def Pkg_alu_iss(idx: Int): Alu_iss_info = {

    val res = Wire(new Alu_iss_info)
    val src1 = postBufOperator(idx)(0)
    val src2 = postBufOperator(idx)(1)
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
    res.param.dat.op4 := 0.U
    res.param.dat.op5 := 0.U

    res.param.rd0 := bufInfo(idx).phy.rd0
    res.param.rd1 := 0.U
    return res
  }


  val aluIssIdx = Wire( Vec( aluNum, UInt((log2Ceil(dptEntry)).W) ) )
  val aluIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_alu_iss(i) }
  val aluIssFifo = for( i <- 0 until aluNum ) yield {
    Module(new Queue( new Alu_iss_info, ( if(!isMinArea) 4 else 1 ), flow = true ))
  }

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
        maskCondAluIss(iss)(i) := maskCondAluIss(iss-1)(i) | (i.U === aluIssIdx(iss-1))
      }
    }
    aluIssMatrix(iss) := MatrixMask( ageMatrixR, maskCondAluIss(iss) )
    
    assert(
      aluIssMatrix(iss).forall( (x: Vec[Bool]) => x.forall{(y: Bool) => (y === true.B)}  ) |
      PopCount( aluIssMatrix(iss).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
    )
  }
  
  for( iss <- 0 until aluNum ){
    aluIssFifo(iss).io.enq.valid := ~aluIssMatrix(iss).forall{ (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
    aluIssIdx(iss) := aluIssMatrix(iss).indexWhere( (x: Vec[Bool]) => x.forall( y => (y === false.B) ) ) //index a row which all zero
    aluIssFifo(iss).io.enq.bits  := 
      Mux1H( ( 0 until dptEntry ).map{ i => { (aluIssMatrix(iss)(i).forall( (y: Bool) => ( y === false.B ) )) -> aluIssInfo(i) } } )

    for( i <- 0 until dptEntry ) {
      when( aluIssFifo(iss).io.enq.fire & aluIssIdx(iss) === i.U ) {
        bufValid(i) := false.B
        assert( postIsOpReady(i)(0) & postIsOpReady(i)(1) )
        assert( bufValid(i) )
        assert( bufInfo(i).alu_isa.is_alu )
      }
    }
    aluIssFifo(iss).io.deq <> io.alu_iss_exe(iss)
    aluIssFifo(iss).reset := io.flush | reset.asBool
  }


}

trait IssSelMul{ this: IssueSel =>

  def Pkg_mul_iss(idx: Int): Mul_iss_info = {
    val res = Wire(new Mul_iss_info)
    res.fun := bufInfo(idx).mul_isa
 
    res.param.dat.op1 := postBufOperator(idx)(0)
    res.param.dat.op2 := postBufOperator(idx)(1)
    res.param.dat.op3 := 0.U
    res.param.dat.op4 := 0.U
    res.param.dat.op5 := 0.U
    res.param.rd0     := bufInfo(idx).phy.rd0
    res.param.rd1     := 0.U
    return res
  }

  if ( mulNum != 0 ) {
    val mulIssIdx  = Wire( Vec(mulNum, UInt((log2Ceil(dptEntry)).W) )  )
    val mulIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_mul_iss(i) }
    val mulIssFifo = for( i <- 0 until mulNum ) yield {
      Module(new Queue( new Mul_iss_info, ( if(!isMinArea) 4 else 1 ), flow = true ))
    }

    val mulIssMatrix   = Wire( Vec(mulNum, Vec( dptEntry, Vec(dptEntry, Bool() ) )) )
    val maskCondMulIss = Wire( Vec(mulNum, Vec( dptEntry, Bool()) ))

    for( iss <- 0 until mulNum ){
      if( iss == 0 ) {  
        for( i <- 0 until dptEntry ) {
          maskCondMulIss(iss)(i) := 
            ~bufValid(i) |
            ~bufInfo(i).mul_isa.is_mulDiv |
            ~(postIsOpReady(i)(0) & postIsOpReady(i)(1))
        }
      } else {
        for( i <- 0 until dptEntry ) {
          maskCondMulIss(iss)(i) := maskCondMulIss(iss-1)(i) | (i.U === mulIssIdx(iss-1))
        }
      }
      mulIssMatrix(iss) := MatrixMask( ageMatrixR, maskCondMulIss(iss) )
      
      assert(
        mulIssMatrix(iss).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) } ) |
        PopCount( mulIssMatrix(iss).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
      )
    }
    
    for( iss <- 0 until mulNum ){
      mulIssFifo(iss).io.enq.valid := ~mulIssMatrix(iss).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => y === true.B }} //all ture
      mulIssIdx(iss) := mulIssMatrix(iss).indexWhere( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === false.B) ) ) //index a row which all zero
      mulIssFifo(iss).io.enq.bits  := 
        Mux1H( ( 0 until dptEntry ).map{ i => { (mulIssMatrix(iss)(i).forall( (y: Bool) => ( y === false.B ) )) -> mulIssInfo(i) } } )

      for( i <- 0 until dptEntry ) {
        when( mulIssFifo(iss).io.enq.fire & mulIssIdx(iss) === i.U ) {
          bufValid(i) := false.B
          assert( postIsOpReady(i)(0) & postIsOpReady(i)(1) )
          assert( bufValid(i) )
          assert( bufInfo(i).mul_isa.is_mulDiv )
        }
      }
      mulIssFifo(iss).io.deq <> io.mul_iss_exe(iss)
      mulIssFifo(iss).reset := io.flush | reset.asBool
    }

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
    res.param.dat.op1 := postBufOperator(idx)(0)
    res.param.dat.op2 := postBufOperator(idx)(1)
    res.param.dat.op3 := 0.U
    res.param.dat.op4 := 0.U
    res.param.dat.op5 := 0.U
    res.param.rd0     := bufInfo(idx).phy.rd0
    res.param.rd1     := 0.U

    return res
  }

  val bruIssIdx = Wire( UInt((log2Ceil(dptEntry)).W) )
  val bruIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_bru_iss(i) }
  val bruIssFifo = Module(new Queue( new Bru_iss_info, ( if(!isMinArea) 4 else 1 ), flow = true ) )

  val bruIssMatrix   = Wire( Vec( dptEntry, Vec(dptEntry, Bool() ) )) 
  val maskCondBruIss = Wire( Vec( dptEntry, Bool()) )

  //only oldest instr will be selected
  for( i <- 0 until dptEntry ) {
    maskCondBruIss(i) := 
      ~bufValid(i) |
      ~bufInfo(i).bru_isa.is_bru
  }

  bruIssMatrix := MatrixMask( ageMatrixR, maskCondBruIss )
  
  assert(
    bruIssMatrix.forall( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === true.B) ) ) |
    PopCount( bruIssMatrix.map{(x: Vec[Bool]) => x.forall((y: Bool) => (y === false.B))} ) === 1.U
  )

  bruIssIdx := bruIssMatrix.indexWhere( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === false.B) ) ) //index a row which all zero

  bruIssFifo.io.enq.valid := 
    ( 0 until dptEntry ).map{ i => { bruIssMatrix(i).forall( (x: Bool) => (x === false.B) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) } }.reduce(_|_)

  bruIssFifo.io.enq.bits  := 
    Mux1H( ( 0 until dptEntry ).map{ i => { (bruIssMatrix(i).forall( (y: Bool) => ( y === false.B ) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) ) -> bruIssInfo(i) } } )

  for( i <- 0 until dptEntry ) {
    when( bruIssFifo.io.enq.fire & bruIssIdx === i.U ) {
      bufValid(i) := false.B
      assert( postIsOpReady(i)(0) & postIsOpReady(i)(1) )
      assert( bufValid(i) )
      assert( bufInfo(i).bru_isa.is_bru )
    }
  }

  bruIssFifo.io.deq <> io.bru_iss_exe
  bruIssFifo.reset := io.flush | reset.asBool

}

trait IssSelCsr{ this: IssueSel =>

  def Pkg_csr_iss(idx: Int): Csr_iss_info = {
    val res = Wire(new Csr_iss_info)
    val csrSel = bufInfo(idx).param.imm(11,0)

    res.fun.rc  := bufInfo(idx).csr_isa.rc | bufInfo(idx).csr_isa.rci
    res.fun.rs  := bufInfo(idx).csr_isa.rs | bufInfo(idx).csr_isa.rsi
    res.fun.rw  := bufInfo(idx).csr_isa.rw | bufInfo(idx).csr_isa.rwi

    res.param.dat.op1 := 
      Mux1H(Seq(
        bufInfo(idx).csr_isa.rw  -> postBufOperator(idx)(0), bufInfo(idx).csr_isa.rwi -> bufInfo(idx).param.raw.rs1,
        bufInfo(idx).csr_isa.rs  -> postBufOperator(idx)(0), bufInfo(idx).csr_isa.rsi -> bufInfo(idx).param.raw.rs1,
        bufInfo(idx).csr_isa.rc  -> postBufOperator(idx)(0), bufInfo(idx).csr_isa.rci -> bufInfo(idx).param.raw.rs1
      ))

    res.param.dat.op2 := 
      Mux1H( Seq(
        (csrSel === "hf11".U) -> io.csrfiles.mvendorid,
        (csrSel === "hf12".U) -> io.csrfiles.marchid,
        (csrSel === "hf13".U) -> io.csrfiles.mimpid,
        (csrSel === "hf14".U) -> io.csrfiles.mhartid,
        (csrSel === "h300".U) -> io.csrfiles.mstatus.asUInt,
        (csrSel === "h301".U) -> io.csrfiles.misa,
        (csrSel === "h302".U) -> io.csrfiles.medeleg,
        (csrSel === "h303".U) -> io.csrfiles.mideleg,
        (csrSel === "h304".U) -> io.csrfiles.mie.asUInt,
        (csrSel === "h305".U) -> io.csrfiles.mtvec.asUInt,
        (csrSel === "h306".U) -> io.csrfiles.mcounteren.asUInt,
        (csrSel === "h340".U) -> io.csrfiles.mscratch,
        (csrSel === "h341".U) -> io.csrfiles.mepc,
        (csrSel === "h342".U) -> io.csrfiles.mcause.asUInt,
        (csrSel === "h343".U) -> io.csrfiles.mtval,
        (csrSel === "h344".U) -> io.csrfiles.mip.asUInt,
        (csrSel === "h34A".U) -> io.csrfiles.mtinst,
        (csrSel === "h34B".U) -> io.csrfiles.mtval2,
        (csrSel === "hB00".U) -> io.csrfiles.mcycle,
        (csrSel === "hB02".U) -> io.csrfiles.minstret,
        (csrSel === "h100".U) -> io.csrfiles.sstatus.asUInt,
        // (csrSel === "h102".U) -> io.csrfiles.sedeleg,
        // (csrSel === "h103".U) -> io.csrfiles.sideleg,
        (csrSel === "h104".U) -> io.csrfiles.sie.asUInt,
        (csrSel === "h105".U) -> io.csrfiles.stvec.asUInt,
        (csrSel === "h106".U) -> io.csrfiles.scounteren.asUInt,
        (csrSel === "h140".U) -> io.csrfiles.sscratch,
        (csrSel === "h141".U) -> io.csrfiles.sepc,
        (csrSel === "h142".U) -> io.csrfiles.scause.asUInt,
        (csrSel === "h143".U) -> io.csrfiles.stval,
        (csrSel === "h144".U) -> io.csrfiles.sip.asUInt,
        (csrSel === "h180".U) -> io.csrfiles.satp.asUInt,
        (csrSel === "h7A0".U) -> io.csrfiles.tselect,
        (csrSel === "h7A1".U) -> io.csrfiles.tdata1,
        (csrSel === "h7A2".U) -> io.csrfiles.tdata2,
        (csrSel === "h7A3".U) -> io.csrfiles.tdata3,
        (csrSel === "h7B0".U) -> io.csrfiles.dcsr.asUInt,
        (csrSel === "h7B1".U) -> io.csrfiles.dpc,
        (csrSel === "h7B2".U) -> io.csrfiles.dscratch0,
        (csrSel === "h7B3".U) -> io.csrfiles.dscratch1,
        (csrSel === "h7B4".U) -> io.csrfiles.dscratch2,
        (csrSel === "h001".U) -> io.csrfiles.fcsr.fflags,
        (csrSel === "h002".U) -> io.csrfiles.fcsr.frm,
        (csrSel === "h003".U) -> io.csrfiles.fcsr.asUInt,
        (csrSel === "h320".U) -> io.csrfiles.mcountinhibit,
        (csrSel === "hC00".U) -> io.csrfiles.cycle,
        (csrSel === "hC01".U) -> io.csrfiles.time,
        (csrSel === "hC02".U) -> io.csrfiles.instret,
        ) ++

        (for( i <- 0 until pmpNum by 2 ) yield{
          ((csrSel === ("h3A0".U + i.U)) -> io.csrfiles.pmpcfg(i).asUInt)
        }) ++

        (for( i <- 0 until pmpNum*8 ) yield{
          ((csrSel === ("h3B0".U + i.U)) -> io.csrfiles.pmpaddr(i).asUInt)
        }) ++

        (for( i <- 3 until 32 ) yield{
          ((csrSel === ("hB00".U + i.U)) -> io.csrfiles.mhpmcounter(i))
        }) ++

        (for( i <- 3 until 32 ) yield{
          ((csrSel === ("h320".U + i.U)) -> io.csrfiles.mhpmevent(i))
        }) ++ 

        (for( i <- 3 until 32 ) yield{
          ((csrSel === ("hC00".U + i.U)) -> io.csrfiles.hpmcounter(i))
        })   
      )
      
    res.param.dat.op3 := 0.U
    res.param.dat.op4 := 0.U
    res.param.dat.op5 := 0.U

    res.param.rd0     := bufInfo(idx).phy.rd0
    res.param.rd1     := 0.U
    res.param.csrw    := bufInfo(idx).csrw

    return res
  }

  val csrIssIdx = Wire( UInt((log2Ceil(dptEntry)).W) )
  val csrIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_csr_iss(i) }
  val csrIssFifo = Module(new Queue( new Csr_iss_info, ( if(!isMinArea) 4 else 1 ), flow = true ) )

  val csrIssMatrix   = Wire( Vec( dptEntry, Vec(dptEntry, Bool() ) )) 
  val maskCondCsrIss = Wire( Vec( dptEntry, Bool()) )

  //only oldest instr will be selected
  for( i <- 0 until dptEntry ) {
    maskCondCsrIss(i) := 
      ~bufValid(i) |
      ~bufInfo(i).csr_isa.is_csr
  }

  csrIssMatrix := MatrixMask( ageMatrixR, maskCondCsrIss )
  
  assert(
    csrIssMatrix.forall( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === true.B) ) ) |
    PopCount( csrIssMatrix.map{(x: Vec[Bool]) => x.forall((y: Bool) => (y === false.B))} ) === 1.U
  )

  csrIssIdx := csrIssMatrix.indexWhere( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === false.B) ) ) //index a row which all zero

  csrIssFifo.io.enq.valid := 
    ( 0 until dptEntry ).map{ i => { csrIssMatrix(i).forall( (x: Bool) => (x === false.B) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) & isCSRRReady(i) } }.reduce(_|_)

  csrIssFifo.io.enq.bits  := 
    Mux1H( ( 0 until dptEntry ).map{ i => { (csrIssMatrix(i).forall( (y: Bool) => ( y === false.B ) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) & isCSRRReady(i) ) -> csrIssInfo(i) } } )

  for( i <- 0 until dptEntry ) {
    when( csrIssFifo.io.enq.fire & csrIssIdx === i.U ) {
      bufValid(i) := false.B
      assert( postIsOpReady(i)(0) & postIsOpReady(i)(1) )
      assert( bufValid(i) )
      assert( bufInfo(i).csr_isa.is_csr )
    }
  }

  csrIssFifo.io.deq <> io.csr_iss_exe
  csrIssFifo.reset := io.flush | reset.asBool
}

trait IssSelLsu{ this: IssueSel =>

  def Pkg_lsu_iss( idx: Int ): Lsu_iss_info = {
    val res = Wire(new Lsu_iss_info)

    res.fun := bufInfo(idx).lsu_isa

    res.param.dat.op1 := 
      Mux( (bufInfo(idx).lsu_isa.is_lrsc | bufInfo(idx).lsu_isa.is_amo), postBufOperator(idx)(0),  (postBufOperator(idx)(0).asSInt + bufInfo(idx).param.imm.asSInt()).asUInt() )
    res.param.dat.op2 :=
      Mux(
        bufInfo(idx).lsu_isa.is_fst,
        ieee(unbox(postBufOperator(idx)(1), 1.U, None), t = FType.D),
        postBufOperator(idx)(1)
      )
    res.param.dat.op3 := 0.U
    res.param.dat.op4 := 0.U
    res.param.dat.op5 := 0.U

    res.param.rd0 := bufInfo(idx).phy.rd0
    res.param.rd1 := 0.U

    return res
  }
  val lsuIssIdx = Wire( UInt((log2Ceil(dptEntry)).W) )
  val lsuIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_lsu_iss(i) }
  val lsuIssFifo = Module(new Queue( new Lsu_iss_info, ( if(!isMinArea) 4 else 1 ), flow = true ) )

  val lsuIssMatrix   = Wire( Vec( dptEntry, Vec(dptEntry, Bool() ) )) 
  val maskCondLsuIss = Wire( Vec( dptEntry, Bool()) )

  //only oldest instr will be selected
  for( i <- 0 until dptEntry ) {
    maskCondLsuIss(i) := 
      ~bufValid(i) |
      ~bufInfo(i).lsu_isa.is_lsu
  }

  lsuIssMatrix := MatrixMask( ageMatrixR, maskCondLsuIss )
  
  assert(
    lsuIssMatrix.forall( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === true.B) ) ) |
    PopCount( lsuIssMatrix.map{(x: Vec[Bool]) => x.forall((y: Bool) => (y === false.B))} ) === 1.U
  )

  lsuIssIdx := lsuIssMatrix.indexWhere( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === false.B) ) ) //index a row which all zero

  lsuIssFifo.io.enq.valid := 
    ( 0 until dptEntry ).map{ i => { lsuIssMatrix(i).forall( (x: Bool) => (x === false.B) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) } }.reduce(_|_)

  lsuIssFifo.io.enq.bits  := 
    Mux1H( ( 0 until dptEntry ).map{ i => { (lsuIssMatrix(i).forall( (y: Bool) => ( y === false.B ) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) ) -> lsuIssInfo(i) } } )

  for( i <- 0 until dptEntry ) {
    when( lsuIssFifo.io.enq.fire & lsuIssIdx === i.U ) {
      bufValid(i) := false.B
      assert( postIsOpReady(i)(0) & postIsOpReady(i)(1) )
      assert( bufValid(i) )
      assert( bufInfo(i).lsu_isa.is_lsu )
    }
  }

  lsuIssFifo.io.deq <> io.lsu_iss_exe
  lsuIssFifo.reset := io.flush | reset.asBool
}

trait IssSelFpu{ this: IssueSel =>

  require( fpuNum <= 1, s"fpu is not support out-of-order in this Vesrion!" )

  def Pkg_fpu_iss(idx: Int): Fpu_iss_info = {
    val res = Wire(new Fpu_iss_info)

    res.fun := bufInfo(idx).fpu_isa

    res.param.dat.op1 := postBufOperator(idx)(0)
    res.param.dat.op2 := postBufOperator(idx)(1)
    res.param.dat.op3 := postBufOperator(idx)(2)
    res.param.dat.op4 := io.csrfiles.fcsr.frm
    res.param.dat.op5 := 0.U
    
    res.param.rd0 := bufInfo(idx).phy.rd0
    res.param.rd1 := 0.U
    res.param.rm := bufInfo(idx).param.rm
    res.param.csrw    := bufInfo(idx).csrw

    return res
  }


  if( fpuNum != 0 ){
    val fpuIssIdx = Wire( Vec( fpuNum, UInt((log2Ceil(dptEntry)).W) ))
    val fpuIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_fpu_iss(i) }
    val fpuIssFifo = Module(new Queue( new Fpu_iss_info, ( if(!isMinArea) 4 else 1 ), flow = true))

    val fpuIssMatrix   = Wire( Vec(fpuNum, Vec( dptEntry, Vec(dptEntry, Bool() ) )) )
    val maskCondFpuIss = Wire( Vec(fpuNum, Vec( dptEntry, Bool()) ))



  
    for( i <- 0 until dptEntry ) {
      maskCondFpuIss(0)(i) := 
        ~bufValid(i) |
        ~bufInfo(i).fpu_isa.is_fpu
    }

    fpuIssMatrix(0) := MatrixMask( ageMatrixR, maskCondFpuIss(0) )
    
    assert(
      fpuIssMatrix(0).forall( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === true.B) ) ) |
      PopCount( fpuIssMatrix(0).map{(x: Vec[Bool]) => x.forall((y: Bool) => (y === false.B))} ) === 1.U
    )

    fpuIssFifo.io.enq.valid := 
      ( 0 until dptEntry ).map{ i => { fpuIssMatrix(0)(i).forall( (x: Bool) => (x === false.B) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) & postIsOpReady(i)(2) & isFRMRReady(i) } }.reduce(_|_)

    fpuIssFifo.io.enq.bits  := 
      Mux1H( ( 0 until dptEntry ).map{ i => { (fpuIssMatrix(0)(i).forall( (y: Bool) => ( y === false.B ) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) & postIsOpReady(i)(2) & isFRMRReady(i) ) -> fpuIssInfo(i) } } )
    

    fpuIssIdx(0) := fpuIssMatrix(0).indexWhere( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === false.B) ) ) //index a row which all zero

    for( i <- 0 until dptEntry ) {
      when( fpuIssFifo.io.enq.fire & fpuIssIdx(0) === i.U ) {
        bufValid(i) := false.B
        assert( postIsOpReady(i)(0) & postIsOpReady(i)(1) & postIsOpReady(i)(2) )
        assert( bufValid(i) )
        assert( bufInfo(i).fpu_isa.is_fpu )
      }
    }

    fpuIssFifo.io.deq <> io.fpu_iss_exe(0)
    fpuIssFifo.reset := io.flush | reset.asBool
  } else {
    io.fpu_iss_exe(0).valid := false.B
    io.fpu_iss_exe(0).bits  := DontCare
  }

}

trait IssSelVpu{ this: IssueSel =>

}


class Issue()(implicit p: Parameters) extends IssueSel
with IssSelAlu
with IssSelMul
with IssSelBru
with IssSelCsr
with IssSelLsu
with IssSelFpu{
  
}

