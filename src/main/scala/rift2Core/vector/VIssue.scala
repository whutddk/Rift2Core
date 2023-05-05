
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


abstract class VPUDispatchBase()(implicit p: Parameters) extends RiftModule{

}



// abstract class DptBase ()(implicit p: Parameters) extends RiftModule with HasFPUParameters{
//   // def dptEntry = 16

//   class IssueIO extends Bundle{
//     val dptReq = Vec(rnChn, Flipped(new DecoupledIO(new Dpt_info)))

//     val alu_iss_exe = Vec(aluNum, new DecoupledIO(new Alu_iss_info))
//     val mul_iss_exe = Vec(mulNum max 1, new DecoupledIO(new Mul_iss_info))
//     val bru_iss_exe = new DecoupledIO(new Bru_iss_info)
//     val csr_iss_exe = new DecoupledIO(new Csr_iss_info)
//     val lsu_iss_exe = new DecoupledIO(new Lsu_iss_info)
//     val fpu_iss_exe = Vec(fpuNum max 1, new DecoupledIO(new Fpu_iss_info))

//     // val vpu_iss_exe = new DecoupledIO(new Vpu_iss_info)

//     val irgLog = Input( Vec(xRegNum, UInt(2.W)) )
//     val frgLog = Input( Vec(fRegNum, UInt(2.W)) )
//     val vrgLog = Input( Vec(vRegNum, UInt(2.W)) )

//     val irgReq = Vec( opChn, Valid( UInt((log2Ceil(xRegNum)).W) ) )
//     val frgReq = Vec( opChn, Valid( UInt((log2Ceil(fRegNum)).W) ) )    
//     val vrgReq = Vec( vParams.opChn, Valid( UInt((log2Ceil(vRegNum)).W) ) )    

//     val irgRsp = Vec( opChn, Flipped(Valid(new ReadOp_Rsp_Bundle(64)) ) )
//     val frgRsp = Vec( opChn, Flipped(Valid(new ReadOp_Rsp_Bundle(65)) ) )
//     val vrgRsp = Vec( vParams.opChn, Flipped(Valid(new ReadOp_Rsp_Bundle(vParams.vlen)) ) )

//     val csrfiles = Input(new CSR_Bundle)

//     val flush = Input(Bool())    
//   } 

//   val io: IssueIO = IO(new IssueIO)
// }


// abstract class DptBoard()(implicit p: Parameters) extends DptBase {
//   val bufValidDnxt = Wire( Vec(rnChn, Vec( dptEntry, Bool() ) ))
//   val bufValid     = RegInit( VecInit( Seq.fill(dptEntry){false.B} ))
//   val bufInfo      = Reg(  Vec( dptEntry, new Dpt_info              ))
//   val bufReqNum    = Wire( Vec( dptEntry, Vec( 4, UInt((log2Ceil(maxRegNum)).W)) ))
//   val isBufXop     = Reg(  Vec( dptEntry, Vec( 4, Bool()    )) ) //skip op0 op3
//   val isBufFop     = Reg(  Vec( dptEntry, Vec( 4, Bool()    )) ) //skip op0
//   val isBufVop     = Reg(  Vec( dptEntry, Vec( 4, Bool()    )) )
//   val isOpReady    = Reg(  Vec( dptEntry, Vec( 4, Bool()    )) )
//   val bufOperator  = Reg(  Vec( dptEntry, Vec( 4, UInt(vParams.vlen.W))) )

//   val entrySel     = for( i <- 0 until rnChn ) yield {
//     if( i == 0 ) {
//       bufValid.indexWhere( (x:Bool) => (x === false.B) )
//     } else {
//       bufValidDnxt(i-1).indexWhere( (x:Bool) => (x === false.B) )
//     }
//   }

//   bufValid := bufValidDnxt( rnChn-1 )

//   for( i <- 0 until rnChn ) {
//     io.dptReq(i).ready := PopCount( bufValid.map{ x => (x === false.B) } ) > i.U


//     if ( i == 0 ) {
//       bufValidDnxt(i) := bufValid
//     } else {
//       bufValidDnxt(i) := bufValidDnxt(i-1)
//     }

//     when( io.flush ) {
//       bufValidDnxt(i) := 0.U.asTypeOf(bufValidDnxt(i))
//     } .elsewhen( io.dptReq(i).fire ) {
//       bufValidDnxt(i)(entrySel(i)) := true.B
//       bufInfo(entrySel(i)) := io.dptReq(i).bits
//     }
//   }

//   for( i <- 0 until rnChn ) {
//     when( io.dptReq(i).fire ) {
//       isBufXop(entrySel(i))(1) := io.dptReq(i).bits.isRS1
//       isBufXop(entrySel(i))(2) := io.dptReq(i).bits.isRS2

//       isBufFop(entrySel(i))(1) := io.dptReq(i).bits.isFS1
//       isBufFop(entrySel(i))(2) := io.dptReq(i).bits.isFS2
//       isBufFop(entrySel(i))(3) := io.dptReq(i).bits.isFS3

//       isBufVop(entrySel(i))(0) := io.dptReq(i).bits.isVM0
//       isBufVop(entrySel(i))(1) := io.dptReq(i).bits.isVS1
//       isBufVop(entrySel(i))(2) := io.dptReq(i).bits.isVS2
//       isBufVop(entrySel(i))(3) := io.dptReq(i).bits.isVS3

//     }
//   }



//   for( i <- 0 until rnChn ) {
//     when( io.dptReq(i).fire ) {
//       when( io.dptReq(i).bits.phy.vm0 === 0.U ) {
//         isOpReady (entrySel(i))(0) := true.B
//         bufOperator(entrySel(i))(0) := 0.U
//       } .otherwise {
//         isOpReady (entrySel(i))(0) := false.B
//       }
//       when( io.dptReq(i).bits.phy.rs1 === 0.U ) {
//         isOpReady (entrySel(i))(1) := true.B
//         bufOperator(entrySel(i))(1) := 0.U
//       } .otherwise {
//         isOpReady (entrySel(i))(1) := false.B
//       }
//       when( io.dptReq(i).bits.phy.rs2 === 0.U ) {
//         isOpReady (entrySel(i))(2) := true.B
//         bufOperator(entrySel(i))(2) := 0.U
//       } .otherwise{
//         isOpReady (entrySel(i))(2) := false.B
//       }
//       when( io.dptReq(i).bits.phy.rs3 === 0.U ) {
//         isOpReady (entrySel(i))(3) := true.B
//         bufOperator(entrySel(i))(3) := 0.U
//       } .otherwise {
//         isOpReady (entrySel(i))(3) := false.B
//       }
//     }
//   }

//   for( i <- 0 until dptEntry ){
//     dontTouch(bufReqNum)
//     bufReqNum(i)(0) := bufInfo(i).phy.vm0
//     bufReqNum(i)(1) := bufInfo(i).phy.rs1
//     bufReqNum(i)(2) := bufInfo(i).phy.rs2
//     bufReqNum(i)(3) := bufInfo(i).phy.rs3
//   }

// }


// abstract class DptAgeMatrix()(implicit p: Parameters) extends DptBoard {


//   val ageMatrixR = Wire( Vec( dptEntry, Vec(dptEntry, Bool()) ) )
//   val ageMatrixW = Reg( Vec( dptEntry, Vec(dptEntry, Bool()) ) )
//   // dontTouch(ageMatrixR)

//   for ( i <- 0 until dptEntry ) {
//     for( j <- 0 until i ) {
//       ageMatrixR(i)(j) := ~ageMatrixW(j)(i)
//     }
//     ageMatrixR(i)(i) := DontCare
//     for( j <- i until dptEntry ) {
//       ageMatrixR(i)(j) :=  ageMatrixW(i)(j)
//     }
//   }

//   for( i <- 0 until rnChn ) {
//     when( io.dptReq(i).fire ) {
//       ageMatrixW(entrySel(i)).map{ x => {x := true.B} }
//       ageMatrixW.map{ x => {x(entrySel(i)) := false.B} }
//     }
//   }

//   val ageMatrixPostR  = Wire( Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool()) ) )
//   for( i <- 0 until dptEntry+rnChn ) {
//     ageMatrixPostR(i)(i) := DontCare
//     for( j <- 0 until dptEntry+rnChn ) {
//       if( i < dptEntry  && j < dptEntry  )  ageMatrixPostR(i)(j) := ageMatrixR(i)(j)
//       if( i < dptEntry  && j >= dptEntry )  ageMatrixPostR(i)(j) := false.B
//       if( i >= dptEntry && j < dptEntry  )  ageMatrixPostR(i)(j) := true.B
//       if( i >= dptEntry && j >= dptEntry ) {
//         if( i < j ) ageMatrixPostR(i)(j) := false.B
//         if( i > j ) ageMatrixPostR(i)(j) := true.B
//       }
//     }
//   }


//   def MatrixMask( matrixIn: Vec[Vec[Bool]], maskCond: Vec[Bool] ): Vec[Vec[Bool]]= {
//     val len = matrixIn.length
//     require( matrixIn.length    == len )
//     require( matrixIn(0).length == len )
//     require( maskCond.length    == len )

//     val matrixOut = Wire( Vec( len, Vec(len, Bool()) ))
//     for ( i <- 0 until len ){
//       for ( j <- 0 until len ){
//         matrixOut(i)(j) := (matrixIn(i)(j) & ~maskCond(j)) | maskCond(i)
//       }
//     }
//     return matrixOut
//   }
// }

// trait DptReadIOp { this: DptAgeMatrix =>
//   val rIOpNum = Wire( Vec( opChn, UInt((log2Ceil(xRegNum)).W)) )


//   /** Whether this rs can request operator reading */
//   val canIOpReq     = Wire( Vec( dptEntry, Vec( 4, Bool() )))


//   for( i <- 0 until dptEntry ) {
//     canIOpReq(i)(0) := DontCare
//     canIOpReq(i)(3) := DontCare

//     canIOpReq(i)(1) := 
//       io.irgLog(bufInfo(i).phy.rs1) === "b11".U & //reg-log is ready
//       isBufXop(i)(1) & //is a Xop req
//       ~isOpReady(i)(1) & ~io.irgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(1))}}.reduce(_|_) //pending and non-rsp in same cycle
    
//     canIOpReq(i)(2) :=
//       io.irgLog(bufInfo(i).phy.rs2) === "b11".U & //reg-log is ready
//       isBufXop(i)(2) & //is a Xop req
//       ~isOpReady(i)(2) & ~io.irgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(2))}}.reduce(_|_) //pending and non-rsp in same cycle
//   }

//   val canIOpPostReq = Wire( Vec( rnChn, Vec( 4, Bool() )) )

//   for( i <- 0 until rnChn ) {
//     canIOpPostReq(i)(0) := DontCare
//     canIOpPostReq(i)(3) := DontCare

//     canIOpPostReq(i)(1) := 
//       io.irgLog(io.dptReq(i).bits.phy.rs1) === "b11".U & //reg-log is ready
//       io.dptReq(i).bits.phy.rs1 =/= 0.U &
//       io.dptReq(i).bits.isRS1
    
//     canIOpPostReq(i)(2) :=
//       io.irgLog(io.dptReq(i).bits.phy.rs2) === "b11".U & //reg-log is ready
//       io.dptReq(i).bits.phy.rs2 =/= 0.U &
//        io.dptReq(i).bits.isRS2
//   }



//   /** Who is the highest priority to read operator in each chn */
//   val selMatrixIRS1 = Wire( Vec( opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
//   val selMatrixIRS2 = Wire( Vec( opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
//   val maskCondSelIRS1 = Wire( Vec( opChn, Vec( dptEntry+rnChn, Bool() ) ) )
//   val maskCondSelIRS2 = Wire( Vec( opChn, Vec( dptEntry+rnChn, Bool() ) ) )



//   for( chn <- 0 until opChn ){
//     if( chn == 0 ){
//       for ( i <- 0 until dptEntry ){
//         maskCondSelIRS1(chn)(i) := ~bufValid(i) | ~canIOpReq(i)(1)
//         maskCondSelIRS2(chn)(i) := ~bufValid(i) | ~canIOpReq(i)(2)
//       }
//       for ( i <- 0 until rnChn ){
//         maskCondSelIRS1(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canIOpPostReq(i)(1)
//         maskCondSelIRS2(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canIOpPostReq(i)(2)
//       }
//     } else {
//       for ( i <- 0 until dptEntry ){
//         maskCondSelIRS1(chn)(i) := maskCondSelIRS1(chn-1)(i) | (bufReqNum(i)(1) === rIOpNum(chn-1))
//         maskCondSelIRS2(chn)(i) := maskCondSelIRS2(chn-1)(i) | (bufReqNum(i)(2) === rIOpNum(chn-1))
//       }
//       for( i <- 0 until rnChn ){
//         maskCondSelIRS1(chn)(dptEntry+i) := maskCondSelIRS1(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs1 === rIOpNum(chn-1))
//         maskCondSelIRS2(chn)(dptEntry+i) := maskCondSelIRS2(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs2 === rIOpNum(chn-1))        
//       }
//     }
//     selMatrixIRS1(chn) := MatrixMask( ageMatrixPostR, maskCondSelIRS1(chn) )
//     selMatrixIRS2(chn) := MatrixMask( ageMatrixPostR, maskCondSelIRS2(chn) )

//     assert(
//       selMatrixIRS1(chn).forall( (x: Vec[Bool]) => x.forall{(y: Bool) => (y === true.B)} ) |
//       PopCount( selMatrixIRS1(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
//     )

//     assert(
//       selMatrixIRS2(chn).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B)} ) |
//       PopCount( selMatrixIRS2(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
//     )
//   }



//   val isIRS1NoneReq = Wire( Vec(opChn, Bool()) )
//   val isIRS2NoneReq = Wire( Vec(opChn, Bool()) )



//   val selIRS1 = Wire( Vec(opChn, UInt((log2Ceil(xRegNum)).W) ) )
//   val selIRS2 = Wire( Vec(opChn, UInt((log2Ceil(xRegNum)).W) ) )

//   for( chn <- 0 until opChn ){
//     isIRS1NoneReq(chn) := selMatrixIRS1(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
//     isIRS2NoneReq(chn) := selMatrixIRS2(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture

//     selIRS1(chn) :=
//       Mux1H(
//         ( 0 until dptEntry ).map{ i => { (selMatrixIRS1(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(1) } } ++
//         ( 0 until rnChn    ).map{ i => { (selMatrixIRS1(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs1 }}
//       ) //index a row which all zero
//     selIRS2(chn) :=
//       Mux1H(
//         ( 0 until dptEntry ).map{ i => { (selMatrixIRS2(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(2) } } ++
//         ( 0 until rnChn    ).map{ i => { (selMatrixIRS2(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs2 }}
//       ) //index a row which all zero

//     if( chn % 2 == 0 ){
//       rIOpNum(chn) := Mux( ~isIRS1NoneReq(chn), selIRS1(chn), Mux( ~isIRS2NoneReq(chn), selIRS2(chn), 0.U ))
//     } else {
//       rIOpNum(chn) := Mux( ~isIRS2NoneReq(chn), selIRS2(chn), Mux( ~isIRS1NoneReq(chn), selIRS1(chn), 0.U ))
//     }

//     io.irgReq(chn).valid := ~isIRS1NoneReq(chn) | ~isIRS2NoneReq(chn)
//     io.irgReq(chn).bits  := rIOpNum(chn)
//   }




// }

// trait DptReadFOp { this: DptAgeMatrix =>
//   val rFOpNum = Wire( Vec( opChn, UInt((log2Ceil(fRegNum)).W)) )


//   /** Whether this rs can request operator reading */
//   val canFOpReq = Wire( Vec( dptEntry, Vec( 4, Bool() )))

//   for( i <- 0 until dptEntry ) {
//     canFOpReq(i)(0) := DontCare

//     canFOpReq(i)(1) := 
//       io.frgLog(bufInfo(i).phy.rs1) === "b11".U & //reg-log is ready
//       isBufFop(i)(1) & //is a fop req
//       ~isOpReady(i)(1) & ~io.frgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(1))}}.reduce(_|_) //pending and non-rsp in same cycle

//     canFOpReq(i)(2) :=
//       io.frgLog(bufInfo(i).phy.rs2) === "b11".U & //reg-log is ready
//       isBufFop(i)(2) & //is a fop req
//       ~isOpReady(i)(2) & ~io.frgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(2))}}.reduce(_|_) //pending and non-rsp in same cycle

//     canFOpReq(i)(3) :=
//       io.frgLog(bufInfo(i).phy.rs3) === "b11".U & //reg-log is ready
//       isBufFop(i)(3) & //is a fop req
//       ~isOpReady(i)(3) & ~io.frgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(3))}}.reduce(_|_) //pending and non-rsp in same cycle
//   }

//   val canFOpPostReq = Wire( Vec( rnChn, Vec( 4, Bool() )) )

//   for( i <- 0 until rnChn ) {
//     canFOpPostReq(i)(0) := DontCare

//     canFOpPostReq(i)(1) := 
//       io.frgLog(io.dptReq(i).bits.phy.rs1) === "b11".U & //reg-log is ready
//       io.dptReq(i).bits.phy.rs1 =/= 0.U &
//       io.dptReq(i).bits.isFS1
    
//     canFOpPostReq(i)(2) :=
//       io.frgLog(io.dptReq(i).bits.phy.rs2) === "b11".U & //reg-log is ready
//       io.dptReq(i).bits.phy.rs2 =/= 0.U &
//       io.dptReq(i).bits.isFS2

//     canFOpPostReq(i)(3) :=
//       io.frgLog(io.dptReq(i).bits.phy.rs3) === "b11".U & //reg-log is ready
//       io.dptReq(i).bits.phy.rs3 =/= 0.U &
//       io.dptReq(i).bits.isFS3
//   }


//   /** Who is the highest priority to read operator in each chn */
//   val selMatrixFRS1   = Wire( Vec( opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
//   val selMatrixFRS2   = Wire( Vec( opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
//   val selMatrixFRS3   = Wire( Vec( opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
//   val maskCondSelFRS1 = Wire( Vec( opChn, Vec( dptEntry+rnChn, Bool() ) ) )
//   val maskCondSelFRS2 = Wire( Vec( opChn, Vec( dptEntry+rnChn, Bool() ) ) )
//   val maskCondSelFRS3 = Wire( Vec( opChn, Vec( dptEntry+rnChn, Bool() ) ) )



//   for( chn <- 0 until opChn ){
//     if( chn == 0 ){
//       for ( i <- 0 until dptEntry ){
//         maskCondSelFRS1(chn)(i) := ~bufValid(i) | ~canFOpReq(i)(1)
//         maskCondSelFRS2(chn)(i) := ~bufValid(i) | ~canFOpReq(i)(2)
//         maskCondSelFRS3(chn)(i) := ~bufValid(i) | ~canFOpReq(i)(3)
//       }
//       for ( i <- 0 until rnChn ){
//         maskCondSelFRS1(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canFOpPostReq(i)(1)
//         maskCondSelFRS2(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canFOpPostReq(i)(2)
//         maskCondSelFRS3(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canFOpPostReq(i)(3)
//       }
//     } else {
//       for ( i <- 0 until dptEntry ){
//         maskCondSelFRS1(chn)(i) := maskCondSelFRS1(chn-1)(i) | (bufReqNum(i)(1) === rFOpNum(chn-1))
//         maskCondSelFRS2(chn)(i) := maskCondSelFRS2(chn-1)(i) | (bufReqNum(i)(2) === rFOpNum(chn-1))
//         maskCondSelFRS3(chn)(i) := maskCondSelFRS3(chn-1)(i) | (bufReqNum(i)(3) === rFOpNum(chn-1))
//       }
//       for( i <- 0 until rnChn ){
//         maskCondSelFRS1(chn)(dptEntry+i) := maskCondSelFRS1(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs1 === rFOpNum(chn-1))
//         maskCondSelFRS2(chn)(dptEntry+i) := maskCondSelFRS2(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs2 === rFOpNum(chn-1))        
//         maskCondSelFRS3(chn)(dptEntry+i) := maskCondSelFRS3(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs3 === rFOpNum(chn-1))        
//       }
//     }
//     selMatrixFRS1(chn) := MatrixMask( ageMatrixPostR, maskCondSelFRS1(chn) )
//     selMatrixFRS2(chn) := MatrixMask( ageMatrixPostR, maskCondSelFRS2(chn) )
//     selMatrixFRS3(chn) := MatrixMask( ageMatrixPostR, maskCondSelFRS3(chn) )

//     assert(
//       selMatrixFRS1(chn).forall( (x: Vec[Bool]) => x.forall{(y: Bool) => (y === true.B)} ) |
//       PopCount( selMatrixFRS1(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
//     )

//     assert(
//       selMatrixFRS2(chn).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B)} ) |
//       PopCount( selMatrixFRS2(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
//     )

//     assert(
//       selMatrixFRS3(chn).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B)} ) |
//       PopCount( selMatrixFRS3(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
//     )
//   }



//   val isFRS1NoneReq = Wire( Vec(opChn, Bool()) )
//   val isFRS2NoneReq = Wire( Vec(opChn, Bool()) )
//   val isFRS3NoneReq = Wire( Vec(opChn, Bool()) )
//   // dontTouch(isFRS1NoneReq)
//   // dontTouch(isFRS2NoneReq)
//   val selFRS1 = Wire( Vec(opChn,  UInt((log2Ceil(fRegNum)).W) ) )
//   val selFRS2 = Wire( Vec(opChn,  UInt((log2Ceil(fRegNum)).W) ) )
//   val selFRS3 = Wire( Vec(opChn,  UInt((log2Ceil(fRegNum)).W) ) )

//   for( chn <- 0 until opChn ){
//     isFRS1NoneReq(chn) := selMatrixFRS1(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
//     isFRS2NoneReq(chn) := selMatrixFRS2(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
//     isFRS3NoneReq(chn) := selMatrixFRS3(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture

//     selFRS1(chn) :=
//       Mux1H(
//         ( 0 until dptEntry ).map{ i => { (selMatrixFRS1(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(1) } } ++
//         ( 0 until rnChn    ).map{ i => { (selMatrixFRS1(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs1 }}
//       ) //index a row which all zero
//     selFRS2(chn) :=
//       Mux1H(
//         ( 0 until dptEntry ).map{ i => { (selMatrixFRS2(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(2) } } ++
//         ( 0 until rnChn    ).map{ i => { (selMatrixFRS2(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs2 }}

//       ) //index a row which all zero
//     selFRS3(chn) :=
//       Mux1H(
//         ( 0 until dptEntry ).map{ i => { (selMatrixFRS3(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(3) } } ++
//         ( 0 until rnChn    ).map{ i => { (selMatrixFRS3(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs3 }}
//       ) //index a row which all zero

//     if( chn % 3 == 0 ){
//       rFOpNum(chn) := Mux( ~isFRS1NoneReq(chn), selFRS1(chn), Mux( ~isFRS2NoneReq(chn), selFRS2(chn), Mux( ~isFRS3NoneReq(chn), selFRS3(chn), 0.U) ))
//     } else if( chn % 3 == 1 ){
//       rFOpNum(chn) := Mux( ~isFRS2NoneReq(chn), selFRS2(chn), Mux( ~isFRS3NoneReq(chn), selFRS3(chn), Mux( ~isFRS1NoneReq(chn), selFRS1(chn), 0.U) ))
//     } else {
//       rFOpNum(chn) := Mux( ~isFRS3NoneReq(chn), selFRS3(chn), Mux( ~isFRS1NoneReq(chn), selFRS1(chn), Mux( ~isFRS2NoneReq(chn), selFRS2(chn), 0.U) ))
//     }


//     io.frgReq(chn).valid := ~isFRS1NoneReq(chn) | ~isFRS2NoneReq(chn) | ~isFRS3NoneReq(chn)
//     io.frgReq(chn).bits  := rFOpNum(chn)
//   }


// }

// trait DptReadVOp { this: DptAgeMatrix =>
//   val rVOpNum = Wire( Vec( vParams.opChn, UInt((log2Ceil(vRegNum)).W)) )


//   /** Whether this vs can request operator reading */
//   val canVOpReq = Wire( Vec( dptEntry, Vec( 4, Bool() )))

//   for( i <- 0 until dptEntry ) {
//     canVOpReq(i)(0) :=
//       io.vrgLog(bufInfo(i).phy.vm0) === "b11".U & //reg-log is ready
//       isBufVop(i)(0) & //is a vop req
//       ~isOpReady(i)(0) & ~io.vrgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(0))}}.reduce(_|_) //pending and non-rsp in same cycle

//     canVOpReq(i)(1) := 
//       io.vrgLog(bufInfo(i).phy.rs1) === "b11".U & //reg-log is ready
//       isBufVop(i)(1) & //is a vop req
//       ~isOpReady(i)(1) & ~io.vrgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(1))}}.reduce(_|_) //pending and non-rsp in same cycle

//     canVOpReq(i)(2) :=
//       io.vrgLog(bufInfo(i).phy.rs2) === "b11".U & //reg-log is ready
//       isBufVop(i)(2) & //is a vop req
//       ~isOpReady(i)(2) & ~io.vrgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(2))}}.reduce(_|_) //pending and non-rsp in same cycle

//     canVOpReq(i)(3) :=
//       io.vrgLog(bufInfo(i).phy.rs3) === "b11".U & //reg-log is ready
//       isBufVop(i)(3) & //is a vop req
//       ~isOpReady(i)(3) & ~io.vrgRsp.map{x => {x.valid & (x.bits.phy === bufReqNum(i)(3))}}.reduce(_|_) //pending and non-rsp in same cycle
    

//   }

//   val canVOpPostReq = Wire( Vec( rnChn, Vec( 4, Bool() )) )

//   for( i <- 0 until rnChn ) {
//     canVOpPostReq(i)(0) :=
//       io.vrgLog(io.dptReq(i).bits.phy.vm0) === "b11".U & //reg-log is ready
//       io.dptReq(i).bits.phy.vm0 =/= 0.U &
//       io.dptReq(i).bits.isVM0

//     canVOpPostReq(i)(1) := 
//       io.vrgLog(io.dptReq(i).bits.phy.rs1) === "b11".U & //reg-log is ready
//       io.dptReq(i).bits.phy.rs1 =/= 0.U &
//       io.dptReq(i).bits.isVS1
    
//     canVOpPostReq(i)(2) :=
//       io.vrgLog(io.dptReq(i).bits.phy.rs2) === "b11".U & //reg-log is ready
//       io.dptReq(i).bits.phy.rs2 =/= 0.U &
//       io.dptReq(i).bits.isVS2

//     canVOpPostReq(i)(3) :=
//       io.vrgLog(io.dptReq(i).bits.phy.rs3) === "b11".U & //reg-log is ready
//       io.dptReq(i).bits.phy.rs3 =/= 0.U &
//       io.dptReq(i).bits.isVS3
      
//   }


//   /** Who is the highest priority to read operator in each chn */
//   val selMatrixVMS0   = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
//   val selMatrixVRS1   = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
//   val selMatrixVRS2   = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )
//   val selMatrixVRS3   = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Vec(dptEntry+rnChn, Bool() ) ) ) )

//   val maskCondSelVMS0 = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Bool() ) ) )
//   val maskCondSelVRS1 = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Bool() ) ) )
//   val maskCondSelVRS2 = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Bool() ) ) )
//   val maskCondSelVRS3 = Wire( Vec( vParams.opChn, Vec( dptEntry+rnChn, Bool() ) ) )




//   for( chn <- 0 until vParams.opChn ){
//     if( chn == 0 ){
//       for ( i <- 0 until dptEntry ){
//         maskCondSelVMS0(chn)(i) := ~bufValid(i) | ~canVOpReq(i)(0)
//         maskCondSelVRS1(chn)(i) := ~bufValid(i) | ~canVOpReq(i)(1)
//         maskCondSelVRS2(chn)(i) := ~bufValid(i) | ~canVOpReq(i)(2)
//         maskCondSelVRS3(chn)(i) := ~bufValid(i) | ~canVOpReq(i)(3)
//       }
//       for ( i <- 0 until rnChn ){
//         maskCondSelVMS0(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canVOpPostReq(i)(0)
//         maskCondSelVRS1(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canVOpPostReq(i)(1)
//         maskCondSelVRS2(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canVOpPostReq(i)(2)
//         maskCondSelVRS3(chn)(dptEntry+i) := ~io.dptReq(i).fire | ~canVOpPostReq(i)(3)
//       }
//     } else {
//       for ( i <- 0 until dptEntry ){
//         maskCondSelVMS0(chn)(i) := maskCondSelVMS0(chn-1)(i) | (bufReqNum(i)(0) === rVOpNum(chn-1))
//         maskCondSelVRS1(chn)(i) := maskCondSelVRS1(chn-1)(i) | (bufReqNum(i)(1) === rVOpNum(chn-1))
//         maskCondSelVRS2(chn)(i) := maskCondSelVRS2(chn-1)(i) | (bufReqNum(i)(2) === rVOpNum(chn-1))
//         maskCondSelVRS3(chn)(i) := maskCondSelVRS3(chn-1)(i) | (bufReqNum(i)(3) === rVOpNum(chn-1))
//       }
//       for( i <- 0 until rnChn ){
//         maskCondSelVMS0(chn)(dptEntry+i) := maskCondSelVMS0(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.vm0 === rVOpNum(chn-1))
//         maskCondSelVRS1(chn)(dptEntry+i) := maskCondSelVRS1(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs1 === rVOpNum(chn-1))
//         maskCondSelVRS2(chn)(dptEntry+i) := maskCondSelVRS2(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs2 === rVOpNum(chn-1))
//         maskCondSelVRS3(chn)(dptEntry+i) := maskCondSelVRS3(chn-1)(dptEntry+i) | (io.dptReq(i).bits.phy.rs3 === rVOpNum(chn-1))
//       }
//     }
//     selMatrixVMS0(chn) := MatrixMask( ageMatrixPostR, maskCondSelVMS0(chn) )
//     selMatrixVRS1(chn) := MatrixMask( ageMatrixPostR, maskCondSelVRS1(chn) )
//     selMatrixVRS2(chn) := MatrixMask( ageMatrixPostR, maskCondSelVRS2(chn) )
//     selMatrixVRS3(chn) := MatrixMask( ageMatrixPostR, maskCondSelVRS3(chn) )

//     assert(
//       selMatrixVMS0(chn).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B)} ) |
//       PopCount( selMatrixVMS0(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
//     )

//     assert(
//       selMatrixVRS1(chn).forall( (x: Vec[Bool]) => x.forall{(y: Bool) => (y === true.B)} ) |
//       PopCount( selMatrixVRS1(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
//     )

//     assert(
//       selMatrixVRS2(chn).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B)} ) |
//       PopCount( selMatrixVRS2(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
//     )

//     assert(
//       selMatrixVRS3(chn).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B)} ) |
//       PopCount( selMatrixVRS3(chn).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
//     )

//   }

//   val isVMS0NoneReq = Wire( Vec( vParams.opChn, Bool()) )
//   val isVRS1NoneReq = Wire( Vec( vParams.opChn, Bool()) )
//   val isVRS2NoneReq = Wire( Vec( vParams.opChn, Bool()) )
//   val isVRS3NoneReq = Wire( Vec( vParams.opChn, Bool()) )

//   val selVMS0 = Wire( Vec(vParams.opChn,  UInt((log2Ceil(vRegNum)).W) ) )
//   val selVRS1 = Wire( Vec(vParams.opChn,  UInt((log2Ceil(vRegNum)).W) ) )
//   val selVRS2 = Wire( Vec(vParams.opChn,  UInt((log2Ceil(vRegNum)).W) ) )
//   val selVRS3 = Wire( Vec(vParams.opChn,  UInt((log2Ceil(vRegNum)).W) ) )

//   for( chn <- 0 until vParams.opChn ){
//     isVMS0NoneReq(chn) := selMatrixVMS0(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
//     isVRS1NoneReq(chn) := selMatrixVRS1(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
//     isVRS2NoneReq(chn) := selMatrixVRS2(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
//     isVRS3NoneReq(chn) := selMatrixVRS3(chn).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture

//     selVMS0(chn) :=
//       Mux1H(
//         ( 0 until dptEntry ).map{ i => { (selMatrixVMS0(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(0) } } ++
//         ( 0 until rnChn    ).map{ i => { (selMatrixVMS0(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.vm0 }}
//       ) //index a row which all zero
//     selVRS1(chn) :=
//       Mux1H(
//         ( 0 until dptEntry ).map{ i => { (selMatrixVRS1(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(1) } } ++
//         ( 0 until rnChn    ).map{ i => { (selMatrixVRS1(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs1 }}
//       ) //index a row which all zero
//     selVRS2(chn) :=
//       Mux1H(
//         ( 0 until dptEntry ).map{ i => { (selMatrixVRS2(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(2) } } ++
//         ( 0 until rnChn    ).map{ i => { (selMatrixVRS2(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs2 }}
//       ) //index a row which all zero
//     selVRS3(chn) :=
//       Mux1H(
//         ( 0 until dptEntry ).map{ i => { (selMatrixVRS3(chn)(i).forall         ( (y: Bool) => (y === false.B) )) -> bufReqNum(i)(3) } } ++
//         ( 0 until rnChn    ).map{ i => { (selMatrixVRS3(chn)(dptEntry+i).forall( (y: Bool) => (y === false.B) )) -> io.dptReq(i).bits.phy.rs3 }}
//       ) //index a row which all zero



//     if( chn % 4 == 0 ){
//       rVOpNum(chn) := Mux( ~isVMS0NoneReq(chn), selVMS0(chn), Mux( ~isVRS1NoneReq(chn), selVRS1(chn), Mux( ~isVRS2NoneReq(chn), selVRS2(chn), Mux( ~isVRS3NoneReq(chn), selVRS3(chn), 0.U) )) )
//     } else if( chn % 4 == 1 ){
//       rVOpNum(chn) := Mux( ~isVRS1NoneReq(chn), selVRS1(chn), Mux( ~isVRS2NoneReq(chn), selVRS2(chn), Mux( ~isVRS3NoneReq(chn), selVRS3(chn), Mux( ~isVMS0NoneReq(chn), selVMS0(chn), 0.U) )) )
//     } else if( chn % 4 == 2 ){
//       rVOpNum(chn) := Mux( ~isVRS2NoneReq(chn), selVRS2(chn), Mux( ~isVRS3NoneReq(chn), selVRS3(chn), Mux( ~isVMS0NoneReq(chn), selVMS0(chn), Mux( ~isVRS1NoneReq(chn), selVRS1(chn), 0.U) )) )
//     } else if( chn % 4 == 3 ){
//       rVOpNum(chn) := Mux( ~isVRS3NoneReq(chn), selVRS3(chn), Mux( ~isVMS0NoneReq(chn), selVMS0(chn), Mux( ~isVRS1NoneReq(chn), selVRS1(chn), Mux( ~isVRS2NoneReq(chn), selVRS2(chn), 0.U) )) )
//     }


//     io.vrgReq(chn).valid := ~isVMS0NoneReq(chn) | ~isVRS1NoneReq(chn) | ~isVRS2NoneReq(chn) | ~isVRS3NoneReq(chn) 
//     io.vrgReq(chn).bits  := rVOpNum(chn)
//   }


// }

// abstract class IssueBase()(implicit p: Parameters) extends DptAgeMatrix with DptReadIOp with DptReadFOp with DptReadVOp




// trait IssLoadIOp { this: IssueBase =>
//   for( chn <- 0 until opChn ){
//     when( io.irgRsp(chn).valid ){
//       for( i <- 0 until dptEntry ){
//         for( rs <- 1 to 2 ){
//           when( bufValid(i) & (bufReqNum(i)(rs) === io.irgRsp(chn).bits.phy) & isBufXop(i)(rs) ){
//             when( isOpReady(i)(rs) === true.B ) { /*printf(s"Warning, re-request op at chn $chn, Entry $i, rs( $rs )")*/ }
//             isOpReady(i)(rs)   := true.B
//             bufOperator(i)(rs) := io.irgRsp(chn).bits.op
//           }
//         }
//       }
//     }    
//   }

// }

// trait IssLoadFOp { this: IssueBase =>
//   for( chn <- 0 until opChn ){
//     when( io.frgRsp(chn).valid ){
//       for( i <- 0 until dptEntry ){
//         for( rs <- 1 to 3 ){
//           when( bufValid(i) & (bufReqNum(i)(rs) === io.frgRsp(chn).bits.phy) & isBufFop(i)(rs) ){
//             when( isOpReady(i)(rs) === true.B ) { /*printf(s"Warning, re-request op at chn $chn, Entry $i, rs( $rs )")*/ }
//             isOpReady(i)(rs)   := true.B
//             bufOperator(i)(rs) := io.frgRsp(chn).bits.op
//           }
//         }
//       }
//     }    
//   }
// }

// trait IssLoadVOp { this: IssueBase =>
//   for( chn <- 0 until vParams.opChn ){
//     when( io.vrgRsp(chn).valid ){
//       for( i <- 0 until dptEntry ){
//         for( rs <- 0 to 3 ){
//           when( bufValid(i) & (bufReqNum(i)(rs) === io.vrgRsp(chn).bits.phy) & isBufVop(i)(rs) ){
//             when( isOpReady(i)(rs) === true.B ) { /*printf(s"Warning, re-request op at chn $chn, Entry $i, rs( $rs )")*/ }
//             isOpReady(i)(rs)   := true.B
//             bufOperator(i)(rs) := io.vrgRsp(chn).bits.op
//           }
//         }
//       }
//     }    
//   }
// }




// abstract class IssueSel()(implicit p: Parameters) extends IssueBase
// with IssLoadIOp
// with IssLoadFOp
// with IssLoadVOp{
//   val postIsOpReady = Wire( Vec( dptEntry, Vec(4, Bool())) )
//   val postBufOperator = Wire( Vec( dptEntry, Vec(4, UInt(vParams.vlen.W))) )

//   for( i <- 0 until dptEntry ){
//     postIsOpReady(i)(0)   := MuxCase( isOpReady(i)(0),                                                                                                                                                                                                            io.vrgRsp.map{x => { ( isBufVop(i)(0) & x.valid & (x.bits.phy === bufReqNum(i)(0))) -> true.B }})
//     postIsOpReady(i)(1)   := MuxCase( isOpReady(i)(1),    io.irgRsp.map{x => { ( isBufXop(i)(1) & x.valid & (x.bits.phy === bufReqNum(i)(1))) -> true.B }} ++ io.frgRsp.map{x => { ( isBufFop(i)(1) & x.valid & (x.bits.phy === bufReqNum(i)(1))) -> true.B }} ++ io.vrgRsp.map{x => { ( isBufVop(i)(1) & x.valid & (x.bits.phy === bufReqNum(i)(1))) -> true.B }})
//     postIsOpReady(i)(2)   := MuxCase( isOpReady(i)(2),    io.irgRsp.map{x => { ( isBufXop(i)(2) & x.valid & (x.bits.phy === bufReqNum(i)(2))) -> true.B }} ++ io.frgRsp.map{x => { ( isBufFop(i)(2) & x.valid & (x.bits.phy === bufReqNum(i)(2))) -> true.B }} ++ io.vrgRsp.map{x => { ( isBufVop(i)(2) & x.valid & (x.bits.phy === bufReqNum(i)(2))) -> true.B }})
//     postIsOpReady(i)(3)   := MuxCase( isOpReady(i)(3),                                                                                                        io.frgRsp.map{x => { ( isBufFop(i)(3) & x.valid & (x.bits.phy === bufReqNum(i)(3))) -> true.B }} ++ io.vrgRsp.map{x => { ( isBufVop(i)(3) & x.valid & (x.bits.phy === bufReqNum(i)(3))) -> true.B }})

//     postBufOperator(i)(0) := MuxCase( bufOperator(i)(0) ,                                                                                                                                                                                                               io.vrgRsp.map{x => { ( isBufVop(i)(0) & x.valid & (x.bits.phy === bufReqNum(i)(0))) -> x.bits.op }})
//     postBufOperator(i)(1) := MuxCase( bufOperator(i)(1) , io.irgRsp.map{x => { ( isBufXop(i)(1) & x.valid & (x.bits.phy === bufReqNum(i)(1))) -> x.bits.op }} ++ io.frgRsp.map{x => { ( isBufFop(i)(1) & x.valid & (x.bits.phy === bufReqNum(i)(1))) -> x.bits.op }} ++ io.vrgRsp.map{x => { ( isBufVop(i)(1) & x.valid & (x.bits.phy === bufReqNum(i)(1))) -> x.bits.op }})
//     postBufOperator(i)(2) := MuxCase( bufOperator(i)(2) , io.irgRsp.map{x => { ( isBufXop(i)(2) & x.valid & (x.bits.phy === bufReqNum(i)(2))) -> x.bits.op }} ++ io.frgRsp.map{x => { ( isBufFop(i)(2) & x.valid & (x.bits.phy === bufReqNum(i)(2))) -> x.bits.op }} ++ io.vrgRsp.map{x => { ( isBufVop(i)(2) & x.valid & (x.bits.phy === bufReqNum(i)(2))) -> x.bits.op }})
//     postBufOperator(i)(3) := MuxCase( bufOperator(i)(3) ,                                                                                                        io.frgRsp.map{x => { ( isBufFop(i)(3) & x.valid & (x.bits.phy === bufReqNum(i)(3))) -> x.bits.op }} ++ io.vrgRsp.map{x => { ( isBufVop(i)(3) & x.valid & (x.bits.phy === bufReqNum(i)(3))) -> x.bits.op }})
//   }

// }

// trait IssSelAlu{ this: IssueSel =>


//   def Pkg_alu_iss(idx: Int): Alu_iss_info = {

//     val res = Wire(new Alu_iss_info)
//     val src1 = postBufOperator(idx)(1)
//     val src2 = postBufOperator(idx)(2)
//     res.fun.add := bufInfo(idx).alu_isa.is_fun_add
//     res.fun.slt := bufInfo(idx).alu_isa.is_fun_slt
//     res.fun.xor := bufInfo(idx).alu_isa.is_fun_xor
//     res.fun.or  := bufInfo(idx).alu_isa.is_fun_or
//     res.fun.and := bufInfo(idx).alu_isa.is_fun_and
//     res.fun.sll := bufInfo(idx).alu_isa.is_fun_sll
//     res.fun.srl := bufInfo(idx).alu_isa.is_fun_srl
//     res.fun.sra := bufInfo(idx).alu_isa.is_fun_sra

//     res.param.is_32w  := bufInfo(idx).alu_isa.is_32w
//     res.param.is_usi  := bufInfo(idx).alu_isa.is_usi


//     res.param.dat.op1 :=
//       Mux1H(Seq(
//         bufInfo(idx).alu_isa.lui    -> 0.U,  bufInfo(idx).alu_isa.auipc  -> extVaddr(bufInfo(idx).param.pc, vlen),
//         bufInfo(idx).alu_isa.addi   -> src1, bufInfo(idx).alu_isa.addiw  -> src1,
//         bufInfo(idx).alu_isa.slti   -> src1, bufInfo(idx).alu_isa.sltiu  -> src1,
//         bufInfo(idx).alu_isa.xori   -> src1, bufInfo(idx).alu_isa.ori    -> src1,
//         bufInfo(idx).alu_isa.andi   -> src1, bufInfo(idx).alu_isa.slli   -> src1,
//         bufInfo(idx).alu_isa.slliw  -> src1, bufInfo(idx).alu_isa.srli   -> src1,
//         bufInfo(idx).alu_isa.srliw  -> src1, bufInfo(idx).alu_isa.srai   -> src1,
//         bufInfo(idx).alu_isa.sraiw  -> src1, bufInfo(idx).alu_isa.add    -> src1,
//         bufInfo(idx).alu_isa.addw   -> src1, bufInfo(idx).alu_isa.sub    -> src1,
//         bufInfo(idx).alu_isa.subw   -> src1, bufInfo(idx).alu_isa.sll    -> src1,
//         bufInfo(idx).alu_isa.sllw   -> src1, bufInfo(idx).alu_isa.slt    -> src1,
//         bufInfo(idx).alu_isa.sltu   -> src1, bufInfo(idx).alu_isa.xor    -> src1,
//         bufInfo(idx).alu_isa.srl    -> src1, bufInfo(idx).alu_isa.srlw   -> src1,
//         bufInfo(idx).alu_isa.sra    -> src1, bufInfo(idx).alu_isa.sraw   -> src1,
//         bufInfo(idx).alu_isa.or     -> src1, bufInfo(idx).alu_isa.and    -> src1,

//         bufInfo(idx).alu_isa.wfi    -> 0.U,

//     ))

//     res.param.dat.op2 :=
//       Mux1H(Seq(
//         bufInfo(idx).alu_isa.lui    -> bufInfo(idx).param.imm,      bufInfo(idx).alu_isa.auipc  -> bufInfo(idx).param.imm,
//         bufInfo(idx).alu_isa.addi   -> bufInfo(idx).param.imm,      bufInfo(idx).alu_isa.addiw  -> bufInfo(idx).param.imm,
//         bufInfo(idx).alu_isa.slti   -> bufInfo(idx).param.imm,      bufInfo(idx).alu_isa.sltiu  -> bufInfo(idx).param.imm,
//         bufInfo(idx).alu_isa.xori   -> bufInfo(idx).param.imm,      bufInfo(idx).alu_isa.ori    -> bufInfo(idx).param.imm,
//         bufInfo(idx).alu_isa.andi   -> bufInfo(idx).param.imm,      bufInfo(idx).alu_isa.slli   -> bufInfo(idx).param.imm(5,0),
//         bufInfo(idx).alu_isa.slliw  -> bufInfo(idx).param.imm(5,0), bufInfo(idx).alu_isa.srli   -> bufInfo(idx).param.imm(5,0),
//         bufInfo(idx).alu_isa.srliw  -> bufInfo(idx).param.imm(5,0), bufInfo(idx).alu_isa.srai   -> bufInfo(idx).param.imm(5,0),
//         bufInfo(idx).alu_isa.sraiw  -> bufInfo(idx).param.imm(5,0), bufInfo(idx).alu_isa.add    -> src2,
//         bufInfo(idx).alu_isa.addw   -> src2,                        bufInfo(idx).alu_isa.sub    -> (~src2 + 1.U),
//         bufInfo(idx).alu_isa.subw   -> (~src2 + 1.U),               bufInfo(idx).alu_isa.sll    -> src2,
//         bufInfo(idx).alu_isa.sllw   -> src2,                        bufInfo(idx).alu_isa.slt    -> src2,
//         bufInfo(idx).alu_isa.sltu   -> src2,                        bufInfo(idx).alu_isa.xor    -> src2,
//         bufInfo(idx).alu_isa.srl    -> src2,                        bufInfo(idx).alu_isa.srlw   -> src2,
//         bufInfo(idx).alu_isa.sra    -> src2,                        bufInfo(idx).alu_isa.sraw   -> src2,
//         bufInfo(idx).alu_isa.or     -> src2,                        bufInfo(idx).alu_isa.and    -> src2,

//         bufInfo(idx).alu_isa.wfi    -> 0.U
//     ))
//     res.param.dat.op0 := 0.U
//     res.param.dat.op3 := 0.U

//     res.param.rd0 := bufInfo(idx).phy.rd0
//     return res
//   }


//   val aluIssIdx = Wire( Vec( aluNum, UInt((log2Ceil(dptEntry)).W) ) )
//   val aluIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_alu_iss(i) }
//   val aluIssFifo = for( _ <- 0 until aluNum ) yield {
//     Module(new Queue( new Alu_iss_info, ( if(!isMinArea) 4 else 1 ), flow = true ))
//   }

//   val aluIssMatrix   = Wire( Vec(aluNum, Vec( dptEntry, Vec(dptEntry, Bool() ) )) )
//   val maskCondAluIss = Wire( Vec(aluNum, Vec( dptEntry, Bool()) ))


//   for( iss <- 0 until aluNum ){
//     if( iss == 0 ) {
//       for( i <- 0 until dptEntry ) {
//         maskCondAluIss(iss)(i) := 
//           ~bufValid(i) |
//           ~bufInfo(i).alu_isa.is_alu |
//           ~(postIsOpReady(i)(1) & postIsOpReady(i)(2))
//       }
//     } else {
//       for( i <- 0 until dptEntry ) {
//         maskCondAluIss(iss)(i) := maskCondAluIss(iss-1)(i) | (i.U === aluIssIdx(iss-1))
//       }
//     }
//     aluIssMatrix(iss) := MatrixMask( ageMatrixR, maskCondAluIss(iss) )
    
//     assert(
//       aluIssMatrix(iss).forall( (x: Vec[Bool]) => x.forall{(y: Bool) => (y === true.B)}  ) |
//       PopCount( aluIssMatrix(iss).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
//     )
//   }
  
//   for( iss <- 0 until aluNum ){
//     aluIssFifo(iss).io.enq.valid := ~aluIssMatrix(iss).forall{ (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) }} //all ture
//     aluIssIdx(iss) := aluIssMatrix(iss).indexWhere( (x: Vec[Bool]) => x.forall( y => (y === false.B) ) ) //index a row which all zero
//     aluIssFifo(iss).io.enq.bits  := 
//       Mux1H( ( 0 until dptEntry ).map{ i => { (aluIssMatrix(iss)(i).forall( (y: Bool) => ( y === false.B ) )) -> aluIssInfo(i) } } )

//     for( i <- 0 until dptEntry ) {
//       when( aluIssFifo(iss).io.enq.fire & aluIssIdx(iss) === i.U ) {
//         bufValid(i) := false.B
//         assert( postIsOpReady(i)(1) & postIsOpReady(i)(2) )
//         assert( bufValid(i) )
//         assert( bufInfo(i).alu_isa.is_alu )
//       }
//     }
//     aluIssFifo(iss).io.deq <> io.alu_iss_exe(iss)
//     aluIssFifo(iss).reset := io.flush | reset.asBool
//   }
// }


// trait IssSelMul{ this: IssueSel =>

//   def Pkg_mul_iss(idx: Int): Mul_iss_info = {
//     val res = Wire(new Mul_iss_info)
//     res.fun := bufInfo(idx).mul_isa

//     res.param.dat.op0 := 0.U
//     res.param.dat.op1 := postBufOperator(idx)(1)
//     res.param.dat.op2 := postBufOperator(idx)(2)
//     res.param.dat.op3 := 0.U

//     res.param.rd0     := bufInfo(idx).phy.rd0
//     return res
//   }

//   if ( mulNum != 0 ) {
//     val mulIssIdx  = Wire( Vec(mulNum, UInt((log2Ceil(dptEntry)).W) )  )
//     val mulIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_mul_iss(i) }
//     val mulIssFifo = for( _ <- 0 until mulNum ) yield {
//       Module(new Queue( new Mul_iss_info, ( if(!isMinArea) 4 else 1 ), flow = true ))
//     }

//     val mulIssMatrix   = Wire( Vec(mulNum, Vec( dptEntry, Vec(dptEntry, Bool() ) )) )
//     val maskCondMulIss = Wire( Vec(mulNum, Vec( dptEntry, Bool()) ))

//     for( iss <- 0 until mulNum ){
//       if( iss == 0 ) {  
//         for( i <- 0 until dptEntry ) {
//           maskCondMulIss(iss)(i) := 
//             ~bufValid(i) |
//             ~bufInfo(i).mul_isa.is_mulDiv |
//             ~(postIsOpReady(i)(1) & postIsOpReady(i)(2))
//         }
//       } else {
//         for( i <- 0 until dptEntry ) {
//           maskCondMulIss(iss)(i) := maskCondMulIss(iss-1)(i) | (i.U === mulIssIdx(iss-1))
//         }
//       }
//       mulIssMatrix(iss) := MatrixMask( ageMatrixR, maskCondMulIss(iss) )
      
//       assert(
//         mulIssMatrix(iss).forall( (x: Vec[Bool]) => x.forall{ (y: Bool) => (y === true.B) } ) |
//         PopCount( mulIssMatrix(iss).map{(x: Vec[Bool]) => x.forall{(y: Bool) => (y === false.B)} } ) === 1.U
//       )
//     }
    
//     for( iss <- 0 until mulNum ){
//       mulIssFifo(iss).io.enq.valid := ~mulIssMatrix(iss).forall{(x: Vec[Bool]) => x.forall{ (y: Bool) => y === true.B }} //all ture
//       mulIssIdx(iss) := mulIssMatrix(iss).indexWhere( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === false.B) ) ) //index a row which all zero
//       mulIssFifo(iss).io.enq.bits  := 
//         Mux1H( ( 0 until dptEntry ).map{ i => { (mulIssMatrix(iss)(i).forall( (y: Bool) => ( y === false.B ) )) -> mulIssInfo(i) } } )

//       for( i <- 0 until dptEntry ) {
//         when( mulIssFifo(iss).io.enq.fire & mulIssIdx(iss) === i.U ) {
//           bufValid(i) := false.B
//           assert( postIsOpReady(i)(1) & postIsOpReady(i)(2) )
//           assert( bufValid(i) )
//           assert( bufInfo(i).mul_isa.is_mulDiv )
//         }
//       }
//       mulIssFifo(iss).io.deq <> io.mul_iss_exe(iss)
//       mulIssFifo(iss).reset := io.flush | reset.asBool
//     }

//   } else {
//     io.mul_iss_exe(0).valid := false.B
//     io.mul_iss_exe(0).bits  := DontCare
//   }



// }

// trait IssSelBru{ this: IssueSel =>

//   def Pkg_bru_iss(idx: Int): Bru_iss_info = {
//     val res = Wire(new Bru_iss_info)
//     res.fun           := bufInfo(idx).bru_isa
//     res.param.is_rvc  := bufInfo(idx).param.is_rvc
//     res.param.pc      := extVaddr(bufInfo(idx).param.pc, vlen)
//     res.param.imm     := bufInfo(idx).param.imm

//     res.param.dat.op0 := 0.U
//     res.param.dat.op1 := postBufOperator(idx)(1)
//     res.param.dat.op2 := postBufOperator(idx)(2)
//     res.param.dat.op3 := 0.U

//     res.param.rd0     := bufInfo(idx).phy.rd0

//     return res
//   }

//   val bruIssIdx = Wire( UInt((log2Ceil(dptEntry)).W) )
//   val bruIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_bru_iss(i) }
//   val bruIssFifo = Module(new Queue( new Bru_iss_info, ( if(!isMinArea) 4 else 1 ), flow = true ) )

//   val bruIssMatrix   = Wire( Vec( dptEntry, Vec(dptEntry, Bool() ) )) 
//   val maskCondBruIss = Wire( Vec( dptEntry, Bool()) )

//   //only oldest instr will be selected
//   for( i <- 0 until dptEntry ) {
//     maskCondBruIss(i) := 
//       ~bufValid(i) |
//       ~bufInfo(i).bru_isa.is_bru
//   }

//   bruIssMatrix := MatrixMask( ageMatrixR, maskCondBruIss )
  
//   assert(
//     bruIssMatrix.forall( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === true.B) ) ) |
//     PopCount( bruIssMatrix.map{(x: Vec[Bool]) => x.forall((y: Bool) => (y === false.B))} ) === 1.U
//   )

//   bruIssIdx := bruIssMatrix.indexWhere( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === false.B) ) ) //index a row which all zero

//   bruIssFifo.io.enq.valid := 
//     ( 0 until dptEntry ).map{ i => { bruIssMatrix(i).forall( (x: Bool) => (x === false.B) ) & postIsOpReady(i)(1) & postIsOpReady(i)(2) } }.reduce(_|_)

//   bruIssFifo.io.enq.bits  := 
//     Mux1H( ( 0 until dptEntry ).map{ i => { (bruIssMatrix(i).forall( (y: Bool) => ( y === false.B ) ) & postIsOpReady(i)(1) & postIsOpReady(i)(2) ) -> bruIssInfo(i) } } )

//   for( i <- 0 until dptEntry ) {
//     when( bruIssFifo.io.enq.fire & bruIssIdx === i.U ) {
//       bufValid(i) := false.B
//       assert( postIsOpReady(i)(1) & postIsOpReady(i)(2) )
//       assert( bufValid(i) )
//       assert( bufInfo(i).bru_isa.is_bru )
//     }
//   }

//   bruIssFifo.io.deq <> io.bru_iss_exe
//   bruIssFifo.reset := io.flush | reset.asBool

// }

// trait IssSelCsr{ this: IssueSel =>

//   def Pkg_csr_iss(idx: Int): Csr_iss_info = {
//     val res = Wire(new Csr_iss_info)
//     val csrSel = bufInfo(idx).param.imm(11,0)

//     res.fun.rc  := bufInfo(idx).csr_isa.rc | bufInfo(idx).csr_isa.rci
//     res.fun.rs  := bufInfo(idx).csr_isa.rs | bufInfo(idx).csr_isa.rsi
//     res.fun.rw  := bufInfo(idx).csr_isa.rw | bufInfo(idx).csr_isa.rwi

//     res.param.dat.op1 := 
//       Mux1H(Seq(
//         bufInfo(idx).csr_isa.rw  -> postBufOperator(idx)(1), bufInfo(idx).csr_isa.rwi -> bufInfo(idx).param.raw.rs1,
//         bufInfo(idx).csr_isa.rs  -> postBufOperator(idx)(1), bufInfo(idx).csr_isa.rsi -> bufInfo(idx).param.raw.rs1,
//         bufInfo(idx).csr_isa.rc  -> postBufOperator(idx)(1), bufInfo(idx).csr_isa.rci -> bufInfo(idx).param.raw.rs1,
//       ))

//     /** automatically create csr address mux according CSRInfoTable */
//     res.param.dat.op2 := 
//       Mux1H(
//         (for ( info <- CSRInfoTable.CSRGroup; io <- io.csrfiles.elements; if( (info.name == io._1) ) ) yield {
//           (csrSel === (info.address).U) -> ( io._2.asUInt )
//         }) ++

//         (for( i <- 0 until pmpNum by 2 ) yield{
//           ((csrSel === ("h3A0".U + i.U)) -> io.csrfiles.pmpcfg(i).asUInt)
//         }) ++

//         (for( i <- 0 until pmpNum*8 ) yield{
//           ((csrSel === ("h3B0".U + i.U)) -> io.csrfiles.pmpaddr(i).asUInt)
//         }) ++

//         (for( i <- 3 until 32 ) yield{
//           ((csrSel === ("hB00".U + i.U)) -> io.csrfiles.mhpmcounter(i))
//         }) ++

//         (for( i <- 3 until 32 ) yield{
//           ((csrSel === ("h320".U + i.U)) -> io.csrfiles.mhpmevent(i))
//         }) ++ 

//         (for( i <- 3 until 32 ) yield{
//           ((csrSel === ("hC00".U + i.U)) -> io.csrfiles.hpmcounter(i))
//         }) 
//       )

//     res.param.dat.op0 := 0.U
//     res.param.dat.op3 := bufInfo(idx).param.imm(11,0)

//     res.param.rd0     := bufInfo(idx).phy.rd0

//     return res
//   }

//   val csrIssIdx = Wire( UInt((log2Ceil(dptEntry)).W) )
//   val csrIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_csr_iss(i) }
//   val csrIssFifo = Module(new Queue( new Csr_iss_info, ( if(!isMinArea) 4 else 1 ), flow = true ) )

//   val csrIssMatrix   = Wire( Vec( dptEntry, Vec(dptEntry, Bool() ) )) 
//   val maskCondCsrIss = Wire( Vec( dptEntry, Bool()) )

//   /** only oldest instr will be selected, donot bypass un-ready operotar */
//   for( i <- 0 until dptEntry ) {
//     maskCondCsrIss(i) := 
//       ~bufValid(i) |
//       (~bufInfo(i).csr_isa.is_csr) 
//   }

//   csrIssMatrix := MatrixMask( ageMatrixR, maskCondCsrIss )
  
//   assert(
//     csrIssMatrix.forall( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === true.B) ) ) |
//     PopCount( csrIssMatrix.map{(x: Vec[Bool]) => x.forall((y: Bool) => (y === false.B))} ) === 1.U
//   )

//   csrIssIdx := csrIssMatrix.indexWhere( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === false.B) ) ) //index a row which all zero

//   csrIssFifo.io.enq.valid := 
//     ( 0 until dptEntry ).map{ i => { csrIssMatrix(i).forall( (x: Bool) => (x === false.B) ) & postIsOpReady(i)(1) & postIsOpReady(i)(2) } }.reduce(_|_)

//   csrIssFifo.io.enq.bits  := 
//     Mux1H( ( 0 until dptEntry ).map{ i => { (csrIssMatrix(i).forall( (y: Bool) => ( y === false.B ) ) & postIsOpReady(i)(1) & postIsOpReady(i)(2) ) -> csrIssInfo(i) } } )

//   for( i <- 0 until dptEntry ) {
//     when( csrIssFifo.io.enq.fire & csrIssIdx === i.U ) {
//       bufValid(i) := false.B
//       assert( postIsOpReady(i)(1) & postIsOpReady(i)(2) )
//       assert( bufValid(i) )
//       assert( bufInfo(i).csr_isa.is_csr )
//     }
//   }

//   csrIssFifo.io.deq <> io.csr_iss_exe
//   csrIssFifo.reset := io.flush | reset.asBool
// }

// trait IssSelLsu{ this: IssueSel =>

//   def Pkg_lsu_iss( idx: Int ): Lsu_iss_info = {
//     val res = Wire(new Lsu_iss_info)

//     res.fun := bufInfo(idx).lsu_isa

//     res.param.dat.op0 := Mux( bufInfo(idx).lsu_isa.is_vls, postBufOperator(idx)(0), 0.U )

//     res.param.dat.op1 := 
//       MuxCase( (postBufOperator(idx)(1).asSInt + bufInfo(idx).param.imm.asSInt()).asUInt(), Seq(
//         (bufInfo(idx).lsu_isa.is_lrsc | bufInfo(idx).lsu_isa.is_amo) -> postBufOperator(idx)(1),

//       ))
//     res.param.dat.op2 :=
//       MuxCase( postBufOperator(idx)(2), Seq(
//         bufInfo(idx).lsu_isa.isFStore    -> ieee(unbox(postBufOperator(idx)(2), 1.U, None), t = FType.D),
//         bufInfo(idx).lsu_isa.isVConstant -> postBufOperator(idx)(2),//rs2
//         bufInfo(idx).lsu_isa.isVIndex    -> postBufOperator(idx)(2),//vs2
//       ))

//     res.param.dat.op3 := 
//       Mux( bufInfo(idx).lsu_isa.isVStore, postBufOperator(idx)(3), 0.U )


//     res.param.rd0 := bufInfo(idx).phy.rd0


//     if( hasVector ){
 
//       res.vAttach.get.vWidth   := bufInfo(idx).param.vWidth
//       res.vAttach.get.nf       := bufInfo(idx).vAttach.get.nf
//       res.vAttach.get.vm       := bufInfo(idx).vAttach.get.vm
//       // res.vAttach.get.isFoF    := bufInfo(idx).param.raw.rs2.extract(4).asBool
//       res.vAttach.get.bufIdx := DontCare
//       res.vAttach.get.eleIdx := DontCare

//       res.vAttach.get.lmulSel   := bufInfo(idx).vAttach.get.lmulSel
//       res.vAttach.get.nfSel     := bufInfo(idx).vAttach.get.nfSel
//       res.vAttach.get.widenSel  := bufInfo(idx).vAttach.get.widenSel
//       res.vAttach.get.vstartSel := bufInfo(idx).vAttach.get.vstartSel
//       res.vAttach.get.isLast    := bufInfo(idx).vAttach.get.isLast


//       res.vAttach.get.vstart := bufInfo(idx).vAttach.get.vstart
//       res.vAttach.get.vtype  := bufInfo(idx).vAttach.get.vtype
//       res.vAttach.get.vl     := bufInfo(idx).vAttach.get.vl

//       res.vAttach.get.nfSel   := bufInfo(idx).vAttach.get.nfSel
//       res.vAttach.get.lmulSel := bufInfo(idx).vAttach.get.lmulSel

//     }
//     return res
//   }
//   val lsuIssIdx = Wire( UInt((log2Ceil(dptEntry)).W) )
//   val lsuIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_lsu_iss(i) }
//   val lsuIssFifo = Module(new Queue( new Lsu_iss_info, ( if(!isMinArea) 4 else 1 ), flow = true ) )

//   val lsuIssMatrix   = Wire( Vec( dptEntry, Vec(dptEntry, Bool() ) )) 
//   val maskCondLsuIss = Wire( Vec( dptEntry, Bool()) )

//   //only oldest instr will be selected, in-order-issue
//   for( i <- 0 until dptEntry ) {
//     maskCondLsuIss(i) := 
//       ~bufValid(i) |
//       ~bufInfo(i).lsu_isa.is_lsu
//   }

//   lsuIssMatrix := MatrixMask( ageMatrixR, maskCondLsuIss )
  
//   assert(
//     lsuIssMatrix.forall( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === true.B) ) ) |
//     PopCount( lsuIssMatrix.map{(x: Vec[Bool]) => x.forall((y: Bool) => (y === false.B))} ) === 1.U
//   )

//   lsuIssIdx := lsuIssMatrix.indexWhere( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === false.B) ) ) //index a row which all zero

//   lsuIssFifo.io.enq.valid := 
//     ( 0 until dptEntry ).map{ i => { lsuIssMatrix(i).forall( (x: Bool) => (x === false.B) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) & postIsOpReady(i)(2) & postIsOpReady(i)(3) } }.reduce(_|_)

//   lsuIssFifo.io.enq.bits  := 
//     Mux1H( ( 0 until dptEntry ).map{ i => { (lsuIssMatrix(i).forall( (y: Bool) => ( y === false.B ) ) & postIsOpReady(i)(0) & postIsOpReady(i)(1) & postIsOpReady(i)(2) & postIsOpReady(i)(3) ) -> lsuIssInfo(i) } } )

//   for( i <- 0 until dptEntry ) {
//     when( lsuIssFifo.io.enq.fire & lsuIssIdx === i.U ) {
//       bufValid(i) := false.B
//       assert( postIsOpReady(i)(0) & postIsOpReady(i)(1) & postIsOpReady(i)(2) & postIsOpReady(i)(3) )
//       assert( bufValid(i) )
//       assert( bufInfo(i).lsu_isa.is_lsu )
//     }
//   }

//   lsuIssFifo.io.deq <> io.lsu_iss_exe
//   lsuIssFifo.reset := io.flush | reset.asBool
// }

// trait IssSelFpu{ this: IssueSel =>

//   require( fpuNum <= 1, s"fpu is not support out-of-order in this Vesrion!" )

//   def Pkg_fpu_iss(idx: Int): Fpu_iss_info = {
//     val res = Wire(new Fpu_iss_info)

//     res.fun := bufInfo(idx).fpu_isa

//     res.param.dat.op0 := io.csrfiles.fcsr.frm
//     res.param.dat.op1 := postBufOperator(idx)(1)
//     res.param.dat.op2 := postBufOperator(idx)(2)
//     res.param.dat.op3 := postBufOperator(idx)(3)
    
//     res.param.rd0 := bufInfo(idx).phy.rd0
//     res.param.rm := bufInfo(idx).param.rm

//     return res
//   }


//   if( fpuNum != 0 ){
//     val fpuIssIdx = Wire( Vec( fpuNum, UInt((log2Ceil(dptEntry)).W) ))
//     val fpuIssInfo = for( i <- 0 until dptEntry ) yield { Pkg_fpu_iss(i) }
//     val fpuIssFifo = Module(new Queue( new Fpu_iss_info, ( if(!isMinArea) 4 else 1 ), flow = true))

//     val fpuIssMatrix   = Wire( Vec(fpuNum, Vec( dptEntry, Vec(dptEntry, Bool() ) )) )
//     val maskCondFpuIss = Wire( Vec(fpuNum, Vec( dptEntry, Bool()) ))

//     for( i <- 0 until dptEntry ) {
//       maskCondFpuIss(0)(i) := 
//         ~bufValid(i) |
//         ~bufInfo(i).fpu_isa.is_fpu
//     }

//     fpuIssMatrix(0) := MatrixMask( ageMatrixR, maskCondFpuIss(0) )
    
//     assert(
//       fpuIssMatrix(0).forall( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === true.B) ) ) |
//       PopCount( fpuIssMatrix(0).map{(x: Vec[Bool]) => x.forall((y: Bool) => (y === false.B))} ) === 1.U
//     )

//     fpuIssFifo.io.enq.valid := 
//       ( 0 until dptEntry ).map{ i => { fpuIssMatrix(0)(i).forall( (x: Bool) => (x === false.B) ) & postIsOpReady(i)(1) & postIsOpReady(i)(2) & postIsOpReady(i)(3) } }.reduce(_|_)

//     fpuIssFifo.io.enq.bits  := 
//       Mux1H( ( 0 until dptEntry ).map{ i => { (fpuIssMatrix(0)(i).forall( (y: Bool) => ( y === false.B ) ) & postIsOpReady(i)(1) & postIsOpReady(i)(2) & postIsOpReady(i)(3)  ) -> fpuIssInfo(i) } } )
    
//     fpuIssIdx(0) := fpuIssMatrix(0).indexWhere( (x: Vec[Bool]) => x.forall( (y: Bool) => (y === false.B) ) ) //index a row which all zero

//     for( i <- 0 until dptEntry ) {
//       when( fpuIssFifo.io.enq.fire & fpuIssIdx(0) === i.U ) {
//         bufValid(i) := false.B
//         assert( postIsOpReady(i)(1) & postIsOpReady(i)(2) & postIsOpReady(i)(3) )
//         assert( bufValid(i) )
//         assert( bufInfo(i).fpu_isa.is_fpu )
//       }
//     }

//     fpuIssFifo.io.deq <> io.fpu_iss_exe(0)
//     fpuIssFifo.reset := io.flush | reset.asBool
//   } else {
//     io.fpu_iss_exe(0).valid := false.B
//     io.fpu_iss_exe(0).bits  := DontCare
//   }

// }

// trait IssSelVpu{ this: IssueSel =>

// }


// class Issue()(implicit p: Parameters) extends IssueSel
// with IssSelAlu
// with IssSelMul
// with IssSelBru
// with IssSelCsr
// with IssSelLsu
// with IssSelFpu
// with IssSelVpu{
  
// }