
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

abstract class DptBase ()(implicit p: Parameters) extends RiftModule with HasFPUParameters{
  def dptEntry = 16
  val io = IO(new Bundle{

    val dptReq = Vec(rnChn, Flipped(new DecoupledIO(new Dpt_info)))

    val alu_iss_exe = new DecoupledIO(new Alu_iss_info)
    val mul_iss_exe = new DecoupledIO(new Mul_iss_info)
    val bru_iss_exe = new DecoupledIO(new Bru_iss_info)
    val csr_iss_exe = new DecoupledIO(new Csr_iss_info)
    val lsu_iss_exe = new DecoupledIO(new Lsu_iss_info)
    val fpu_iss_exe = new DecoupledIO(new Fpu_iss_info)

    val irgLog = Input( Vec(regNum, UInt(2.W)) )
    val frgLog = Input( Vec(regNum, UInt(2.W)) )

    val irgReq = Valid( Vec( opChn, new RS_PHY ) )
    val frgReq = Valid( Vec( opChn, new RS_PHY ) )    

    val irgRsp = Flipped( Vec( opChn, new Operation_source(64) ) )
    val frgRsp = Flipped( Vec( opChn, new Operation_source(65) ) )

    // val irg_readOp  = Vec( opChn, new iss_readOp_info(dw=64))
    // val frg_readOp  = Vec( opChn, new iss_readOp_info(dw=65))

    val flush = Input(Bool())
  }
}


abstract class DptBoard()(implicit p: Parameters) extends DptBase {
  val bufValidDnxt = Wire( rnChn, Vec( dptEntry, Bool() ) )
  val bufValid     = RegInit( VecInit( Seq.fill(dptEntry){false.B} ))
  val bufInfo      = Reg(Vec( dptEntry, new Dpt_info               ))
  val isBufFop     = Reg( Vec( dptEntry, Vec( 3, Bool()    )) )
  val isBufReady   = Reg( Vec( dptEntry, Vec( 3, Bool()    )) )
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
        isBufReady(entrySel(i))(0)  := true.B
        bufOperator(entrySel(i))(0) := 0.U
      }
      when( io.dptReq(i).bits.phy.rs2 === (regNum-1).U ) {
        isBufReady(entrySel(i))(1)  := true.B
        bufOperator(entrySel(i))(1) := 0.U
      }
      when( io.dptReq(i).bits.phy.rs3 === (regNum-1).U ) {
        isBufReady(entrySel(i))(2)  := true.B
        bufOperator(entrySel(i))(2) := 0.U
      }
    }
  }

}


abstract class DptAgeMatrix()(implicit p: Parameters) extends DptBoard {

  val ageMatrixR = Wire( Vec( dptEntry, Vec(dptEntry, Bool()) ) )
  val ageMatrixW = Reg( Vec( dptEntry, Vec(dptEntry, Bool()) ) )



  for ( i <- 0 until dptEntry ) {
    for( j <- 0 until i ) {
      ageMatrixR(i)(j) := ~ageMatrixW(j)(i) & bufValid(j)
    }
    ageMatrixR(i)(i) := DontCare
    for( j <- i until dptEntry ) {
      ageMatrixR(i)(j) := ageMatrixW(i)(j) & bufValid(j)
    }
  }

  for( i <- 0 until rnChn ) {
    when( io.dptReq(i).fire ) {
      ageMatrixW(entrySel(i)).map{ x => {x := true.B} }
      ageMatrixW.map{ x => {x(entrySel(i)) := false.B} }
    }
  }


}

trait DptReadIOp { this: DptAgeMatrix =>
  
  
  io.irgLog
  io.irgReq :=

}

trait DptReadFOp { this: DptAgeMatrix =>

}






abstract class ()(implicit p: Parameters) extends DptAgeMatrix {





}



