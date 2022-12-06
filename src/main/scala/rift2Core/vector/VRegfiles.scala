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

import rift2Chip._
import chipsalliance.rocketchip.config._




abstract class VRegFilesBase(rop_chn: Int, wb_chn: Int)(implicit p: Parameters) extends RiftModule{

  def opc = rop_chn
  def wbc = wb_chn

  val io = IO( new Bundle{
    val lookup = Vec( rnChn, Flipped(new Lookup_Bundle) )
    val rename = Vec( rnChn, Flipped(new Rename_Bundle) )

    /** read operators based on idx, must success */
    // val iss_readOp = Vec(opc, Flipped( new iss_readOp_info(vlen)) )
    val rgLog = Output( Vec(vRegNum, UInt(2.W)) )
    val rgReq = Flipped(Vec( opc, Valid( UInt((log2Ceil(vRegNum)).W) ) ))
    val rgRsp =         Vec( opc, Valid(new ReadOp_Rsp_Bundle(vlen) ))


    /** writeBack request from exeUnit */
    val exe_writeBack = Vec(wbc, Flipped(new DecoupledIO(new WriteBack_info(dw))))
    /** Commit request from commitUnit */
    val commit = Vec(cm_chn, Flipped(new Info_commit_op))

    val diffReg = Output(Vec(32, UInt(dw.W)))
  })
}

abstract class VRegFilesReal(rop_chn: Int, wb_chn: Int)(implicit p: Parameters) extends RegFilesBase( rop_chn, wb_chn){

  val raw = io.commit.map{ x => x.raw }
  val phy = io.commit.map{ x => x.phy }





  /** there are (vRegNum * atNum * atw) vFiles */
  val vFiles = RegInit( VecInit( Seq.fill(vParams.vRegNum){ VecInit( Seq.fill(vParams.atNum){ UInt(vParams.atw.W) } ) } ))
  require( vParams.atw * vParams.atNum == vParams.vlen )

  /** index that 32 * vParams.atNum renamed register-sources point to */
  val vRenamePtr = RegInit( VecInit( for( i <- 0 until 32*vParams.atNum ) yield {i.U(log2Ceil(vParams.vRegNum * vParams.atNum).W)} ) )

  /** index that 32 * vParams.atNum commited register-sources point to */  
  val vArchitPtr = 
  WireDefault( VecInit( Seq.fill(1 * vParams.atNum){0.U(2.W)} )) ++
  RegInit( VecInit( for( i <- 1 until 32*vParams.atNum ) yield {i.U(log2Ceil(vParams.vRegNum * vParams.atNum).W)} ) )
  
  assert( vArchitPtr(0) === 0.U, s"Assert Failed at VRegfiles, v0 didn't implement here, its architPtr should always keep 0.U!" )

  /** there are regNum * vParams.atNum log */ 
  val vLog = RegInit(VecInit( 
    Seq.fill(          1  * vParams.atNum )( 0.U(2.W) ) ++    //just align at initalization
    Seq.fill(         31  * vParams.atNum )( "b11".U(2.W) ) ++ 
    Seq.fill( (regNum-32) * vParams.atNum )( 0.U(2.W) )
  ))

}

