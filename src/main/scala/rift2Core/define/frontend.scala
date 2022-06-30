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

package rift2Core.define

import chisel3._
import chisel3.util._
import rift._
import chipsalliance.rocketchip.config.Parameters



case class IFParameters(
  uBTB_entry: Int = 16,
  uBTB_tag_w: Int = 8,
  btb_cl: Int = 4096,
  bim_cl: Int = 4096,
  ras_dp: Int = 256,
  tage_table: Int = 6, 
)

trait HasIFParameters extends HasRiftParameters {
  val ifParams: IFParameters

  def uBTB_entry: Int = ifParams.uBTB_entry
  def uBTB_tag_w: Int = ifParams.uBTB_tag_w
  def btb_cl: Int = ifParams.btb_cl
  def bim_cl: Int = ifParams.bim_cl
  def ras_dp: Int = ifParams.ras_dp
  def tage_table: Int = ifParams.tage_table
}

abstract class IFetchModule(implicit p: Parameters) extends RiftModule with HasIFParameters
abstract class IFetchBundle(implicit p: Parameters) extends RiftBundle with HasIFParameters

class Ghist_reflash_Bundle(implicit p: Parameters) extends IFetchBundle {
  val isTaken = Bool()
}

class IF4_Redirect_Bundle(implicit p: Parameters) extends IFetchBundle {
  val target     = UInt(vlen.W)
  val pc         = UInt(vlen.W)
  val isDisAgree = Bool()
}

class RASPP_Bundle(implicit p: Parameters) extends IFetchBundle {
  val target = UInt(vlen.W)
}




class IF1_Bundle(implicit p: Parameters) extends uBTBResp_Bundle {
  val pc = UInt(64.W)
  // val BHR  = UInt(64.W)
}

class IF2_Bundle(implicit p: Parameters) extends IFetchBundle {
  val pc    = UInt(vlen.W)
  val instr = UInt(16.W)
  // val isAccessFault = Bool()
  // val isPagingFault = Bool()
  // val BHR  = UInt(64.W)
  val isFault = Bool()
  val isRedirect = Bool()
  val target = UInt(vlen.W)
}


class PreDecode_Bundle(implicit p: Parameters) extends IFetchBundle {
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

  def is_req_btb  = is_jalr
  def is_req_ras  = is_return
  def is_req_bim  = is_branch
  def is_req_tage = is_branch
  def is_lock_pipe = is_fencei | is_sfencevma

}

class BIMReq_Bundle(implicit p: Parameters) extends IFetchBundle {
  val pc = UInt(vlen.W)
}

class BIMResp_Bundle(implicit p: Parameters) extends IFetchBundle {
  val bim_p = Bool()
  val bim_h = Bool()
}

class BIMUpdate_Bundle(implicit p: Parameters) extends BIMResp_Bundle {
  val pc = UInt(vlen.W)
  val isFinalTaken   = Bool()

  def isMisPredict = isFinalTaken =/= bim_p
}

class BTBReq_Bundle(implicit p: Parameters) extends IFetchBundle {
  val pc = UInt(vlen.W)
}



class uBTBReq_Bundle(implicit p: Parameters) extends BTBReq_Bundle

class BTBResp_Bundle(implicit p: Parameters) extends IFetchBundle {
  val target = UInt(vlen.W)
}

class uBTBResp_Bundle(implicit p: Parameters) extends BTBResp_Bundle {
  val isRedirect = Vec( 8, Bool() )
  val isActive   = Vec( 8, Bool() )
}

class BTBUpdate_Bundle(implicit p: Parameters) extends BTBResp_Bundle {
  val pc = UInt(vlen.W)
}

class uBTBUpdate_Bundle(implicit p: Parameters) extends BTBUpdate_Bundle {
  val isTaken = Bool()
}

class TageTableUpdate_Bundle(implicit p: Parameters) extends IFetchBundle {
  val use   = UInt(2.W)
  val ctl   = UInt(3.W)
  val pc    = UInt(vlen.W)
  val ghist = UInt(64.W)
}

class TageTableReq_Bundle(implicit p: Parameters) extends IFetchBundle {
  val pc = UInt(vlen.W)
  val ghist = UInt(64.W)
}

class TageReq_Bundle(implicit p: Parameters) extends TageTableReq_Bundle


class TageTableResp_Bundle(implicit p: Parameters) extends IFetchBundle {
  val ctl = UInt(3.W)
  val use = UInt(2.W)
  val is_hit = Bool()

  def isTaken = (ctl(2) === 1.U).asBool
}

class TageResp_Bundle(implicit p: Parameters) extends IFetchBundle {

  val ftqTage = Vec( 6, new TageTableResp_Bundle )
  val isProvider = Vec( 6, Bool() )
  val isAltpred  = Vec( 6, Bool() )
  val isPredictTaken = Bool()

  def providerSel = OHToUInt( in = isProvider.asUInt, width = 6)

  def isAgree: Seq[Bool] = {
    for ( i <- 0 until 6 ) yield {
      isAltpred(i) & ftqTage(i).isTaken === isPredictTaken
    }
  }

  def isDisAgree: Seq[Bool] = {
    for ( i <- 0 until 6 ) yield {
      isAltpred(i) & ftqTage(i).isTaken =/= isPredictTaken
    }
  }

  def isAlloc: Seq[Bool] = {
    for ( i <- 0 until 6 ) yield {
      ~isAltpred(i) & ~isProvider(i)
    }
  }
}

class TageUpdate_Bundle(implicit p: Parameters) extends TageResp_Bundle {
  val pc = UInt(vlen.W)
  val ghist = UInt(64.W)
  val isFinalTaken = Bool()

  def isMisPredict = isAlloc.reduce(_|_) & (isPredictTaken =/= isFinalTaken)
}






class IF3_Bundle(implicit p: Parameters) extends RiftBundle {
  val preDecode = new PreDecode_Bundle
  // val predict = new Predict_Bundle
  val instr = UInt(32.W)
  val pc = UInt(vlen.W)
  val ghist = UInt(64.W)
  val isRedirect = Bool()
  val target = UInt(vlen.W)
}



class IF4_Bundle(implicit p: Parameters) extends Info_instruction


class Branch_FTarget_Bundle(implicit p: Parameters) extends RiftBundle {
  val pc = UInt(vlen.W)
  val ghist = UInt(64.W)
  val bimResp  = new BIMResp_Bundle
  val tageResp = new TageResp_Bundle
  val isPredictTaken = Bool()
}

class Jump_FTarget_Bundle(implicit p: Parameters) extends RiftBundle {
  val pc       = UInt(vlen.W)
  val btbResp = new BTBResp_Bundle
  val rasResp = new RASPP_Bundle
  val isRas = Bool()

  def isBtb = ~isRas
}

class Branch_CTarget_Bundle(implicit p: Parameters) extends Branch_FTarget_Bundle {
  val isFinalTaken = Bool()
  val finalTarget = UInt(64.W)
  def isMisPredict = isPredictTaken =/= isFinalTaken
}

class Jump_CTarget_Bundle(implicit p: Parameters) extends Jump_FTarget_Bundle {
  val finalTarget = UInt(64.W)
  def isMisPredict = Mux(isRas, rasResp.target =/= finalTarget, btbResp.target =/= finalTarget )
}


