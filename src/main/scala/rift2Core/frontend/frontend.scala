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
import rift._
import chipsalliance.rocketchip.config.Parameters

case class IFParameters(
  btb_cl: Int = 4096,
  bim_cl: Int = 4096,
  ras_dp: Int = 256,
  tage_table: Int = 6, 
)

trait HasIFParameters extends HasRiftParameters {
  val ifParams: IFParameters

  def btb_cl: Int = ifParams.btb_cl
  def bim_cl: Int = ifParams.bim_cl
  def ras_dp: Int = ifParams.ras_dp
  def tage_table: Int = ifParams.tage_table
}

abstract class IFetchModule(implicit val p: Parameters) extends Module with HasIFParameters { def io: Record }
abstract class IFetchBundle(implicit val p: Parameters) extends Bundle with HasIFParameters

class Ghist_reflash_Bundle extends IFetchBundle {
  val isTaken = Bool()
}

class IF4_Redirect_Bundle extends IFetchBundle {
  val pc = UInt(64.W)
}

class RASPP_Bundle extends IFetchBundle {
  val target = UInt(64.W)
}




class IF1_Bundle extends IFetchBundle {
  val pc = UInt(64.W)
  // val BHR  = UInt(64.W)
}

class IF2_Bundle extends IFetchBundle {
  val pc    = UInt(64.W)
  val instr = UInt(16.W)
  // val BHR  = UInt(64.W)
}

class PreDecode_Bundle extends Bundle {
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

class BIMReq_Bundle extends IFetchBundle {
  val pc = UInt(64.W)
}

class BIMResp_Bundle extends IFetchBundle {
  val bim_p = Bool()
  val bim_h = Bool()
}

class BIMUpdate_Bundle extends BIMResp_Bundle {
  val pc = UInt(64.W)
  val is_finalTaken   = Bool()

  def is_misPredict = is_finalTaken =/= bim_p
}

class BTBReq_Bundle extends IFetchBundle {
  val pc = UInt(64.W)
}

class BTBResp_Bundle extends IFetchBundle {
  val target = UInt(64.W)
}

class BTBUpdate_Bundle extends BTBResp_Bundle {
  val pc = UInt(64.W)
}

class TageTableUpdate_Bundle extends IFetchBundle {
  val use   = UInt(2.W)
  val ctl   = UInt(3.W)
  val pc    = UInt(64.W)
  val ghist = UInt(64.W)
}

class TageTableReq_Bundle extends IFetchBundle {
  val pc = UInt(64.W)
  val ghist = UInt(64.W)
}

class TageReq_Bundle extends TageTableReq_Bundle


class TageTableResp_Bundle extends IFetchBundle {
  val ctr = UInt(3.W)
  val use = UInt(2.W)
  val is_hit = Bool()

  def is_taken = (ctr(3) === 1.U).asBool
}

class TageResp_Bundle extends IFetchBundle {

  val ftq_tage = Vec( 6, new TageTableResp_Bundle )
  val is_provider = Vec( 6, Bool() )
  val is_altpred  = Vec( 6, Bool() )
  val is_predictTaken = Bool()

  def provider_sel = OHToUInt( in = is_provider.asUInt, width = 6)

  def is_agree: Seq[Bool] = {
    for ( i <- 0 until 6 ) yield {
      is_altpred(i) & ftq_tage(i).is_taken === is_predictTaken
    }
  }

  def is_disAgree: Seq[Bool] = {
    for ( i <- 0 until 6 ) yield {
      is_altpred(i) & ftq_tage(i).is_taken =/= is_predictTaken
    }
  }

  def is_alloc: Seq[Bool] = {
    for ( i <- 0 until 6 ) yield {
      ~is_altpred(i) & ~is_provider(i)
    }
  }
}

class TageUpdate_Bundle extends TageResp_Bundle {
  val pc = UInt(64.W)
  val ghist = UInt(64.W)
  val is_finalTaken = Bool()

  def is_misPredict = is_alloc.reduce(_|_) & (is_predictTaken =/= is_finalTaken)
}




class Predict_Bundle extends IFetchBundle {
  val btb = new BIMResp_Bundle
  val bim = new BIMResp_Bundle
  val tage = Vec( 6, new TageTableResp_Bundle )
}


class IF3_Bundle extends Bundle {
  val preDecode = new PreDecode_Bundle
  val predict = new Predict_Bundle
  val instr = UInt(32.W)
  val pc = UInt(64.W)
  val ghist = UInt(64.W)
}



class IF4_Bundle extends Info_instruction


class Branch_FTarget_Bundle extends RiftBundle {
  val pc = UInt(64.W)
  val ghist = UInt(64.W)
  val bimResp  = new BIMResp_Bundle
  val tageResp = new TageResp_Bundle
  val revertTarget = UInt(64.W)
  val isPredictTaken = Bool()
}

class Jump_FTarget_Bundle extends RiftBundle {
  val pc       = UInt(64.W)
  val btbResp = new BTBResp_Bundle
  val rasResp = new RASPP_Bundle

  val isRas = Bool()

  def isBtb = ~isRas
}

class Branch_CTarget_Bundle extends Branch_FTarget_Bundle {
  val isFinalTaken = Bool()
  def isMisPredict = isPredictTaken =/= isFinalTaken
}

class Jump_CTarget_Bundle extends Jump_FTarget_Bundle {
  val finalTarget = UInt(64.W)
  def isMisPredict = Mux(isRas, rasResp.target =/= finalTarget, btbResp.target =/= finalTarget )
}


