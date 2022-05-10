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

case class IFParameters(
  // GHR_length: Int,
  // UBTB_entry: Int,
)

trait HasIFParameters extends HasRiftParameters {
  val ifParams: IFParameters

  // def GHR_length = ifParams.GHR_length
  // def UBTB_entry = ifParams.UBTB_entry
  // val fetch_instr = ifParams.fetch_w/16
}

abstract class IFetchModule(implicit val p: Parameters) extends Module with HasIFParameters { def io: Record }
abstract class IFetchBundle(implicit val p: Parameters) extends Bundle with HasIFParameters


class IF1_Bundle extends IFetchBundle {
  val pc = UInt(64.W)
  // val BHR  = UInt(64.W)
}

class IF2_Bundle extends IFetchBundle {
  val pc    = UInt(64.W)
  val instr = UInt(16.W)
  // val BHR  = UInt(64.W)
}

class IF3_Bundle extends Bundle {
  val info = new Info_preDecode
  val instr = UInt(32.W)
  val pc = UInt(64.W)
}

class BIMReq_Bundle extends IFetchBundle {
  val pc = UInt(64.W)
}

class BIMResp_Bundle extends IFetchBundle {
  val RevertTarget = UInt(64.W)
  val is_taken     = Bool()
}

class BIMUpdate_Bundle extends BIMReq_Bundle {
  val is_finalTaken = Bool()
}

class BTBReq_Bundle extends IFetchBundle {
  val pc = UInt(64.W)
}

class BTBResp_Bundle extends IFetchBundle {
  val target = UInt(64.W)
}

class BTBUpdate_Bundle extends BTBReq_Bundle {
  val new_Target = UInt(64.W)
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
  val pc = UInt(64.W)
  val ghist = UInt(64.W)
  val ftq_tage = Vec( 6, new TageTableResp_Bundle )
  val is_provider = Vec( 6, Bool() )
  val is_altpred  = Vec( 6, Bool() )
  val is_predictTaken = Bool()
  val is_alloc = Vec( 6, Bool() )

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
  val is_finalTaken = Bool()

  def is_misPredict = is_alloc.reduce(_|_) & (is_predictTaken =/= is_finalTaken)
}
