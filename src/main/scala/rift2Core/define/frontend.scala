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

package rift2Core.define

import chisel3._
import chisel3.util._
import rift2Chip._
import org.chipsalliance.cde.config._



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

/**
  * This class represents the output bundle for the Redirect Request Interface from Instruction
  * Fetch Stage-4. It extends the IFetchBundle class and contains additional fields
  * @param p an implicit parameter of type Parameters, which is used by the Processor's implicit Parameters object.
  */
class IF4_Redirect_Bundle(implicit p: Parameters) extends IFetchBundle {
  /** a UInt indicating the target address for redirect, which predicted at IF4*/
  val target     = UInt(vlen.W)
  /** a UInt indicating the current program counter */
  val pc         = UInt(vlen.W)
  /** a Bool indicating whether the current branch prediction(at IF4) disagrees with the actual branch outcome(at IF1)*/
  val isDisAgree = Bool()
}


/**
  * This class represents the bundle for Return Address Stack Pop/Push Interface from Instruction Fetch
  * @param p an implicit parameter of type Parameters, which is used by the Processor's implicit Parameters object.
  */ 
class RASPP_Bundle(implicit p: Parameters) extends IFetchBundle {
  /** a UInt indicating the target address for pop/push operation on the Return Address Stack */
  val target = UInt(vlen.W)
}



/**
  * This class represents the output bundle for the Instruction Fetch Stage 1 (IF1) of the Processor. It extends the uBTBResp_Bundle
  * and contains an additional field
  * @param p an implicit parameter of type Parameters, which is used by the Processor's implicit Parameters object.
  */
class IF1_Bundle(implicit p: Parameters) extends uBTBResp_Bundle {
  /** a UInt indicating the program counter value for the following instructions to be fetched from the cache. */
  val pc = UInt(64.W)
  // val BHR  = UInt(64.W)
}

/**
  * This class represents the output bundle for the Instruction Fetch Stage 2 (IF2)
  * @param p an implicit parameter of type Parameters, which is used by the Processor's implicit Parameters object.
  */
class IF2_Bundle(implicit p: Parameters) extends IFetchBundle {
  /** indicates the program counter value for the instructions has been fetched */
  val pc    = UInt(vlen.W)
  /** the fetched instruction */
  val instr = UInt(16.W)
  // val isAccessFault = Bool()
  // val isPagingFault = Bool()
  // val BHR  = UInt(64.W)
  /** indicates if there's a fault while fetching the instruction */
  val isFault = Bool()
  /** indicates if there was a redirect record at this instruction (from uBTB in IF1) */
  val isRedirect = Bool()
  /** indicates the target address if there was a redirect. Else it is DontCare */
  val target = UInt(vlen.W)
}

/**
  * This class represents the output bundle for the Pre-Decoder.
  * @param p an implicit parameter of type Parameters, which is used by the Processor's implicit Parameters object.
  */
class PreDecode_Bundle(implicit p: Parameters) extends IFetchBundle {
  /** Indicates if the instruction is a JAL */
  val is_jal = Bool()
  /** Indicates if the instruction is a JALR */
  val is_jalr = Bool()
  /** Indicates if the instruction is a branch instruction */
  val is_branch = Bool()
  /** Indicates if the instruction is a CALL instruction */
  val is_call = Bool()
  /** Indicates if the instruction is a RETURN instruction */
  val is_return = Bool()
  /** Indicates if the instruction is a RISC V Compressed instruction */
  val is_rvc = Bool()
  /** Indicates if the instruction is a instruction fence instruction */
  val is_fencei = Bool()
  /** Indicates if the instruction is an sfencevma instruction */
  val is_sfencevma = Bool()
  /** Represents the immediate value of the instruction */
  val imm = UInt(64.W)

   /** Returns true if the instruction flows will be cut.
     * The instruction flow will be cut if it is a JAL, JALR, Branch, FENCE.I, or SFENCE.VMA instruction.
     */
  def is_pineline_cut = is_jal | is_jalr | is_branch | is_fencei | is_sfencevma

  /** Returns true if BTB request is required */
  def is_req_btb  = is_jalr
  /** Returns true if RAS request is required */
  def is_req_ras  = is_return
  /** Returns true if BIM request is required */
  def is_req_bim  = is_branch
  /** Returns true if TAGE request is required */
  def is_req_tage = is_branch
  /** Returns true if the instruction will lockup the pipeline */
  def is_lock_pipe = is_fencei | is_sfencevma
}

/**
  * Bundle for Bi-Model Predictor requests.
  * @constructor Create a new instance of BIMReq_Bundle.
  * @param p The implicit Parameters object for the processor.
  */
class BIMReq_Bundle(implicit p: Parameters) extends IFetchBundle {
  /** Program Counter. */
  val pc = UInt(vlen.W) 
}

/**
  * Bundle for Bi-Model Predictor respond.
  * @constructor Create a new instance of BIMResp_Bundle.
  * @param p The implicit Parameters object for the processor.
  */
class BIMResp_Bundle(implicit p: Parameters) extends IFetchBundle {
  /** Prediction for Taken or Not-Taken for primary BIM. */
  val bim_p = Bool()
  /** Prediction for Taken or Not-Taken for history BIM. */
  val bim_h = Bool()
}

/**
  * Bundle for Bi-Model Predictor update.
  * @constructor Create a new instance of BIMUpdate_Bundle.
  * @param p The implicit Parameters object for the processor.
  * @extends BIMResp_Bundle
  */
class BIMUpdate_Bundle(implicit p: Parameters) extends BIMResp_Bundle {
  /** Program counter for the instruction should be updated */
  val pc = UInt(vlen.W)
  /** Flag indicating whether the instruction is taken or not taken. */
  val isFinalTaken = Bool()
  
  /** Function to determine whether the branch was mispredicted. */
  def isMisPredict = isFinalTaken =/= bim_p 
}

/** 
  * Bundle for Branch Target Buffer (BTB) request interface.
  * @constructor Create a new instance of BTBReq_Bundle.
  * @param p The implicit Parameters object for the processor.
  * @extends IFetchBundle
  */
class BTBReq_Bundle(implicit p: Parameters) extends IFetchBundle {
  /** Program counter for the requesting instruction. */
  val pc = UInt(vlen.W) 
}

/**
  * Bundle for Micro Branch Target Buffer (uBTB) request interface.
  * @constructor Create a new instance of uBTBReq_Bundle.
  * @param p The implicit Parameters object for the processor.
  * @extends BTBReq_Bundle
  */
class uBTBReq_Bundle(implicit p: Parameters) extends BTBReq_Bundle

/**
  * Bundle for Branch Target Buffer (BTB) response interface.
  * @constructor Create a new instance of BTBResp_Bundle.
  * @param p The implicit Parameters object for the processor.
  * @extends IFetchBundle
  */
class BTBResp_Bundle(implicit p: Parameters) extends IFetchBundle {
  /** given out the target Program counter */
  val target = UInt(vlen.W) 
}

/**
  * Bundle for Micro Branch Target Buffer (uBTB) response interface. Inherits from BTBResp_Bundle.
  * @constructor Create a new instance of uBTBResp_Bundle.
  * @param p The implicit Parameters object for the processor.
  * @extends BTBResp_Bundle
  */
class uBTBResp_Bundle(implicit p: Parameters) extends BTBResp_Bundle {
  /** The isRedirect vector indicates whether a redirect should be taken at each slot. */
  val isRedirect = Vec( ftChn, Bool() )
  /** The isActive vector indicates whether the instruction should be consider at each slot. */
  val isActive   = Vec( ftChn, Bool() )
}

/**
  * This class represents a bundle for updating the Branch Target Buffer (BTB).
  * @param p implicit parameter of type Parameters.
  */
class BTBUpdate_Bundle(implicit p: Parameters) extends BTBResp_Bundle {
  /** Program counter for the updating instruction. */
  val pc = UInt(vlen.W)
}

/**
  * This class represents a micro-Branch Target Buffer (uBTB) update bundle,
  * which is used to update the BTB on processing branches.
  * It extends the BTBUpdate_Bundle class and contains an additional field isTaken,
  * which is of type Bool and 
  * @param p an implicit parameter of type Parameters, which is used by the Processor's implicit Parameters object.
  */
class uBTBUpdate_Bundle(implicit p: Parameters) extends BTBUpdate_Bundle {
  /** indicates whether a branch is taken or not at the end */
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





/**
  * This class represents the output interface for the Instruction Fetch Stage 3.
  * @param p an implicit parameter of type Parameters, which is used by the Processor's implicit Parameters object.
  */
class IF3_Bundle(implicit p: Parameters) extends RiftBundle {
  /* Indicates if the instruction is an RISC V Compressed instruction */
  val isRVC = Bool()
  /* Bundle of Pre-Decode output parameters */
  val preDecode = new PreDecode_Bundle
  // val predict = new Predict_Bundle
  /* the instruction that re-align in IF3 */
  val instr = UInt(32.W)
  /* the Program Counter of the instruction */
  val pc = UInt(vlen.W)
  /* Global History Register */
  val ghist = UInt(64.W)
  /* Indicates if the next instruction is redirected */
  val isRedirect = Bool()
  /* The redirect Target address of the next instruction */
  val target = UInt(vlen.W)
}


/**
 * Represents the output of the fourth stage of the Instruction Fetch (IF4), which finishes the instruction decoding.
 * @param p An implicit parameter of type `Parameters` required by the `Info_instruction` parent class.
 * @note This class inherits from the `Info_instruction` class, which provides all information about the instruction.
 */
class IF4_Bundle(implicit p: Parameters) extends Info_instruction{}

/**
  * Bundle definition for the branch fetch target interface.
  * This bundle represents the information that has been predicted for a branch instruction.
  * @param p the set of parameters defining the RISC-V core
  */
class Branch_FTarget_Bundle(implicit p: Parameters) extends RiftBundle {
  /** Program counter of the branch instruction. */
  val pc = UInt(vlen.W)
  /** Global history register value at the time of the branch. */
  val ghist = UInt(64.W)
  /** Respond of the Bi-Model Predictor */
  val bimResp  = new BIMResp_Bundle
  /** Respond of the TAGE predictor */
  val tageResp = new TageResp_Bundle
  /** Indicates whether the branch is predicted to be taken. */
  val isPredictTaken = Bool()
}

/**
  * Bundle definition for the jump fetch target interface.
  * This bundle represents the information that has been predicted for a jump instruction.
  * @param p the set of parameters defining the RISC-V core
  */
class Jump_FTarget_Bundle(implicit p: Parameters) extends RiftBundle {
  /* Program counter of the jump instruction. */
  val pc       = UInt(vlen.W)
  /* Response of the BTB predictor. */
  val btbResp = new BTBResp_Bundle
  /* Response of the RAS predictor. */
  val rasResp = new RASPP_Bundle
  /* Indicates whether the RAS predictor is being used. */
  val isRas = Bool()

  /** @return true if the BTB predictor is being used, false */
  def isBtb = ~isRas
}

/**
  * Bundle definition for the branch commit target interface.
  * This bundle represents the actual outcome of a branch instruction after execution.
  * @param p the set of parameters defining the RISC-V core
  */
class Branch_CTarget_Bundle(implicit p: Parameters) extends Branch_FTarget_Bundle {
  /** Indicates whether the actual branch outcome is taken or not. */
  val isFinalTaken = Bool()
  /** Actual target address of the branch instruction after execution. */
  val finalTarget = UInt(64.W)

  /** @return true if the actual branch outcome is mispredicted, false otherwise */
  def isMisPredict = isPredictTaken =/= isFinalTaken
}

/**
  * Bundle definition for the jump commit target interface.
  * This bundle represents the actual outcome of a jump instruction after execution.
  * @param p the set of parameters defining the RISC-V core
  */
class Jump_CTarget_Bundle(implicit p: Parameters) extends Jump_FTarget_Bundle {
  /** Actual target address of the jump instruction after execution. */
  val finalTarget = UInt(64.W)

  /** @return true if the actual branch outcome is mispredicted, false otherwise */
  def isMisPredict = Mux(isRas, rasResp.target =/= finalTarget, btbResp.target =/= finalTarget )
}


