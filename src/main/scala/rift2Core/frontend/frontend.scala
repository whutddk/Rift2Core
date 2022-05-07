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
  UBTB_entry: Int,
)

trait HasIFParameters extends HasRiftParameters {
  val ifParams: IFParameters

  // def GHR_length = ifParams.GHR_length
  def UBTB_entry = ifParams.UBTB_entry
  val fetch_instr = ifParams.fetch_w/16
}

abstract class IFetchModule(implicit val p: Parameters) extends Module with HasIFParameters { def io: Record }
abstract class IFetchBundle(implicit val p: Parameters) extends Bundle with HasIFParameters


class Info_IF1 extends IFetchBundle {
  val addr = UInt(64.W)
  val BHR  = UInt(64.W)

}

class Info_BTB extends IFetchBundle {
  val targets   = UInt(64.W)
  val tag       = UInt(btb_tag_w.W)
  // val is_jump   = Bool()
  // val is_takens = Bool()
  // val is_branch = Bool()
  // val is_RVC    = Bool()

}


