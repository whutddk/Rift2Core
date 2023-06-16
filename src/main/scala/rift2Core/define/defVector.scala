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

trait Vec_vType_Bundle{ this: RiftBundle =>
  val vtype   = UInt( 64.W )
  def vsew  = vtype(5,3)
  def vlmul = vtype(2,0)
}

trait Vec_PreRename_Bundle{ this: RiftBundle =>
  val nf     = UInt(3.W)
  val vm     = Bool()
  

  // val vstartSel = UInt( (log2Ceil(vParams.vlmax)).W )
  val lmulSel  = UInt(3.W)
  val nfSel    = UInt(3.W)
  val widenSel = UInt(1.W)

  val microIdx = UInt(3.W)
  // val tEleIdx  = UInt( (log2Ceil(vParams.vlmax)+1).W )
  val vlIdx    = UInt( (log2Ceil(vParams.vlmax)+1).W )
  val vlCnt   = UInt( log2Ceil( vParams.vlen / 8 ).W ) //how many vec-element in this micro instrution, load at preRename

}

trait Vec_element_Index{ this: RiftBundle =>
  val eleIdx = UInt(log2Ceil(vParams.vlen/8).W  ) //the index of vec-element in micro instruciton, load at preIssue
}

trait Vec_PreIssue_Bundle{ this: RiftBundle =>

  val vop0 = Bool()
  val vop1 = UInt(64.W)
  val vop2 = UInt(64.W)
        // val voffset = UInt(64.W) //the index of vec-element in whole instruction, load at preIssue used at exe 
}

class VRename_Attach_Bundle(implicit p: Parameters) extends RiftBundle
with Vec_PreRename_Bundle
with Vec_vType_Bundle
with Vec_PreIssue_Bundle
with Vec_element_Index{

}

class VLsu_Attach_Bundle(implicit p: Parameters) extends RiftBundle
with Vec_element_Index
with Vec_vType_Bundle


// class VDcache_Attach_Bundle(implicit p: Parameters) extends VRename_Attach_Bundle{

//   val bufIdx = UInt(log2Ceil(vParams.lsuEntry).W)
//   val eleIdx = UInt(log2Ceil(vParams.vlen/8).W  )

// }




// class VLsu_Attach_Bundle(implicit p: Parameters) extends VRename_Attach_Bundle{
//   val vWidth = UInt(3.W)
//   // val isFoF  = Bool()

//   val bufIdx = UInt(log2Ceil(vParams.lsuEntry).W)
//   val eleIdx = UInt(log2Ceil(vParams.vlen/8).W  )

//   def group = (nfSel+1.U) << lmulSel
//   def vlmul = vtype(2,0)
//   def vsew  = vtype(5,3)
//   def vma   = vtype.extract(7)
//   def vta   = vtype.extract(6)
// }

