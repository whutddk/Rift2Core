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

import rift2Chip._
import org.chipsalliance.cde.config._


trait VRegFilesLookup{ this: RegFilesReal => 

  val mollocIdx: Vec[UInt]

  for ( i <- 0 until rnc ) {
    val idx1 = io.lookup(i).req.rs1
    val idx2 = io.lookup(i).req.rs2
    val idx3 = io.lookup(i).req.rs3


    if ( i == 0 ) {
      io.lookup(i).rsp.vm0 := rename_ptr(0)
      io.lookup(i).rsp.rs1 := rename_ptr(idx1)
      io.lookup(i).rsp.rs2 := rename_ptr(idx2)
      io.lookup(i).rsp.rs3 := rename_ptr(idx3)
    } else {
      io.lookup(i).rsp.vm0 := rename_ptr(0)
      io.lookup(i).rsp.rs1 := rename_ptr(idx1)
      io.lookup(i).rsp.rs2 := rename_ptr(idx2)
      io.lookup(i).rsp.rs3 := rename_ptr(idx3)

      for ( j <- 0 until i ) {
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === 0.U ) ) { io.lookup(i).rsp.vm0 := mollocIdx(j) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx1) ) { io.lookup(i).rsp.rs1 := mollocIdx(j) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx2) ) { io.lookup(i).rsp.rs2 := mollocIdx(j) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx3) ) { io.lookup(i).rsp.rs3 := mollocIdx(j) }
      }
    }
    when( io.rename(i).req.fire ) {
      assert( io.lookup(i).rsp.vm0 =/= 0.U )      
      assert( io.lookup(i).rsp.rs1 =/= 0.U )
      assert( io.lookup(i).rsp.rs2 =/= 0.U )
      assert( io.lookup(i).rsp.rs3 =/= 0.U )
    }
  }
}

class VRegFiles(dw: Int, dp: Int, rnc: Int, rop: Int, wbc: Int, cmm: Int)(implicit p: Parameters) extends RegFilesReal(dw, dp, arc = 32, rnc, rop, wbc, cmm)
with RegFilesReName
with VRegFilesLookup
with RegFilesReadOP
with RegFilesWriteBack
with RegFilesCommit{

}



