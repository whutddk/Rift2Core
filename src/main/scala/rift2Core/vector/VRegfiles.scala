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



// class VRegFiles (rnc: Int, rop: Int, wbc: Int, cmm: Int, regNum: Int)(implicit p: Parameters) extends RegFilesReal(vParams.vlen, rnc, rop, wbc, cmm, vRegNum) with RegFilesReName with RegFilesReadOP with RegFilesWriteBack with RegFilesCommit{

//   for ( i <- 0 until rnc ) {
//     val idx1 = io.lookup(i).req.rs1
//     val idx2 = io.lookup(i).req.rs2

//     if ( i == 0) {
//       io.lookup(i).rsp.rs1 := Mux( idx1 === 0.U, 0.U, rename_ptr(idx1) )
//       io.lookup(i).rsp.rs2 := Mux( idx2 === 0.U, 0.U, rename_ptr(idx2) )
//       io.lookup(i).rsp.rs3 := 0.U
//     } else {
//       io.lookup(i).rsp.rs1 := Mux( idx1 === 0.U, 0.U, rename_ptr(idx1) )
//       io.lookup(i).rsp.rs2 := Mux( idx2 === 0.U, 0.U, rename_ptr(idx2) )
//       io.lookup(i).rsp.rs3 := 0.U
//       for ( j <- 0 until i ) {
//         when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx1) && (idx1 =/= 0.U) ) { io.lookup(i).rsp.rs1 := mollocIdx(j) }
//         when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx2) && (idx2 =/= 0.U) ) { io.lookup(i).rsp.rs2 := mollocIdx(j) }
//       }
//     }
//   }



// }

