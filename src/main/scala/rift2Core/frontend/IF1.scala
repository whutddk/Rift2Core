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










/**
  * instract fetch stage 1, generate pc and predict by a fully associative cache
  */
abstract class IF1Base extends IFetchModule {
  val io = IO(new Bundle{
    val redir_if2 = Flipped(Valid())
    val redir_if3 = Flipped(Valid())
    val redir_cmm = Flipped(Valid())


    val pc_gen = Decoupled(new Info_IF1)
  })

  val pc_reg = RegInit(0.U(64.W))


}

trait GHR { this: IF1Base =>

  val GHR = RegInit(0.U(64.W))


}

// trait uBTB { this: IF1Base =>

//   val uBTB_table = RegInit( VecInit( Seq.fill(UBTB_entry)( 0.U.asTypeOf(new Info_UBTB) )))
//   val uBTB_valid = RegInit( VecInit( Seq.fill(UBTB_entry)( false.B                     )))

//   def update_uBTB = {

//     val cb_sel = 
//   }


// }

class IF1 extends IF1Base with GHR with uBtB {

}
