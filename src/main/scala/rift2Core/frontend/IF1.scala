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
  * instract fetch stage 1, generate pc
  */
abstract class IF1Base extends IFetchModule {
  val io = IO(new Bundle{
    val redir_if4 = Flipped(Valid())
    val redir_cmm = Flipped(Valid())


    val pc_gen = Decoupled(new Info_IF1)
  })

  val pc_dnxt = Wire(UInt(64.W))
  val pc_qout = RegInit("h80000000".U(64.W))

}




class IF1 extends IF1Base {
  val any_reset = RegInit(true.B)
  when( io.pc_gen.fire ) { any_reset := false.B }


  pc_dnxt := 
    Mux( any_reset, "h80000000".U, 
      Mux( io.redir_cmm.valid, io.redir_cmm.bits.pc,
        Mux( io.redir_if4.valid, io.redir_if4.bits.pc,
          (pc_qout + 16.U) >> 4 << 4 ) ) )

  io.pc_gen.valid   := true.B
  io.pc_gen.bits.pc := pc_dnxt
  when( io.pc_gen.fire ) { pc_qout := pc_dnxt }



}
