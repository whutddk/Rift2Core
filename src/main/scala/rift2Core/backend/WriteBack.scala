



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
import base._

import rift2Core.define._
import rift2Core.diff._

class WriteBack(dp: Int=64, rn_chn: Int = 2, rop_chn: Int=2, wb_chn: Int=4, cmm_chn: Int = 2) extends Module {
  val io = IO(new Bundle{
    val dpt_rename = Vec( rn_chn, new dpt_rename_info(dp) )



    val ooo_readOp  = Vec(4, Flipped( new iss_readOp_info(dp)))
    val ito_readOp  = Flipped( new iss_readOp_info(dp))
    val fpu_readOp  = Flipped( new iss_readOp_info(dp))

    val alu_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dp)))
    val bru_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dp)))
    val csr_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dp)))
    val mem_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dp)))
    val mul_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dp)))
    val fpu_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dp)))

    val commit = Vec(cmm_chn, Flipped(Valid(new Info_commit_op(dp))))

    val diff_register = Output(new Info_abi_reg)
  })


  val iReg = Module(new RegFiles(dp, rn_chn, rop_chn, wb_chn, cmm_chn))
//   val fReg = Module(new RegFiles(dp=64, rn_chn=2, rop_chn=1, wb_chn=1, cmm_chn=2))

    iReg.io.dpt_rename <> io.dpt_rename
    iReg.io.commit <> io.commit
    iReg.io.diff_register <> io.diff_register



    val readOp_arb = {
      val mdl = Module(new XArbiter(new iss_readOp_info(dp), in = 6, out = rop_chn))
      mdl.io.enq(0) <> io.ooo_readOp(0)
      mdl.io.enq(1) <> io.ooo_readOp(1)
      mdl.io.enq(2) <> io.ooo_readOp(2)
      mdl.io.enq(3) <> io.ooo_readOp(3)
      mdl.io.enq(4) <> io.ito_readOp
      mdl.io.enq(5) <> io.fpu_readOp
      mdl.io.deq <> iReg.io.iss_readOp

      mdl
    }

    val writeBack_arb = {
      val mdl = Module(new XArbiter(new WriteBack_info(dp), in = 6, out = wb_chn))
      mdl.io.enq(0) <> io.alu_iWriteBack
      mdl.io.enq(1) <> io.bru_iWriteBack
      mdl.io.enq(2) <> io.csr_iWriteBack
      mdl.io.enq(3) <> io.mem_iWriteBack
      mdl.io.enq(4) <> io.mul_iWriteBack
      mdl.io.enq(5) <> io.fpu_iWriteBack
      mdl.io.deq <> iReg.io.exe_writeBack

      mdl
    }

}
