

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

package rift2Core.backend.memory

import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._
import rift._
import base._
import rift2Core.L1Cache._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._


class IO_Lsu(edge: TLEdgeOut, idx: Int)(implicit p: Parameters) extends RiftModule{
  def nm = 8
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Info_cache_s0s1))
    val deq = new DecoupledIO(new Info_cache_retn)
    val is_empty = Output(Bool())

    val getPut    = new DecoupledIO(new TLBundleA(edge.bundle))
    val access = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
  })

  val is_busy = RegInit(false.B)
  val pending_wb = Reg(new WriteBack_info(dw=64,dp=64))
  val pending_paddr = Reg(UInt(64.W))
  val pending_fun = Reg(new Cache_op)

  io.is_empty := ~is_busy

  io.getPut.valid := io.enq.valid & ~is_busy
  io.enq.ready := io.getPut.fire
  when( io.enq.valid ) {
    when( io.enq.bits.fun.is_lu & ~io.enq.bits.fun.is_lr) {
        io.getPut.bits := 
          edge.Get(
            fromSource = idx.U,
            toAddress = io.enq.bits.paddr,
            lgSize = log2Ceil(64/8).U
          )._2    
      } .elsewhen( io.enq.bits.fun.is_su & ~io.enq.bits.fun.is_sc ) {
        io.getPut.bits :=
          edge.Put(
            fromSource = idx.U,
            toAddress = io.enq.bits.paddr,
            lgSize = log2Ceil(64/8).U,
            data = io.enq.bits.wdata(0),
            mask = io.enq.bits.wstrb
          )._2
      } .otherwise{
        io.getPut.bits := DontCare
        assert(false.B, "Assert Failed at IO_Lsu, RISCV-A is not support at IO region")      
      }    
  } .otherwise {
    io.getPut.bits := DontCare
  }
  


  when( io.getPut.fire ) {
    assert( is_busy === false.B  )
    pending_wb.rd0 := io.enq.bits.rd.rd0
    pending_paddr := io.enq.bits.paddr
    pending_fun := io.enq.bits.fun
    is_busy := true.B
  } .elsewhen( io.access.fire ) {
    assert( is_busy === true.B  )
    is_busy := false.B
  }

  io.deq.valid    := io.access.valid

  io.deq.bits.wb := pending_wb
  io.deq.bits.wb.res := 
  {
    val rdata = io.access.bits.data
    val paddr = pending_paddr
    val fun = pending_fun
    val overlap_wdata = io.enq.bits.wdata(0)
    val overlap_wstrb = io.enq.bits.wstrb
    
    val res_pre_pre = {
      val (new_data, new_strb) = overlap_wr( rdata, 0.U, overlap_wdata, overlap_wstrb)
      new_data
    }
    val res_pre = get_loadRes( fun, paddr, res_pre_pre )
    res_pre
  }

  io.deq.bits.is_load_amo := (io.access.bits.opcode === TLMessages.AccessAckData)
  // io.deq.bits.paddr := pending_paddr
  io.deq.bits.chk_idx := 0.U

  io.deq.bits.is_flw := Mux( io.deq.valid, pending_fun.flw, false.B )
  io.deq.bits.is_fld := Mux( io.deq.valid, pending_fun.fld, false.B )


  io.access.ready := io.deq.ready



    
}


