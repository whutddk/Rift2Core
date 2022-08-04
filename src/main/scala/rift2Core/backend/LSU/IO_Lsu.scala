

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

package rift2Core.backend.lsu

import chisel3._
import chisel3.util._

import rift2Core.define._

import rift._
import base._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._


class IO_Lsu(edge: TLEdgeOut)(implicit p: Parameters) extends RiftModule{
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Lsu_iss_info))
    val deq = new DecoupledIO(new Dcache_Deq_Bundle)
    val is_empty = Output(Bool())

    val getPut    = new DecoupledIO(new TLBundleA(edge.bundle))
    val access = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
  })

  val is_busy = RegInit(false.B)
  val pending = Reg(new Lsu_iss_info)

  io.is_empty := ~is_busy

  io.getPut.valid := io.enq.valid & ~is_busy
  io.enq.ready := io.getPut.fire
  when( io.enq.valid ) {
    when( io.enq.bits.fun.is_lu & ~io.enq.bits.fun.is_lr) {
        io.getPut.bits := 
          edge.Get(
            fromSource = 0.U,
            toAddress = io.enq.bits.paddr >> log2Ceil(64/8).U << log2Ceil(64/8).U,
            lgSize = log2Ceil(64/8).U
          )._2    
      } .elsewhen( io.enq.bits.fun.is_su & ~io.enq.bits.fun.is_sc ) {
        io.getPut.bits :=
          edge.Put(
            fromSource = 0.U,
            toAddress = io.enq.bits.paddr >> log2Ceil(64/8).U << log2Ceil(64/8).U,
            lgSize = log2Ceil(64/8).U,
            data = io.enq.bits.wdata_align64,
            mask = io.enq.bits.wstrb_align64
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
    pending := io.enq.bits
    // pending_paddr := io.enq.bits.paddr
    // pending_fun := io.enq.bits.fun
    is_busy := true.B
  } .elsewhen( io.access.fire ) {
    assert( is_busy === true.B  )
    is_busy := false.B
  }

  io.deq.valid    := io.access.valid

  io.deq.bits.wb.rd0 := pending.param.rd0
  io.deq.bits.wb.res := 
  {
    val rdata = io.access.bits.data
    val paddr = pending.paddr
    val fun = pending.fun
    // val overlap_wdata = pending.wdata
    // val overlap_wstrb = pending.wstrb
    
    // val res_pre_pre = {
    //   val (new_data, new_strb) = overlap_wr( rdata, 0.U, overlap_wdata, overlap_wstrb)
    //   new_data
    // }
    val res_pre = get_loadRes( fun, paddr, rdata )
    res_pre
  }

  io.deq.bits.is_load_amo := (io.access.bits.opcode === TLMessages.AccessAckData)
  // io.deq.bits.paddr := pending_paddr
  io.deq.bits.chkIdx := 0.U

  io.deq.bits.is_flw := Mux( io.deq.valid, pending.fun.flw, false.B )
  io.deq.bits.is_fld := Mux( io.deq.valid, pending.fun.fld, false.B )


  io.access.ready := io.deq.ready



    
}

