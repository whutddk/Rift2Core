

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

package rift2Core.backend.mem

import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._



class IO_Lsu(edge: TLEdgeOut, idx: Int)(implicit p: Parameters) extends RiftModule{
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Lsu_iss_info))
    val deq = new DecoupledIO(new Info_cache_retn)
    val overlap = new Info_overlap

    val getPut    = new DecoupledIO(new TLBundleA(edge(nm).bundle))
    val access = Flipped(new DecoupledIO(new TLBundleD(edge(nm).bundle)))
  })

  val req_fifo = {
    val mdl = Module(new Queue( new Lsu_iss_info, 8 ), pipe = true)
    // mdl.io.enq <> io.enq
    mdl
  }






  val ls_arb = {
    val mdl = Module(new Arbiter(new Info_cache_s0s1, 2))
    mdl.io.in(0).valid := opMux.io.ld_deq.valid
    mdl.io.in(0).bits  := pkg_Info_cache_s0s1(opMux.io.ld_deq.bits)
    opMux.io.ld_deq.ready := mdl.io.in(0).ready

    mdl.io.in(1) <> stQueue.io.deq
    mdl
  }

  val lu_wb_fifo = Module( new Queue( new WriteBack_info, 1, false, true ) )

  val wb_arb = {
    val mdl = Module(new Arbiter(new WriteBack_info, 2))
    mdl.io.in(0) <> su_wb_fifo.io.deq
    mdl.io.in(1) <> lu_wb_fifo.io.deq
    mdl.io.out   <> io.deq
    mdl
  }


  // ls_arb.io.out <> dcache.io.enq

  io.getPut.valid := ls_arb.io.out.valid & ~is_busy
  when( io.enq.bits.fun.is_lu & ~io.enq.bits.fun.is_lr) {
    io.getPut.bits := 
      edge.Get(
        fromSource = idx.U,
        toAddress = io.enq.paddr
        lgSize = log2Ceil(64/8).U
      )._2    
  } .elsewhen( io.enq.bits.fun.is_su & ~io.enq.bits.fun.is_sc ) {
    io.getPut.bits :=
      edge.Put(
        fromSource = idx.U,
        toAddress = io.enq.paddr
        lgSize = log2Ceil(64/8).U,
        data = io.enq.wdata,
        mask = io.enq.wstrb
      )._2
  } .otherwise{
    io.getPut.bits := DontCare

    assert(false.B, "Assert Failed at IO_Lsu, RISCV-A is not support at IO region")
  }

  val is_busy = RegInit(false.B)
  val pending_rd = Reg(new Rd_Param)
  val pending_paddr = Reg(new UInt(64.W))
  when( io.getPut.fire ) {
    assert( is_busy === false.B  )
    pending_rd := io.enq.rd
    pending_paddr := io.enq.bits.param.op1
    is_busy := true.B
  } .elsewhen( io.access.fire ) {
    assert( is_busy === true.B  )
    is_busy := false.B
  }

  io.deq.valid    := io.access.valid

  io.deq.bits.wb.rd := pending_rd
  io.deq.bits.wb.res := io.access.bits.data
  io.deq.bits.is_load_amo := (io.access.bits.opcode === TLMessages.AccessAckData)
  io.deq.bits.paddr := pending_paddr
  io.access.ready := io.deq.ready

  // io.deq.bits.res := {
  //   val mdl = Module(new overlap_chk)
  //   val ori = io.access.bits.data

  //   mdl.io.rd_info.valid := io.deq.valid
  //   mdl.io.rd_info.bits.paddr :=
  //   mdl.io.rd_info.bits.rdata := 

  //   mdl.io.wr_info := io.overlap

  //   Mux( mdl.io.wr_info.rsp.valid,
  //     overlap_wr( ori, DontCare, mdl.io.wr_info.rsp.bits.wdata, mdl.io.wr_info.rsp.bits.wstrb)._1,
  //     ori
  //   )
  // }
  
  // io.deq.bits.rd  := pending_rd

    
}


