

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


class Cache_Lsu(edge: TLEdgeOut)(implicit p: Parameters) extends RiftModule{
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Lsu_iss_info))
    val deq = new DecoupledIO(new WriteBack_info)

    val missUnit_dcache_acquire = Decoupled(new TLBundleA(edge.bundle))
    val missUnit_dcache_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val missUnit_dcache_grantAck = Decoupled(new TLBundleE(edge.bundle))
    val probeUnit_dcache_probe = Flipped(DecoupledIO(new TLBundleB(edge.bundle)))
    val writeBackUnit_dcache_release = DecoupledIO(new TLBundleC(edge.bundle))
    val writeBackUnit_dcache_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val flush = Input(Bool())
  })


  val opMux = Module(new OpMux) 
    opMux.io.enq <> io.enq

    


  val stQueue = Module(new Store_queue)
  val stBuffer = Module(new Store_buffer)
  val dcache = Module(new Dcache(edge(i)))

  val su_wb_fifo = Module( new Queue( new WriteBack_info, 1, false, true ) )
  val lu_wb_fifo = Module( new Queue( new WriteBack_info, 1, false, true ) )
  val wb_arb = Module(new Arbiter(new WriteBack_info, 2))



  opMux.io.ld_deq = DecoupledIO(new Lsu_iss_info)
  opMux.io.am_deq = DecoupledIO(new Lsu_iss_info)

  su_wb_fifo.io.enq.valid := opMux.io.st_deq.fire
  su_wb_fifo.io.enq.bits  := opMux.io.st_deq.bits.param.rd

  stQueue.io.enq.valid := opMux.io.st_deq.fire | opMux.io.am_deq.fire
  // stQueue.io.enq.bits.paddr := {
  //   val clr_mask = ~(("b111".U)(64.W))
  //   Mux1H( Seq(
  //     opMux.io.st_deq.fire -> (opMux.io.st_deq.bits.param.op1 & clr_mask),
  //     opMux.io.am_deq.fire -> (opMux.io.am_deq.bits.param.op1 & clr_mask),
  //   ))
  // }

  // stQueue.io.enq.bits.wdata := {
  //   val res = Wire(UInt(64.W))
  //   val paddr = 
  //     Mux1H( Seq(
  //       opMux.io.st_deq.fire -> opMux.io.st_deq.bits.param.op1,
  //       opMux.io.am_deq.fire -> opMux.io.am_deq.bits.param.op1,
  //     ))
  //   val shift = Wire(UInt(6.W))
  //   shift := Cat( paddr(2,0), 0.U(3.W) )
  //   res := 
  //     Mux1H( Seq(
  //       opMux.io.st_deq.fire -> opMux.io.st_deq.bits.param.op2 << shift,
  //       opMux.io.am_deq.fire -> opMux.io.am_deq.bits.param.op2 << shift,
  //     ))      
      
  //   res
  // }

  // stQueue.io.enq.bits.wmask := {
  //   val paddr =
  //     Mux1H( Seq(
  //       opMux.io.st_deq.fire -> opMux.io.st_deq.bits.param.op1,
  //       opMux.io.am_deq.fire -> opMux.io.am_deq.bits.param.op1,
  //     )) 
      
  //   val op =
  //     Mux1H( Seq(
  //       opMux.io.st_deq.fire -> opMux.io.st_deq.bits.fun,
  //       opMux.io.am_deq.fire -> opMux.io.am_deq.bits.fun,
  //     )) 

  //   Mux1H(Seq(
  //     op.is_byte -> Fill( 8, 1.U), op.is_half -> Fill(16, 1.U),
  //     op.is_word -> Fill(32, 1.U), op.is_dubl -> Fill(64, 1.U)
  //   )) << Cat(paddr(2,0), 0.U(3.W) )
  // }

  stQueue.io.enq.bits.fun :=
    Mux1H(Seq(
      opMux.io.st_deq.fire -> opMux.io.st_deq.bits.fun,
      opMux.io.am_deq.fire -> opMux.io.am_deq.bits.fun,
    ))

  stQueue.io.enq.bits.chk_idx := DontCare
  stQueue.io.enq.bits.rd :=
    Mux1H(Seq(
      opMux.io.st_deq.fire -> opMux.io.st_deq.bits.param.rd,
      opMux.io.am_deq.fire -> opMux.io.am_deq.bits.param.rd,
    ))

  opMux.io.st_deq.ready := su_exe_iwb_fifo.io.enq.ready & stQueue.io.enq.ready
  opMux.io.am_deq.ready := stQueue.io.enq.ready

  // stBuffer.io.enq = DecoupledIO(new Reservation_Info)
  // stBuffer.io.rls = DecoupledIO(UInt(6.W))
  // dcache.io.

  stBuffer.io.enq.valid := stQueue.io.deq.fire
  stBuffer.io.enq.bits  := stQueue.io.deq.bits
  

  // dcache.io.deq = new DecoupledIO(new Info_cache_retn)
  // dcache.io.is_lr_clear = Input(Bool())

  dcache.io.missUnit_dcache_acquire <> io.missUnit_dcache_acquire
  dcache.io.missUnit_dcache_grant <> io.missUnit_dcache_grant
  dcache.io.missUnit_dcache_grantAck <> io.missUnit_dcache_grantAck
  dcache.io.probeUnit_dcache_probe <> io.probeUnit_dcache_probe
  dcache.io.writeBackUnit_dcache_release <> io.writeBackUnit_dcache_release
  dcache.io.writeBackUnit_dcache_grant <> io.writeBackUnit_dcache_grant

  dcache.io.enq = Flipped(new DecoupledIO(new Info_cache_s0s1))
  dcache.io.enq.valid := stQueue.io.deq.fire
  dcache.io.enq.bits.
  val paddr = UInt(64.W)
  val wmask  = UInt(8.W)
  val wdata  = Vec(bk,UInt(64.W))
  val op    = new Cache_op
  val rd = new Rd_Param
  chk_idx = UInt(8.W)

  stQueue.io.deq.ready  := stBuffer.io.enq.ready & dcache.io.enq.ready


  lu_exe_iwb_fifo.io.enq.valid :=
  lu_exe_iwb_fifo.io.enq.bits :=
  lu_exe_iwb_fifo.io.enq.ready

  wb_arb.io.in(0) <> su_exe_iwb_fifo.io.deq
  wb_arb.io.in(1) <> lu_exe_iwb_fifo.io.deq
  wb_arb.io.out   <> io.deq

}


class Mem(edge: Vec[TLEdgeOut])(implicit p: Parameters) extends RiftModule{
  val io = IO(new Bundle{
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))

    val lsu_exe_iwb = new DecoupledIO(new Exe_iwb_info)
    val lsu_exe_fwb = new DecoupledIO(new Exe_fwb_info)

    val cmm_lsu = Input(new Info_cmm_lsu)
    val lsu_cmm = Output( new Info_lsu_cmm )

    val lsu_mmu = DecoupledIO(new Info_mmu_req)
    val mmu_lsu = Flipped(DecoupledIO(new Info_mmu_rsp))

    val missUnit_dcache_acquire = 
      for ( i <- 0 until nm ) yield Decoupled(new TLBundleA(edge(i).bundle))
    val missUnit_dcache_grant = 
      for ( i <- 0 until nm ) yield Flipped(DecoupledIO(new TLBundleD(edge(i).bundle)))
    val missUnit_dcache_grantAck  = 
      for ( i <- 0 until nm ) yield Decoupled(new TLBundleE(edge(i).bundle))
    val probeUnit_dcache_probe = 
      for ( i <- 0 until nm ) yield Flipped(DecoupledIO(new TLBundleB(edge(i).bundle)))
    val writeBackUnit_dcache_release =
      for ( i <- 0 until nm ) yield DecoupledIO(new TLBundleC(edge(i).bundle))
    val writeBackUnit_dcache_grant   =
      for ( i <- 0 until nm ) yield Flipped(DecoupledIO(new TLBundleD(edge(i).bundle)))

    val sys_get    = new DecoupledIO(new TLBundleA(edge(nm).bundle))
    val sys_access = Flipped(new DecoupledIO(new TLBundleD(edge(nm).bundle)))

    val preph_get    = new DecoupledIO(new TLBundleA(edge(nm+1).bundle))
    val preph_access = Flipped(new DecoupledIO(new TLBundleD(edge(nm+1).bundle)))

    // val sys_chn_ar = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    // val sys_chn_r = Flipped( new DecoupledIO(new AXI_chn_r( 64, 1, 1)) )
    // val sys_chn_aw = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    // val sys_chn_w = new DecoupledIO(new AXI_chn_w( 64, 1 )) 
    // val sys_chn_b = Flipped( new DecoupledIO(new AXI_chn_b( 1, 1 )))


    val flush = Input(Bool())
  })


  val regionMux = Module(new regionMux)
  val cacheMux = Module(new cacheMux)
 
  val rtn_arb = Module(new Arbiter( new Info_cache_retn, nm+2))

  val su_exe_iwb_fifo = Module( new Queue( new Exe_iwb_info, 1, false, true ) )
  val lu_exe_iwb_fifo = Module( new Queue( new Exe_iwb_info, 1, false, true ) )
  val fe_exe_iwb_fifo = Module( new Queue( new Exe_iwb_info, 1, false, true ) )
  val su_exe_fwb_fifo = Module( new Queue( new Exe_fwb_info, 1, false, true ) )
  val lu_exe_fwb_fifo = Module( new Queue( new Exe_fwb_info, 1, false, true ) )

  regionMux.io.enq := addrTrans( io.iss_exe, io.mmu_lsu )
  regionMux.io.deq(0)
  regionMux.io.deq(1)

  regionMux.io.deq(2) <> cacheMux.io.enq


  







}

