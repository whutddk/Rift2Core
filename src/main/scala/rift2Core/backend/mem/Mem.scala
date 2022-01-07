

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

  val stQueue = Module(new Store_queue)
  val ls_arb = Module(new Arbiter(new Info_cache_s0s1, 2))
  val dcache = Module(new Dcache(edge(i)))

  val su_wb_fifo = Module( new Queue( new WriteBack_info, 1, false, true ) )
  val lu_wb_fifo = Module( new Queue( new WriteBack_info, 1, false, true ) )

  val wb_arb = Module(new Arbiter(new WriteBack_info, 2))


  io.enq <> opMux.io.enq 


  ls_arb.io.in(0).valid := opMux.io.ld_deq.valid
  ls_arb.io.in(0).bits  := pkg_Info_cache_s0s1(opMux.io.ld_deq.bits)
  opMux.io.ld_deq.ready := ls_arb.io.in(0).ready



  su_wb_fifo.io.enq.valid := opMux.io.st_deq.fire
  su_wb_fifo.io.enq.bits  := opMux.io.st_deq.bits.param.rd

  stQueue.io.enq.valid := opMux.io.st_deq.fire | opMux.io.am_deq.fire
  stQueue.io.enq.bits :=
    Mux1H(Seq(
      opMux.io.st_deq.fire -> opMux.io.st_deq.bits,
      opMux.io.am_deq.fire -> opMux.io.am_deq.bits,
    ))

  opMux.io.st_deq.ready := su_wb_fifo.io.enq.ready & stQueue.io.enq.ready
  opMux.io.am_deq.ready := stQueue.io.enq.ready

  ls_arb.io.in(1) <> stQueue.io.deq
  ls_arb.io.out <> dcache.io.enq
  


  dcache.io.flush := io.flush

  dcache.io.missUnit_dcache_acquire <> io.missUnit_dcache_acquire
  dcache.io.missUnit_dcache_grant <> io.missUnit_dcache_grant
  dcache.io.missUnit_dcache_grantAck <> io.missUnit_dcache_grantAck
  dcache.io.probeUnit_dcache_probe <> io.probeUnit_dcache_probe
  dcache.io.writeBackUnit_dcache_release <> io.writeBackUnit_dcache_release
  dcache.io.writeBackUnit_dcache_grant <> io.writeBackUnit_dcache_grant


  lu_wb_fifo.io.enq.valid := dcache.io.deq.valid & dcache.io.deq.bits.is_load_amo
  lu_wb_fifo.io.enq.bits  := dcache.io.deq.bits.wb
  su_wb_fifo.io.enq.valid := dcache.io.deq.valid & ~dcache.io.deq.bits.is_load_amo
  su_wb_fifo.io.enq.bits  := dcache.io.deq.bits.wb

  dcache.io.deq.ready := lu_wb_fifo.io.enq.ready & su_wb_fifo.io.enq.ready

  wb_arb.io.in(0) <> su_wb_fifo.io.deq
  wb_arb.io.in(1) <> lu_wb_fifo.io.deq
  wb_arb.io.out   <> io.deq

}

class IO_Lsu(edge: TLEdgeOut)(implicit p: Parameters) extends RiftModule{
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Lsu_iss_info))
    val deq = new DecoupledIO(new WriteBack_info)

    val get    = new DecoupledIO(new TLBundleA(edge(nm).bundle))
    val access = Flipped(new DecoupledIO(new TLBundleD(edge(nm).bundle)))

    val flush = Input(Bool())
  })

  val req_fifo = Module(new Queue( new Lsu_iss_info, 8 ), pipe = true)

  io.icache_get.valid := icache_state_qout === 1.U & ~is_hit & ~io.flush
  io.icache_get.bits :=
    edge.Get(
      fromSource = 0.U,
      toAddress = io.mmu_if.bits.paddr & ("hffffffff".U << addr_lsb.U),
      lgSize = log2Ceil(256/8).U
    )._2

    
}

class Mem(edge: Vec[TLEdgeOut])(implicit p: Parameters) extends RiftModule{
  val io = IO(new Bundle{
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))

    val lsu_exe_wb = new DecoupledIO(new WriteBack_info)


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

  val cache = for ( i <- 0 until nm ) yield {
    val mdl = Moudle(new Cache_Lsu(edge(i)))
    mdl.io.enq := cacheMux.io.deq(i)
    mdl.io.deq := rtn_arb.io.in(i)

    mdl.io.missUnit_dcache_acquire <> io.missUnit_dcache_acquire(i)
    mdl.io.missUnit_dcache_grant <> io.missUnit_dcache_grant(i)
    mdl.io.missUnit_dcache_grantAck <> io.missUnit_dcache_grantAck(i)
    mdl.io.probeUnit_dcache_probe <> io.probeUnit_dcache_probe(i)
    mdl.io.writeBackUnit_dcache_release <> io.writeBackUnit_dcache_release(i)
    mdl.io.writeBackUnit_dcache_grant <> io.writeBackUnit_dcache_grant(i)

    mdl.io.flush := io.flush

    mdl
  }


  regionMux.io.enq := addrTrans( io.iss_exe, io.mmu_lsu )
  <> regionMux.io.deq(0)
  <> regionMux.io.deq(1)

  rtn_arb.io.in(nm) <>
  rtn_arb.io.in(nm+1) <>

  cacheMux.io.enq <> regionMux.io.deq(2)
  io.lsu_exe_wb <> rtn_arb.io.out

  







}

