

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

    val system_getPut = new DecoupledIO(new TLBundleA(edge(nm).bundle))
    val system_access = Flipped(new DecoupledIO(new TLBundleD(edge(nm).bundle)))

    val periph_getPut = new DecoupledIO(new TLBundleA(edge(nm+1).bundle))
    val periph_access = Flipped(new DecoupledIO(new TLBundleD(edge(nm+1).bundle)))


    val flush = Input(Bool())
  })


  val opMux = {
    val mdl = Module(new OpMux)
    mdl.io.enq <> addrTrans( io.lsu_iss_exe, io.mmu_lsu )
    mdl
  }


  val stQueue = {
    val mdl = Module(new Store_queue)
    mdl.io.enq.valid := opMux.io.st_deq.fire | opMux.io.am_deq.fire
    mdl.io.enq.bits :=
      Mux1H(Seq(
        opMux.io.st_deq.fire -> opMux.io.st_deq.bits,
        opMux.io.am_deq.fire -> opMux.io.am_deq.bits,
      ))
    mdl.io.cmm_lsu := io.cmm_lsu
    mdl.io.flush = io.flush
    mdl
  }

  

  val ls_arb = {
    val mdl = Module(new Arbiter(new Info_cache_s0s1, 2))
    mdl.io.in(0).valid := opMux.io.ld_deq.valid
    mdl.io.in(0).bits  := pkg_Info_cache_s0s1(opMux.io.ld_deq.bits)
    mdl.io.in(1) <> stQueue.io.deq
    mdl
  }

  val regionMux = {
    val mdl = Module(new regionMux)
    mdl.io.enq <> ls_arb.io.out
    mdl
  }


  val cacheMux = {
    val mdl = Module(new cacheMux)
    mdl.io.enq <> regionMux.io.deq(2)
    mdl
  }



  val cache = for ( i <- 0 until nm ) yield {
    val mdl = Moudle(new Cache_Lsu(edge(i)))
    mdl.io.enq := cacheMux.io.deq(i)


    mdl.io.missUnit_dcache_acquire <> io.missUnit_dcache_acquire(i)
    mdl.io.missUnit_dcache_grant <> io.missUnit_dcache_grant(i)
    mdl.io.missUnit_dcache_grantAck <> io.missUnit_dcache_grantAck(i)
    mdl.io.probeUnit_dcache_probe <> io.probeUnit_dcache_probe(i)
    mdl.io.writeBackUnit_dcache_release <> io.writeBackUnit_dcache_release(i)
    mdl.io.writeBackUnit_dcache_grant <> io.writeBackUnit_dcache_grant(i)

    mdl.io.flush := io.flush

    mdl
  }

  val system = {
    val mdl = IO_Lsu(edge(nm+1), idx = nm+1)
    mdl.io.enq <> regionMux.io.deq(1)


    mdl.io.getPut <> io.system_getPut
    mdl.io.access <> io.system_access
    mdl
  }

  val periph = {
    val mdl = IO_Lsu(edge(nm), idx = nm)
    mdl.io.enq <> regionMux.io.deq(0)


    mdl.io.getPut <> io.periph_getPut
    mdl.io.access <> io.periph_access
    mdl
  }






  val lu_wb_arb = {
    val mdl = Module(new Arbiter(3, new Info_cache_retn))
    mdl.io.in(0) <> cache.io.deq
    mdl.io.in(1) <> system.io.deq
    mdl.io.in(2) <> periph.io.deq
    mdl
  }

  val lu_wb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info, 1, false, true ) )
    mdl.io.enq.valid := lu_wb_arb.io.out.valid
    mdl.io.enq.bits.rd := lu_wb_arb.io.out.bits.wb.rd
    mdl.io.enq.bits.res := {
      stQueue.io.overlap.paddr := lu_wb_arb.io.out.bits.paddr
      overlap_wr( lu_wb_arb.io.out.bits.wb.res, 0.U, stQueue.io.overlap.wdata, stQueue.io.overlap.wstrb)
    }
    lu_wb_arb.io.out.ready := mdl.io.enq.ready 
    mdl
  }

  val su_wb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info, 1, false, true ) )
    mdl.io.enq.valid := opMux.io.st_deq.fire
    mdl.io.enq.bits  := opMux.io.st_deq.bits.param.rd
    mdl
  }


  opMux.io.st_deq.ready := su_wb_fifo.io.enq.ready & stQueue.io.enq.ready
  opMux.io.am_deq.ready := stQueue.io.enq.ready
  opMux.io.ld_deq.ready := ls_arb.io.in(0).ready




  val rtn_arb = {
    val mdl = Module(new Arbiter( new Info_cache_retn, nm+2))

    for ( i <- 0 unitl nm ) until {
      mdl.io.in(i) <> cache.io.deq
    }

    mdl.io.in(nm)   <> system.io.deq
    mdl.io.in(nm+1) <> periph.io.deq

    mdl
  }


  io.lsu_exe_wb.valid := rtn_arb.io.out.valid
  io.lsu_exe_wb.bits := rtn_arb.io.out.bits.wb
  rtn_arb.io.out.ready := io.lsu_exe_wb.ready

}

