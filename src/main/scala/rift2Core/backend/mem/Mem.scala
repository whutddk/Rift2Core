

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

    val is_commited = Input(Vec(2,Bool()))
    val overlap_paddr = ValidIO(UInt(64.W))
    val overlap_wdata = Flipped(ValidIO(UInt(64.W)))
    val overlap_wstrb = Flipped(ValidIO(UInt(64.W)))


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

class IO_Lsu(edge: TLEdgeOut, idx: Int)(implicit p: Parameters) extends RiftModule{
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Lsu_iss_info))
    val deq = new DecoupledIO(new WriteBack_info)

    val getPut    = new DecoupledIO(new TLBundleA(edge(nm).bundle))
    val access = Flipped(new DecoupledIO(new TLBundleD(edge(nm).bundle)))
  })

  val req_fifo = {
    val mdl = Module(new Queue( new Lsu_iss_info, 8 ), pipe = true)
    mdl.io.enq <> io.enq
    mdl
  }


  val opMux = {
    val mdl = Module(new OpMux)
    mdl.io.enq <> req_fifo.io.deq
    mdl.io.am_deq.ready := true.B
    assert(mdl.io.am_deq.valid === false.B, "Assert Failed at IO_Lsu, AMO is not supported in IO region")
    mdl
  }

  val stQueue = {
    val mdl = Module(new Store_queue)
    mdl.io.enq.valid := opMux.io.st_deq.fire 
    mdl.io.enq.bits := opMux.io.st_deq.bits
    mdl
  }

    val is_commited = Input(Vec(2,Bool()))
    val overlap_paddr = ValidIO(UInt(64.W))
    val overlap_wdata = Flipped(ValidIO(UInt(64.W)))
    val overlap_wstrb = Flipped(ValidIO(UInt(64.W)))

  val su_wb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info, 1, false, true ) )
    mdl.io.enq.valid := opMux.io.st_deq.fire
    mdl.io.enq.bits  := opMux.io.st_deq.bits.param.rd
    mdl
  }
  opMux.io.st_deq.ready := su_wb_fifo.io.enq.ready & stQueue.io.enq.ready


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


  ls_arb.io.out <> dcache.io.enq

  io.getPut.valid := io.enq.valid & ~is_busy
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
  when( io.getPut.fire ) {
    assert( is_busy === false.B  )
    pending_rd := io.enq.rd
    is_busy := true.B
  } .elsewhen( io.access.fire ) {
    assert( is_busy === true.B  )
    is_busy := false.B
  }

  io.deq.valid    := io.access.valid
  io.deq.bits.res := io.access.bits.data
  io.deq.bits.rd  := pending_rd
  io.access.ready := io.deq.ready
    
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

    val system_getPut = new DecoupledIO(new TLBundleA(edge(nm).bundle))
    val system_access = Flipped(new DecoupledIO(new TLBundleD(edge(nm).bundle)))

    val periph_getPut = new DecoupledIO(new TLBundleA(edge(nm+1).bundle))
    val periph_access = Flipped(new DecoupledIO(new TLBundleD(edge(nm+1).bundle)))


    val flush = Input(Bool())
  })


  val regionMux = {
    val mdl = Module(new regionMux)
    mdl.io.enq := addrTrans( io.lsu_iss_exe, io.mmu_lsu )
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

  val periph = {
    val mdl = IO_Lsu(edge(nm), idx = nm)
    mdl.io.enq <> regionMux.io.deq(0)


    mdl.io.getPut <> io.periph_getPut
    mdl.io.access <> io.periph_access
    mdl
  }
  val system = {
    val mdl = IO_Lsu(edge(nm+1), idx = nm+1)
    mdl.io.enq <> regionMux.io.deq(1)


    mdl.io.getPut <> io.system_getPut
    mdl.io.access <> io.system_access
    mdl
  }


  val rtn_arb = {
    val mdl = Module(new Arbiter( new Info_cache_retn, nm+2))

    for ( i <- 0 unitl nm ) until {
      mdl.io.in(i) <> cache.io.deq
    }

    mdl.io.in(nm)   <> system.io.deq
    mdl.io.in(nm+1) <> periph.io.deq

    mdl
  }


  io.lsu_exe_wb <> rtn_arb.io.out

}

