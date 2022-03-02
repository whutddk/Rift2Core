

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
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import rift2Core.L1Cache._
import rift2Core.privilege._


trait Fence_op{
  /** when a flush comes, flush all uncommit write & amo request in pending-fifo, and block all request from issue until scoreboard is empty */
  val trans_kill = RegInit(false.B)

  /** indicate if the fence is an icache fence */
  // val is_fence  = RegInit(false.B)
  // val is_fence_i  = RegInit(false.B)
  // val is_sfence_vma  = RegInit(false.B)
  // def is_fence_op = is_fence | is_fence_i | is_sfence_vma
}


class Lsu(edge: Seq[TLEdgeOut])(implicit p: Parameters) extends RiftModule with Fence_op with HasFPUParameters{
  def nm = 8
  val io = IO(new Bundle{
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val lsu_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64,dp=64))
    val lsu_exe_fwb = new DecoupledIO(new WriteBack_info(dw=65, dp=64))

    val cmm_lsu = Input(new Info_cmm_lsu)
    val lsu_cmm = Output( new Info_lsu_cmm )

    val lsu_mmu = DecoupledIO(new Info_mmu_req)
    val mmu_lsu = Flipped(DecoupledIO(new Info_mmu_rsp))

    val missUnit_dcache_acquire = 
      MixedVec(  for ( i <- 0 until 8 ) yield  DecoupledIO(new TLBundleA(edge(i).bundle))) 
    val missUnit_dcache_grant = 
      MixedVec(  for ( i <- 0 until 8 ) yield  Flipped(DecoupledIO(new TLBundleD(edge(i).bundle))))
    val missUnit_dcache_grantAck  = 
      MixedVec(  for ( i <- 0 until 8 ) yield  DecoupledIO(new TLBundleE(edge(i).bundle)))
    val probeUnit_dcache_probe = 
      MixedVec(  for ( i <- 0 until 8 ) yield  Flipped(DecoupledIO(new TLBundleB(edge(i).bundle))))
    val writeBackUnit_dcache_release =
      MixedVec(  for ( i <- 0 until 8 ) yield  DecoupledIO(new TLBundleC(edge(i).bundle)))
    val writeBackUnit_dcache_grant   =
      MixedVec(  for ( i <- 0 until 8 ) yield  Flipped(DecoupledIO(new TLBundleD(edge(i).bundle))))

    val system_getPut = new DecoupledIO(new TLBundleA(edge(nm).bundle))
    val system_access = Flipped(new DecoupledIO(new TLBundleD(edge(nm).bundle)))

    val periph_getPut = new DecoupledIO(new TLBundleA(edge(nm+1).bundle))
    val periph_access = Flipped(new DecoupledIO(new TLBundleD(edge(nm+1).bundle)))


    val flush = Input(Bool())
  })



  /** divide operation into load, store, and amo */
  val opMux = {
    val mdl = Module(new OpMux)
    mdl.io.enq <> addrTrans( io.lsu_iss_exe.bits, io.mmu_lsu, trans_kill | is_fence_op)
    mdl
  }

  /** for store and amo, they should be push into stQueue waiting for commited and pending commit */
  val stQueue = {
    val mdl = Module(new Store_queue)
    mdl.io.enq.valid := opMux.io.st_deq.fire | opMux.io.am_deq.fire
    mdl.io.enq.bits :=
      Mux1H(Seq(
        opMux.io.st_deq.fire -> opMux.io.st_deq.bits,
        opMux.io.am_deq.fire -> opMux.io.am_deq.bits,
      ))
    mdl.io.cmm_lsu := io.cmm_lsu
    mdl.io.flush := io.flush
    mdl
  }

  
  /** the raw req that will be merged
    * @param in Info_cache_s0s1
    * @return Info_cache_s0s1
    */
  val ls_arb = {
    val mdl = Module(new Arbiter(new Info_cache_s0s1, 2))
    stQueue.io.overlap.paddr := opMux.io.ld_deq.bits.param.dat.op1
    mdl.io.in(0).valid := opMux.io.ld_deq.valid
    mdl.io.in(0).bits  := pkg_Info_cache_s0s1(opMux.io.ld_deq.bits, stQueue.io.overlap.wdata, stQueue.io.overlap.wstrb)
    mdl.io.in(1) <> stQueue.io.deq
    mdl
  }

  /** according to the paddr, the request will be sent to cache, system-bus, periph-bus
    * @param in Info_cache_s0s1
    * @return Info_cache_s0s1
    */ 
  val regionMux = {
    val mdl = Module(new regionMux)
    mdl.io.enq <> ls_arb.io.out
    mdl
  }

  /** according to the paddr, the cache request will be divided into 4 or 8 (nm) "bank",
    * @note there are 2 "bank" defined here, which may make you confuse
    * @param in Info_cache_s0s1
    * @return Info_cache_s0s1
    */ 
  val cacheMux = {
    val mdl = Module(new cacheMux)
    mdl.io.enq <> regionMux.io.deq(0)
    mdl
  }


  /** there are nm cache bank 
    * @param in Info_cache_s0s1
    * @return Info_cache_retn
    */
  val cache = for ( i <- 0 until nm ) yield {
    val mdl = Module(new Dcache(edge(i)))
    mdl.io.enq <> cacheMux.io.deq(i)
    // mdl.io.overlap <> stQueue.io.overlap(i)

    io.missUnit_dcache_acquire(i).valid := mdl.io.missUnit_dcache_acquire.valid
    io.missUnit_dcache_acquire(i).bits := mdl.io.missUnit_dcache_acquire.bits
    mdl.io.missUnit_dcache_acquire.ready := io.missUnit_dcache_acquire(i).ready

    mdl.io.missUnit_dcache_grant.valid := io.missUnit_dcache_grant(i).valid
    mdl.io.missUnit_dcache_grant.bits  := io.missUnit_dcache_grant(i).bits
    io.missUnit_dcache_grant(i).ready := mdl.io.missUnit_dcache_grant.ready

    io.missUnit_dcache_grantAck(i).valid := mdl.io.missUnit_dcache_grantAck.valid
    io.missUnit_dcache_grantAck(i).bits := mdl.io.missUnit_dcache_grantAck.bits
    mdl.io.missUnit_dcache_grantAck.ready := io.missUnit_dcache_grantAck(i).ready

    mdl.io.probeUnit_dcache_probe.valid := io.probeUnit_dcache_probe(i).valid
    mdl.io.probeUnit_dcache_probe.bits := io.probeUnit_dcache_probe(i).bits
    io.probeUnit_dcache_probe(i).ready := mdl.io.probeUnit_dcache_probe.ready

    io.writeBackUnit_dcache_release(i).valid := mdl.io.writeBackUnit_dcache_release.valid
    io.writeBackUnit_dcache_release(i).bits := mdl.io.writeBackUnit_dcache_release.bits
    mdl.io.writeBackUnit_dcache_release.ready := io.writeBackUnit_dcache_release(i).ready

    mdl.io.writeBackUnit_dcache_grant.valid := io.writeBackUnit_dcache_grant(i).valid
    mdl.io.writeBackUnit_dcache_grant.bits := io.writeBackUnit_dcache_grant(i).bits
    io.writeBackUnit_dcache_grant(i).ready := mdl.io.writeBackUnit_dcache_grant.ready

    mdl.io.flush := io.flush

    mdl
  }

  val system = {
    val mdl = Module(new IO_Lsu(edge(nm+1), idx = nm+1))
    mdl.io.enq <> regionMux.io.deq(1)
    // mdl.io.overlap <> stQueue.io.overlap(nm+1)

    io.system_getPut.valid := mdl.io.getPut.valid
    io.system_getPut.bits := mdl.io.getPut.bits
    mdl.io.getPut.ready := io.system_getPut.ready

    mdl.io.access.valid := io.system_access.valid
    mdl.io.access.bits := io.system_access.bits
    io.system_access.ready := mdl.io.access.ready
    mdl
  }

  val periph = {
    val mdl = Module(new IO_Lsu(edge(nm), idx = nm))
    mdl.io.enq <> regionMux.io.deq(2)
    // mdl.io.overlap <> stQueue.io.overlap(nm)

    io.periph_getPut.valid := mdl.io.getPut.valid
    io.periph_getPut.bits := mdl.io.getPut.bits
    mdl.io.getPut.ready := io.periph_getPut.ready

    mdl.io.access.valid := io.periph_access.valid
    mdl.io.access.bits := io.periph_access.bits
    io.periph_access.ready := mdl.io.access.ready
    mdl
  }

  /** the load-and-amo operation write-back info from cache or bus
    * @param enq Vec[Info_cache_retn]
    * @return Info_cache_retn
    */
  val lu_wb_arb = {
    val mdl = Module(new Arbiter(new Info_cache_retn, nm+2))
    for ( i <- 0 until nm ) yield {
      mdl.io.in(i).valid := cache(i).io.deq.valid & cache(i).io.deq.bits.is_load_amo
      mdl.io.in(i).bits := Mux( mdl.io.in(i).valid, cache(i).io.deq.bits, 0.U.asTypeOf(new Info_cache_retn) )
      cache(i).io.deq.ready := mdl.io.in(i).ready | ~cache(i).io.deq.bits.is_load_amo
      when( cache(i).io.deq.bits.is_load_amo ) { assert(cache(i).io.deq.fire === mdl.io.in(i).fire) }
    }


    mdl.io.in(nm).valid := system.io.deq.valid & system.io.deq.bits.is_load_amo
    mdl.io.in(nm).bits := Mux( mdl.io.in(nm).valid, system.io.deq.bits, 0.U.asTypeOf(new Info_cache_retn) )
    system.io.deq.ready := mdl.io.in(nm).ready | ~system.io.deq.bits.is_load_amo
    when( system.io.deq.bits.is_load_amo ) { assert(system.io.deq.fire === mdl.io.in(nm).fire) }

    mdl.io.in(nm+1).valid := periph.io.deq.valid & periph.io.deq.bits.is_load_amo
    mdl.io.in(nm+1).bits := Mux( mdl.io.in(nm+1).valid, periph.io.deq.bits, 0.U.asTypeOf(new Info_cache_retn) )
    periph.io.deq.ready := mdl.io.in(nm+1).ready | ~periph.io.deq.bits.is_load_amo
    when( periph.io.deq.bits.is_load_amo ) { assert(periph.io.deq.fire === mdl.io.in(nm+1).fire) }


    mdl
  }

  /** the load-and-amo write-back fifo, 
    * @note overlap will be merge here
    * @note when trans_kill, the load result will be abort here to prevent write-back
    * @param in Info_cache_retn
    * @return WriteBack_info
    */
  val lu_wb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info(dw=64,dp=64), 1, false, true ) )
    mdl.io.enq.valid := lu_wb_arb.io.out.valid & lu_wb_arb.io.out.bits.is_iwb &  ~trans_kill 
    mdl.io.enq.bits.rd0 := lu_wb_arb.io.out.bits.wb.rd0
    mdl.io.enq.bits.res := lu_wb_arb.io.out.bits.wb.res
    
    mdl.reset := reset.asBool | io.flush
    mdl
  }

  val flu_wb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info(dw=65,dp=64), 1, false, true ) )
    mdl.io.enq.valid := lu_wb_arb.io.out.valid & lu_wb_arb.io.out.bits.is_fwb & ~trans_kill 
    mdl.io.enq.bits.rd0 := lu_wb_arb.io.out.bits.wb.rd0
    mdl.io.enq.bits.res := 
      Mux1H(Seq(
        lu_wb_arb.io.out.bits.is_flw -> box(recode(lu_wb_arb.io.out.bits.wb.res, 0), FType.D),
        lu_wb_arb.io.out.bits.is_fld -> box(recode(lu_wb_arb.io.out.bits.wb.res, 1), FType.D),
      ))
    mdl.io.deq <> io.lsu_exe_fwb
    mdl.reset := reset.asBool | io.flush
    mdl 
  }

  lu_wb_arb.io.out.ready := (lu_wb_fifo.io.enq.ready & flu_wb_fifo.io.enq.ready) | trans_kill

  /** store operations will write-back dircetly from opMux
    * @param in Lsu_iss_info
    * @return WriteBack_info
    */
  val su_wb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info(dw=64,dp=64), 1, false, true ) )
    mdl.io.enq.valid := opMux.io.st_deq.fire
    mdl.io.enq.bits.rd0  := opMux.io.st_deq.bits.param.rd0
    mdl.io.enq.bits.res := 0.U
    mdl.reset := reset.asBool | io.flush
    mdl
  }


  opMux.io.st_deq.ready := su_wb_fifo.io.enq.ready & stQueue.io.enq.ready
  opMux.io.am_deq.ready := stQueue.io.enq.ready
  opMux.io.ld_deq.ready := ls_arb.io.in(0).ready

  /** indicate the mem unit is empty by all seq-element is empty*/

  val is_empty = Wire(Bool())

  val fe_wb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info(dw=64,dp=64), 1, false, true ) )
    mdl.reset := reset.asBool | io.flush
    mdl.io.enq.valid := is_empty & is_fence_op & io.lsu_iss_exe.valid
    mdl.io.enq.bits.rd0 := io.lsu_iss_exe.bits.param.rd0
    mdl.io.enq.bits.res := 0.U
    mdl.reset := reset.asBool | io.flush
    mdl
  }

  is_empty := 
    stQueue.io.is_empty & 
    VecInit(cache.map{x => x.io.is_empty}).forall((x:Bool) => (x === true.B)) &
    system.io.is_empty &
    periph.io.is_empty &
    ~su_wb_fifo.io.deq.valid & 
    ~lu_wb_fifo.io.deq.valid// & 
    //~fe_wb_fifo.io.deq.valid //DontCare about fe_fifo

  /** merge lu-writeback and su-writeback
    * @param in WriteBack_info
    * @return WriteBack_info
    */
  val rtn_arb = {
    val mdl = Module(new Arbiter( new WriteBack_info(dw=64,dp=64), 3))
    mdl.io.in(0) <> su_wb_fifo.io.deq
    mdl.io.in(1) <> lu_wb_fifo.io.deq
    mdl.io.in(2) <> fe_wb_fifo.io.deq
    mdl.io.out <> io.lsu_exe_iwb
    mdl
  }





  io.lsu_mmu.valid := io.lsu_iss_exe.valid & ~io.lsu_iss_exe.bits.fun.is_fence
  io.lsu_mmu.bits.vaddr := io.lsu_iss_exe.bits.param.dat.op1
  io.lsu_mmu.bits.is_R := io.lsu_iss_exe.bits.fun.is_R
  io.lsu_mmu.bits.is_W := io.lsu_iss_exe.bits.fun.is_W
  io.lsu_mmu.bits.is_X := false.B
  io.lsu_iss_exe.ready :=
    ( ~io.lsu_iss_exe.bits.fun.is_fence & io.lsu_mmu.ready) |
    (  io.lsu_iss_exe.bits.fun.is_fence & fe_wb_fifo.io.enq.fire) 




  io.lsu_cmm.is_access_fault :=
    io.mmu_lsu.valid & io.mmu_lsu.bits.is_access_fault & is_empty

  io.lsu_cmm.is_paging_fault :=
    io.mmu_lsu.valid & io.mmu_lsu.bits.is_paging_fault & is_empty

  io.lsu_cmm.is_misAlign :=
    io.mmu_lsu.valid & io.lsu_iss_exe.bits.is_misAlign & is_empty

  io.lsu_cmm.trap_addr := io.lsu_iss_exe.bits.param.dat.op1







  when( io.flush ) { trans_kill := true.B }
  .elsewhen( is_empty ) { trans_kill := false.B }

  def is_fence      = io.lsu_iss_exe.bits.fun.fence
  def is_fence_i    = io.lsu_iss_exe.bits.fun.fence_i
  def is_sfence_vma = io.lsu_iss_exe.bits.fun.sfence_vma
  def is_fence_op = is_fence | is_fence_i | is_sfence_vma
  // when( io.lsu_iss_exe.valid & io.lsu_iss_exe.bits.fun.is_fence & ~is_fence_op & ~io.flush) {
  //   is_fence      := io.lsu_iss_exe.bits.fun.fence
  //   is_fence_i     := io.lsu_iss_exe.bits.fun.fence_i
  //   is_sfence_vma := io.lsu_iss_exe.bits.fun.sfence_vma
  // }
  // .elsewhen( (is_empty &  is_fence_op) | io.flush ) {
  //   is_fence      := false.B
  //   is_fence_i    := false.B
  //   is_sfence_vma := false.B
  // }

}

