

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

abstract class LsuBase (edge: Seq[TLEdgeOut])(implicit p: Parameters) extends DcacheModule with HasFPUParameters {
  val dEdge = edge

  val io = IO(new Bundle{
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val lsu_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64,dp=64))
    val lsu_exe_fwb = new DecoupledIO(new WriteBack_info(dw=65, dp=64))

    val cmm_lsu = Input(new Info_cmm_lsu)
    val lsu_cmm = Output( new Info_lsu_cmm )

    val lsu_mmu = DecoupledIO(new Info_mmu_req)
    val mmu_lsu = Flipped(DecoupledIO(new Info_mmu_rsp))

    val missUnit_dcache_acquire      = DecoupledIO(new TLBundleA(dEdge(0).bundle))
    val missUnit_dcache_grant        = Flipped(DecoupledIO(new TLBundleD(dEdge(0).bundle)))
    val missUnit_dcache_grantAck     = DecoupledIO(new TLBundleE(dEdge(0).bundle))
    val probeUnit_dcache_probe       = Flipped(DecoupledIO(new TLBundleB(dEdge(0).bundle)))
    val writeBackUnit_dcache_release = DecoupledIO(new TLBundleC(dEdge(0).bundle))
    val writeBackUnit_dcache_grant   = Flipped(DecoupledIO(new TLBundleD(dEdge(0).bundle)))

    val system_getPut = new DecoupledIO(new TLBundleA(dEdge(1).bundle))
    val system_access = Flipped(new DecoupledIO(new TLBundleD(dEdge(1).bundle)))

    val periph_getPut = new DecoupledIO(new TLBundleA(dEdge(2).bundle))
    val periph_access = Flipped(new DecoupledIO(new TLBundleD(dEdge(2).bundle)))

    val preFetch = ValidIO( new PreFetch_Req_Bundle )

    val flush = Input(Bool())
  })

  /** when a flush comes, flush all uncommit write & amo request in pending-fifo, and block all request from issue until scoreboard is empty */
  val trans_kill = RegInit(false.B)

  /** decoupled output of addrTrans */
  val addrTransIO = Wire(DecoupledIO(new Lsu_iss_info))

  /** decoupled output of OP_mux */
  val opStIO = Wire(DecoupledIO(new Lsu_iss_info))
  val opLdIO = Wire(DecoupledIO(new Lsu_iss_info))
  val opAmIO = Wire(DecoupledIO(new Lsu_iss_info) )

  /** for store and amo, they should be push into stQueue waiting for commited and pending commit */
  val stQueue = Module(new Store_queue)

  /** Merge the request from 1) opMux (load is bypassing the stQueue) 2) store and amo form stQueue */
  val ls_arb = Module(new Arbiter(new Lsu_iss_info, 2))

  val regionDCacheIO = Wire(DecoupledIO(new Lsu_iss_info) )
  val regionSystemIO = Wire(DecoupledIO(new Lsu_iss_info) )
  val regionPeriphIO = Wire(DecoupledIO(new Lsu_iss_info) )

  val cacheBankIO = Wire( Vec( bk, DecoupledIO(new Info_cache_s0s1) ) )

  val system = Module(new IO_Lsu(dEdge(2)))
  val periph = Module(new IO_Lsu(dEdge(1)))
  val cache = for ( i <- 0 until bk ) yield { val mdl = Module(new Dcache(dEdge(0), i)); mdl }

  /** the load-and-amo operation write-back info from cache or bus
    * @param enq Vec[Info_cache_retn]
    * @return Info_cache_retn
    */
  val lu_wb_arb = Module(new Arbiter(new Info_cache_retn, bk+2))

  /** the load-and-amo write-back fifo, flow !!!!!
    * @note when trans_kill, the load result will be abort here to prevent write-back
    * @param in Info_cache_retn
    * @return WriteBack_info 
    */
  val lu_wb_fifo = Module( new Queue( new WriteBack_info(dw=64,dp=64), 1, false, true ) )

  val flu_wb_fifo = Module( new Queue( new WriteBack_info(dw=65,dp=64), 1, false, true ) )

  /** store operations will write-back dircetly from opMux, flow !!!!!
    * @param in Lsu_iss_info
    * @return WriteBack_info
    */
  val su_wb_fifo = Module( new Queue( new WriteBack_info(dw=64,dp=64), 1, false, true ) )
  val fe_wb_fifo = Module( new Queue( new WriteBack_info(dw=64,dp=64), 1, false, true ) )

  /** merge lu-writeback and su-writeback
    * @param in WriteBack_info
    * @return WriteBack_info
    */
  val rtn_arb = Module(new Arbiter( new WriteBack_info(dw=64,dp=64), 3))

  /** indicate the mem unit is empty by all seq-element is empty*/
  val is_empty = Wire(Bool())
}

/** request mmu and get the paddr */
trait LSU_AddrTrans { this: LsuBase => 
  // addrTrans( io.lsu_iss_exe.bits, io.mmu_lsu, trans_kill | is_fence_op)

  /** the transation will be blocked if the request is illegal */
  addrTransIO.valid := io.mmu_lsu.valid & ~io.mmu_lsu.bits.is_fault & ~io.lsu_iss_exe.bits.is_misAlign & ~(trans_kill | io.lsu_iss_exe.bits.fun.is_fence)
  addrTransIO.bits := io.lsu_iss_exe.bits
  addrTransIO.bits.param.dat.op1 := io.mmu_lsu.bits.paddr
  io.mmu_lsu.ready := addrTransIO.ready & ~io.mmu_lsu.bits.is_fault & ~io.lsu_iss_exe.bits.is_misAlign & ~(trans_kill | io.lsu_iss_exe.bits.fun.is_fence)

  io.lsu_mmu.valid := io.lsu_iss_exe.valid & ~io.lsu_iss_exe.bits.fun.is_fence
  io.lsu_mmu.bits.vaddr := io.lsu_iss_exe.bits.param.dat.op1
  io.lsu_mmu.bits.is_R := io.lsu_iss_exe.bits.fun.is_R
  io.lsu_mmu.bits.is_W := io.lsu_iss_exe.bits.fun.is_W
  io.lsu_mmu.bits.is_X := false.B
  io.lsu_iss_exe.ready :=
    ( ~io.lsu_iss_exe.bits.fun.is_fence & io.lsu_mmu.ready) |
    (  io.lsu_iss_exe.bits.fun.is_fence & fe_wb_fifo.io.enq.fire) 

}

/** Mux store load amo request into three different ways */
trait LSU_OpMux { this: LsuBase => 
  /** route operation into load, store, and amo */
  // val opMux = {
  //   val mdl = Module(new OpMux)
  //   mdl.io.enq <> addrTrans( io.lsu_iss_exe.bits, io.mmu_lsu, trans_kill | is_fence_op)
  //   mdl
  // }
  addrTransIO.ready := false.B

  when( addrTransIO.bits.fun.is_lu ) {
    opLdIO.valid      := addrTransIO.valid
    opLdIO.bits       := addrTransIO.bits
    addrTransIO.ready := opLdIO.ready
  } .otherwise{
    opLdIO.valid := false.B
    opLdIO.bits  := 0.U.asTypeOf(new Lsu_iss_info)
  }

  when( addrTransIO.bits.fun.is_su ) {
    opStIO.valid      := addrTransIO.valid
    opStIO.bits       := addrTransIO.bits
    addrTransIO.ready := opStIO.ready
  } .otherwise{
    opStIO.valid := false.B
    opStIO.bits  := 0.U.asTypeOf(new Lsu_iss_info)
  }

  when( addrTransIO.bits.fun.is_amo ) {
    opAmIO.valid      := addrTransIO.valid
    opAmIO.bits       := addrTransIO.bits
    addrTransIO.ready := opAmIO.ready
  } .otherwise{
    opAmIO.valid := false.B
    opAmIO.bits  := 0.U.asTypeOf(new Lsu_iss_info)
  }

}

/** for store and amo, they should be push into stQueue waiting for commited and pending commit */
trait LSU_StQueue { this: LsuBase =>

  stQueue.io.enq.valid := opStIO.fire | opAmIO.fire
  stQueue.io.enq.bits :=
    Mux1H(Seq(
      opStIO.fire -> opStIO.bits,
      opAmIO.fire -> opAmIO.bits,
    ))
  stQueue.io.cmm_lsu := io.cmm_lsu
  io.preFetch := stQueue.io.preFetch
  stQueue.io.flush := io.flush
}

/** Merge the request from 1) opMux (load is bypassing the stQueue) 2) store and amo form stQueue */
trait LSU_LsArb { this: LsuBase =>


  // /** the raw req that will be merged
  //   * @param in Info_cache_s0s1
  //   * @return Info_cache_s0s1
  //   */
  // val ls_arb = {
  //   val mdl = Module(new Arbiter(new Lsu_iss_info, 2))
  //   stQueue.io.overlapReq.bits.paddr := opMux.io.ld_deq.bits.param.dat.op1
  //   stQueue.io.overlapReq.valid := opMux.io.ld_deq.valid

  //   mdl.io.in(0).valid := opMux.io.ld_deq.valid & stQueue.io.overlapResp.valid
  //   mdl.io.in(0).bits := opMux.io.ld_deq.bits
  //   opMux.io.ld_deq.ready := mdl.io.in(0).ready & stQueue.io.overlapResp.valid
    
  //   mdl.io.in(1) <> stQueue.io.deq
  //   mdl
  // }

  stQueue.io.overlapReq.bits.paddr := opLdIO.bits.param.dat.op1
  stQueue.io.overlapReq.valid := opLdIO.valid

  ls_arb.io.in(0).valid := opLdIO.valid & stQueue.io.overlapResp.valid
  ls_arb.io.in(0).bits := opLdIO.bits
  opLdIO.ready := ls_arb.io.in(0).ready & stQueue.io.overlapResp.valid
  
  ls_arb.io.in(1) <> stQueue.io.deq

}

/** Mux request to three different region, cache, system, periph depending on paddr */
trait LSU_RegionMux { this: LsuBase =>
  // /** according to the paddr, the request will be sent to cache, system-bus, periph-bus
  //   * @param in Info_cache_s0s1
  //   * @return Info_cache_s0s1
  //   */ 

  // val regionMux = {
  //   val mdl = Module(new regionMux)
  //   mdl.io.enq <> ls_arb.io.out
  //   mdl
  // }
  val psel = ls_arb.io.out.bits.param.dat.op1(31,28)
  val sel = Mux( psel(3), 0.U, Mux( psel(2), 1.U, 2.U) )


  ls_arb.io.out.ready := false.B

  when( sel === 2.U ) {
    regionPeriphIO.valid := ls_arb.io.out.valid
    regionPeriphIO.bits  := ls_arb.io.out.bits
    ls_arb.io.out.ready  := regionPeriphIO.ready 
  } .otherwise {
    regionPeriphIO.valid := false.B
    regionPeriphIO.bits  := 0.U.asTypeOf(new Lsu_iss_info)
  }

  when( sel === 1.U ) {
    regionSystemIO.valid := ls_arb.io.out.valid
    regionSystemIO.bits  := ls_arb.io.out.bits
    ls_arb.io.out.ready  := regionSystemIO.ready
  } .otherwise {
    regionSystemIO.valid := false.B
    regionSystemIO.bits  := 0.U.asTypeOf(new Lsu_iss_info)
  }

  when( sel === 0.U ) {
    regionDCacheIO.valid := ls_arb.io.out.valid
    regionDCacheIO.bits  := ls_arb.io.out.bits
    ls_arb.io.out.ready  := regionDCacheIO.ready
  } .otherwise {
    regionDCacheIO.valid := false.B
    regionDCacheIO.bits  := 0.U.asTypeOf(new Lsu_iss_info)
  }
}

/** depending on the paddr, the cache request will be divided into 4 or 8 (nm) "bank" */
trait LSU_CacheMux { this: LsuBase =>
  val CacheMuxBits = pkg_Info_cache_s0s1(regionDCacheIO.bits, stQueue.io.overlapReq.bits, stQueue.io.overlapResp.bits)

  //   val cacheMux = {
  //   val mdl = Module(new cacheMux)
  //   mdl.io.enq.bits := 
  //   mdl.io.enq.valid := regionDCacheIO.valid
  //   regionDCacheIO.ready := mdl.io.enq.ready
  //   mdl
  // }

  // class cacheMux(implicit p: Parameters) extends DcacheModule{
  //   val io = IO(new Bundle{
  //     val enq = Flipped(new DecoupledIO(new Info_cache_s0s1))
  //     val deq = Vec(bk, new DecoupledIO(new Info_cache_s0s1))
  //   })

  val chn = CacheMuxBits.paddr(addr_lsb+bk_w-1,addr_lsb)

  regionDCacheIO.ready := false.B
  
  for ( i <- 0 until bk ) yield {
    when( i.U === chn ) {
      cacheBankIO(i).valid := regionDCacheIO.valid
      cacheBankIO(i).bits  := CacheMuxBits
      regionDCacheIO.ready := cacheBankIO(i).ready
    } .otherwise {
      cacheBankIO(i).valid := false.B
      cacheBankIO(i).bits  := 0.U.asTypeOf(new Info_cache_s0s1)
    }
  }

  io.missUnit_dcache_acquire.valid := false.B
  io.missUnit_dcache_acquire.bits := 0.U.asTypeOf(new TLBundleA(dEdge(0).bundle))
  io.missUnit_dcache_grant.ready := false.B
  io.missUnit_dcache_grantAck.valid := false.B
  io.missUnit_dcache_grantAck.bits := 0.U.asTypeOf(new TLBundleE(dEdge(0).bundle))
  io.probeUnit_dcache_probe.ready := false.B
  io.writeBackUnit_dcache_release.valid := false.B
  io.writeBackUnit_dcache_release.bits := 0.U.asTypeOf(new TLBundleC(dEdge(0).bundle))
  io.writeBackUnit_dcache_grant.ready := false.B

  for ( i <- 0 until bk ) yield {
    cache(i).io.missUnit_dcache_acquire.ready  := false.B
    cache(i).io.missUnit_dcache_grant.valid    := false.B
    cache(i).io.missUnit_dcache_grant.bits     := 0.U.asTypeOf(new TLBundleD(dEdge(0).bundle))
    cache(i).io.missUnit_dcache_grantAck.ready := false.B
    cache(i).io.probeUnit_dcache_probe.valid   := false.B
    cache(i).io.probeUnit_dcache_probe.bits    := 0.U.asTypeOf(new TLBundleB(dEdge(0).bundle))
    cache(i).io.writeBackUnit_dcache_release.ready := false.B
    cache(i).io.writeBackUnit_dcache_grant.valid   := false.B
    cache(i).io.writeBackUnit_dcache_grant.bits    := 0.U.asTypeOf(new TLBundleD(dEdge(0).bundle))
  }


  for ( i <- bk-1 to 0 by -1 ) {
    when( cache(i).io.missUnit_dcache_acquire.valid ) {
      io.missUnit_dcache_acquire <> cache(i).io.missUnit_dcache_acquire
    }


    when( io.missUnit_dcache_grant.bits.source === i.U ) {
      cache(i).io.missUnit_dcache_grant <> io.missUnit_dcache_grant  
    }

    when( cache(i).io.missUnit_dcache_grantAck.valid ) {
      io.missUnit_dcache_grantAck <> cache(i).io.missUnit_dcache_grantAck   
    }

    when( io.probeUnit_dcache_probe.bits.address(addr_lsb+bk_w-1,addr_lsb) === i.U ) {
      cache(i).io.probeUnit_dcache_probe <> io.probeUnit_dcache_probe   
    }

    when( io.writeBackUnit_dcache_grant.bits.source === i.U ) {
      cache(i).io.writeBackUnit_dcache_grant <> io.writeBackUnit_dcache_grant    
    }

  }

  /** @note chn-C will carry multi-beat data from client to manager, which is a N to 1 transation, and may be grabbed
    * @note however there will be one cycle latency for chn-c
    */
  val is_chnc_busy = RegInit(VecInit(Seq.fill(bk)(false.B)))
  val is_release_done = for ( i <- 0 until bk ) yield { dEdge(0).count(cache(i).io.writeBackUnit_dcache_release)._3 }
  for( i <- 0 until bk ) yield {
    when( cache(i).io.writeBackUnit_dcache_release.fire ) { is_chnc_busy(i) := Mux( is_release_done(i), false.B, true.B )}
  }

  when( is_chnc_busy.forall((x:Bool) => (x === false.B)) ) {
    for ( i <- bk-1 to 0 by -1 ) {
      when( cache(i).io.writeBackUnit_dcache_release.valid ) { io.writeBackUnit_dcache_release <> cache(i).io.writeBackUnit_dcache_release }
    }
  } .otherwise {
    for ( i <- 0 until bk ) {
      when( is_chnc_busy(i) === true.B  ) { io.writeBackUnit_dcache_release <> cache(i).io.writeBackUnit_dcache_release }
    }
  }
  assert( PopCount(is_chnc_busy) <= 1.U, "Assert Failed at dcache chn-c Mux, sel should be one-hot" )


}


trait LSU_Mem { this: LsuBase =>

  for ( i <- 0 until bk ) {
    cache(i).io.enq.valid := cacheBankIO(i).valid
    cache(i).io.enq.bits  := cacheBankIO(i).bits
    cacheBankIO(i).ready  := cache(i).io.enq.ready
    cache(i).io.flush := io.flush    
  }

  system.io.enq.valid  := regionSystemIO.valid
  system.io.enq.bits   := regionSystemIO.bits
  regionSystemIO.ready := system.io.enq.ready

  io.system_getPut.valid := system.io.getPut.valid
  io.system_getPut.bits  := system.io.getPut.bits
  system.io.getPut.ready := io.system_getPut.ready
  system.io.access.valid := io.system_access.valid
  system.io.access.bits  := io.system_access.bits
  io.system_access.ready := system.io.access.ready

  periph.io.enq.valid  := regionPeriphIO.valid
  periph.io.enq.bits   := regionPeriphIO.bits
  regionPeriphIO.ready := periph.io.enq.ready

  io.periph_getPut.valid := periph.io.getPut.valid
  io.periph_getPut.bits  := periph.io.getPut.bits
  periph.io.getPut.ready := io.periph_getPut.ready
  periph.io.access.valid := io.periph_access.valid
  periph.io.access.bits  := io.periph_access.bits
  io.periph_access.ready := periph.io.access.ready

}


trait LSU_WriteBack { this: LsuBase =>
  when( io.flush ) { trans_kill := true.B }
  .elsewhen( is_empty ) { trans_kill := false.B }





  for ( i <- 0 until bk ) yield {
    lu_wb_arb.io.in(i).valid := cache(i).io.deq.valid & cache(i).io.deq.bits.is_load_amo
    lu_wb_arb.io.in(i).bits := Mux( lu_wb_arb.io.in(i).valid, cache(i).io.deq.bits, 0.U.asTypeOf(new Info_cache_retn) )
    cache(i).io.deq.ready := lu_wb_arb.io.in(i).ready | ~cache(i).io.deq.bits.is_load_amo
    when( cache(i).io.deq.bits.is_load_amo ) { assert(cache(i).io.deq.fire === lu_wb_arb.io.in(i).fire) }
  }

  lu_wb_arb.io.in(bk).valid := system.io.deq.valid & system.io.deq.bits.is_load_amo
  lu_wb_arb.io.in(bk).bits := Mux( lu_wb_arb.io.in(bk).valid, system.io.deq.bits, 0.U.asTypeOf(new Info_cache_retn) )
  system.io.deq.ready := lu_wb_arb.io.in(bk).ready | ~system.io.deq.bits.is_load_amo
  when( system.io.deq.bits.is_load_amo ) { assert(system.io.deq.fire === lu_wb_arb.io.in(bk).fire) }

  lu_wb_arb.io.in(bk+1).valid := periph.io.deq.valid & periph.io.deq.bits.is_load_amo
  lu_wb_arb.io.in(bk+1).bits := Mux( lu_wb_arb.io.in(bk+1).valid, periph.io.deq.bits, 0.U.asTypeOf(new Info_cache_retn) )
  periph.io.deq.ready := lu_wb_arb.io.in(bk+1).ready | ~periph.io.deq.bits.is_load_amo
  when( periph.io.deq.bits.is_load_amo ) { assert(periph.io.deq.fire === lu_wb_arb.io.in(bk+1).fire) }



  lu_wb_fifo.io.enq.valid := lu_wb_arb.io.out.valid & lu_wb_arb.io.out.bits.is_iwb &  ~trans_kill 
  lu_wb_fifo.io.enq.bits.rd0 := lu_wb_arb.io.out.bits.wb.rd0
  lu_wb_fifo.io.enq.bits.res := lu_wb_arb.io.out.bits.wb.res
  lu_wb_fifo.reset := reset.asBool | io.flush


  flu_wb_fifo.io.enq.valid := lu_wb_arb.io.out.valid & lu_wb_arb.io.out.bits.is_fwb & ~trans_kill 
  flu_wb_fifo.io.enq.bits.rd0 := lu_wb_arb.io.out.bits.wb.rd0
  flu_wb_fifo.io.enq.bits.res := 
      Mux1H(Seq(
        lu_wb_arb.io.out.bits.is_flw -> box(recode(lu_wb_arb.io.out.bits.wb.res, 0), FType.D),
        lu_wb_arb.io.out.bits.is_fld -> box(recode(lu_wb_arb.io.out.bits.wb.res, 1), FType.D),
      ))
  flu_wb_fifo.io.deq <> io.lsu_exe_fwb
  flu_wb_fifo.reset := reset.asBool | io.flush


  lu_wb_arb.io.out.ready := (lu_wb_fifo.io.enq.ready & flu_wb_fifo.io.enq.ready) | trans_kill


  su_wb_fifo.io.enq.valid    := opStIO.fire
  su_wb_fifo.io.enq.bits.rd0 := opStIO.bits.param.rd0
  su_wb_fifo.io.enq.bits.res := 0.U
  su_wb_fifo.reset := reset.asBool | io.flush

  opStIO.ready := su_wb_fifo.io.enq.ready & stQueue.io.enq.ready
  opAmIO.ready := stQueue.io.enq.ready
  

  fe_wb_fifo.reset := reset.asBool | io.flush
  fe_wb_fifo.io.enq.valid := is_empty & io.lsu_iss_exe.bits.fun.is_fence & io.lsu_iss_exe.valid
  fe_wb_fifo.io.enq.bits.rd0 := io.lsu_iss_exe.bits.param.rd0
  fe_wb_fifo.io.enq.bits.res := 0.U
  fe_wb_fifo.reset := reset.asBool | io.flush



  rtn_arb.io.in(0) <> su_wb_fifo.io.deq
  rtn_arb.io.in(1) <> lu_wb_fifo.io.deq
  rtn_arb.io.in(2) <> fe_wb_fifo.io.deq
  rtn_arb.io.out <> io.lsu_exe_iwb


}



trait LSU_Fault { this: LsuBase =>
  val isAccessFaultReg = RegInit(false.B)
  val isPagingFault    = RegInit(false.B)
  val isMisAlign = RegInit(false.B)
  val trapAddrReg = Reg(UInt(64.W))

  when( io.flush ) {
    isAccessFaultReg := false.B
    isPagingFault    := false.B
    isMisAlign       := false.B    
  } .elsewhen( io.mmu_lsu.valid & is_empty ) {
    isAccessFaultReg := io.mmu_lsu.bits.is_access_fault
    isPagingFault    := io.mmu_lsu.bits.is_paging_fault
    isMisAlign       := io.lsu_iss_exe.bits.is_misAlign
    trapAddrReg      :=  io.lsu_iss_exe.bits.param.dat.op1
  }

  io.lsu_cmm.is_access_fault := isAccessFaultReg
  io.lsu_cmm.is_paging_fault := isPagingFault
  io.lsu_cmm.is_misAlign     := isMisAlign
  io.lsu_cmm.trap_addr       := trapAddrReg

















}

class Lsu(edge: Seq[TLEdgeOut])(implicit p: Parameters) extends LsuBase(edge)
  with LSU_AddrTrans
  with LSU_OpMux
  with LSU_StQueue
  with LSU_LsArb
  with LSU_RegionMux
  with LSU_CacheMux
  with LSU_Mem
  with LSU_WriteBack
  with LSU_Fault{

  is_empty := 
    stQueue.io.is_empty & 
    VecInit(cache.map{x => x.io.is_empty}).forall((x:Bool) => (x === true.B)) &
    system.io.is_empty &
    periph.io.is_empty &
    ~su_wb_fifo.io.deq.valid & 
    ~lu_wb_fifo.io.deq.valid// & 
    //~fe_wb_fifo.io.deq.valid //DontCare about fe_fifo
}
  