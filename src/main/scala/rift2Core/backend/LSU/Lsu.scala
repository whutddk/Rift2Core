

/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

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
import rift2Chip._

import rift2Core.privilege._
import rift2Core.backend._

import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._
import chisel3.experimental.dataview._

class LSU_WriteBack_Bundle(dw: Int)(implicit p: Parameters) extends WriteBack_info(dw = 64){
  val vAttach = if(hasVector) {Some(new VDcache_Attach_Bundle)} else {None}
}

abstract class LsuBase (edge: Seq[TLEdgeOut])(implicit p: Parameters) extends DcacheModule with HasFPUParameters {
  val dEdge = edge

  class LsuIO extends Bundle{
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val lsu_exe_iwb = new DecoupledIO(new LSU_WriteBack_Bundle(dw=64))
    val lsu_exe_fwb = new DecoupledIO(new WriteBack_info(dw=65))

    val lsu_cWriteBack = Valid(new SeqReg_WriteBack_Bundle(64, cRegNum))

    val cmm_lsu = Input(new Info_cmm_lsu)
                    // val lsu_cmm = Output( new Info_lsu_cmm )

    val lsu_mmu = DecoupledIO(new Info_mmu_req)
    val mmu_lsu = Flipped(DecoupledIO(new Info_mmu_rsp))


    val missUnit_dcache_acquire      = if( hasL2 ) Some(new DecoupledIO(new TLBundleA(dEdge(0).bundle)))          else {None}
    val missUnit_dcache_grant        = if( hasL2 ) Some(Flipped(new DecoupledIO(new TLBundleD(dEdge(0).bundle)))) else {None}
    val missUnit_dcache_grantAck     = if( hasL2 ) Some(DecoupledIO(new TLBundleE(dEdge(0).bundle)))              else {None}
    val probeUnit_dcache_probe       = if( hasL2 ) Some(Flipped(new DecoupledIO(new TLBundleB(dEdge(0).bundle)))) else {None}
    val writeBackUnit_dcache_release = if( hasL2 ) Some(new DecoupledIO(new TLBundleC(dEdge(0).bundle))         ) else {None}
    val writeBackUnit_dcache_grant   = if( hasL2 ) Some(Flipped(new DecoupledIO(new TLBundleD(dEdge(0).bundle)))) else {None}

    val dcache_getPut = if ( hasL2 ) { None } else { Some(        new DecoupledIO(new TLBundleA(dEdge(0).bundle)) ) }
    val dcache_access = if ( hasL2 ) { None } else { Some(Flipped(new DecoupledIO(new TLBundleD(dEdge(0).bundle)))) }


    val system_getPut = new DecoupledIO(new TLBundleA(dEdge(1).bundle))
    val system_access = Flipped(new DecoupledIO(new TLBundleD(dEdge(1).bundle)))

    val periph_getPut = new DecoupledIO(new TLBundleA(dEdge(2).bundle))
    val periph_access = Flipped(new DecoupledIO(new TLBundleD(dEdge(2).bundle)))

    val preFetch = ValidIO( new PreFetch_Req_Bundle )

    val flush = Input(Bool())    
  }


  val io: LsuIO = IO(new LsuIO)

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

  val cacheBankIO = Wire( Vec( bk, DecoupledIO(new Dcache_Enq_Bundle) ) )

  val system = Module(new IO_Lsu(dEdge(2)))
  val periph = Module(new IO_Lsu(dEdge(1)))
  val cache = for ( i <- 0 until bk ) yield { val mdl = Module(new Dcache(dEdge(0), i)); mdl }

  /** the load-and-amo operation write-back info from cache or bus
    * @param enq Vec[Dcache_Deq_Bundle]
    * @return Dcache_Deq_Bundle
    */
  val lu_wb_arb = Module(new Arbiter(new Dcache_Deq_Bundle, bk+2))

  /** the load-and-amo write-back fifo, flow !!!!!
    * @note when trans_kill, the load result will be abort here to prevent write-back
    * @param in Dcache_Deq_Bundle
    * @return WriteBack_info 
    */
  val lu_wb_fifo = Module( new Queue( new LSU_WriteBack_Bundle(dw=64), 1, false, true ) )

  val flu_wb_fifo = Module( new Queue( new WriteBack_info(dw=65), 1, false, true ) )

  /** store operations will write-back dircetly from opMux, flow !!!!!
    * @param in Lsu_iss_info
    * @return WriteBack_info
    */
  val su_wb_fifo = Module( new Queue( new LSU_WriteBack_Bundle(dw=64), 1, false, true ) )
  val fe_wb_fifo = Module( new Queue( new LSU_WriteBack_Bundle(dw=64), 1, false, true ) )

  /** merge lu-writeback and su-writeback
    * @param in WriteBack_info
    * @return WriteBack_info
    */
  val irtn_arb = Module(new Arbiter( new LSU_WriteBack_Bundle(dw=64), 4))
  val frtn_arb = Module(new Arbiter( new WriteBack_info(dw=65), 2))

  /** indicate the mem unit is empty by all seq-element is empty*/
  val is_empty = Wire(Bool())



  /** package write and amo operation*/
  def pkg_Dcache_Enq_Bundle( ori: Lsu_iss_info, overlapReq: Stq_req_Bundle, overlapResp: Stq_resp_Bundle)(implicit p: Parameters) = {

    val res = Wire(new Dcache_Enq_Bundle)
    val dw = res.wdata.getWidth

    res.paddr := ori.paddr
    res.wdata := Mux( ori.fun.is_lu, reAlign_data( from = 64, to = dw, data = overlapResp.wdata, addr = overlapReq.paddr ), ori.wdata_align(dw))
    res.wstrb := Mux( ori.fun.is_lu, reAlign_strb( from = 64, to = dw, strb = overlapResp.wstrb, addr = overlapReq.paddr ), ori.wstrb_align(dw))

    {
      res.fun := 0.U.asTypeOf(new Cache_op)
      res.fun.viewAsSupertype(new Lsu_isa) := ori.fun.viewAsSupertype(new Lsu_isa)

    }
    res.rd.rd0 := ori.param.rd0

    res.chkIdx := 0.U

    if( hasVector ){
      res.vAttach.get := ori.vAttach.get
    }
 



    res
  
  }
}

/** request mmu and get the paddr */
trait LSU_AddrTrans { this: LsuBase => 

  /** the transation will be blocked if the request is illegal */
  addrTransIO.valid := io.mmu_lsu.valid & ~io.mmu_lsu.bits.is_fault & ~io.lsu_iss_exe.bits.is_misAlign & ~(trans_kill | io.lsu_iss_exe.bits.fun.is_fence)
  addrTransIO.bits := io.lsu_iss_exe.bits
  addrTransIO.bits.param.dat.op1 := io.mmu_lsu.bits.paddr
  io.mmu_lsu.ready :=
    ( addrTransIO.ready & ~io.mmu_lsu.bits.is_fault & ~io.lsu_iss_exe.bits.is_misAlign & ~(trans_kill | io.lsu_iss_exe.bits.fun.is_fence) ) |
    ( irtn_arb.io.in(3).fire ) |
    ( frtn_arb.io.in(1).fire )


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

  stQueue.io.overlapReq.bits.paddr := opLdIO.bits.param.dat.op1
  stQueue.io.overlapReq.valid := opLdIO.valid

  ls_arb.io.in(0).valid := opLdIO.valid & stQueue.io.overlapResp.valid
  ls_arb.io.in(0).bits := opLdIO.bits
  opLdIO.ready := ls_arb.io.in(0).ready & stQueue.io.overlapResp.valid
  
  ls_arb.io.in(1) <> stQueue.io.deq

}

/** Mux request to three different region, cache, system, periph depending on paddr */
trait LSU_RegionMux { this: LsuBase =>

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
  println("Warning, different Cache Bank is requesting one by one!")
  val CacheMuxBits = pkg_Dcache_Enq_Bundle(regionDCacheIO.bits, stQueue.io.overlapReq.bits, stQueue.io.overlapResp.bits)


  val chn = if( bk > 1 ) { CacheMuxBits.paddr(addr_lsb+bk_w-1,addr_lsb) } else { 0.U }

  regionDCacheIO.ready := false.B
  
  for ( i <- 0 until bk ) yield {
    when( i.U === chn ) {
      cacheBankIO(i).valid := regionDCacheIO.valid
      cacheBankIO(i).bits  := CacheMuxBits
      regionDCacheIO.ready := cacheBankIO(i).ready
    } .otherwise {
      cacheBankIO(i).valid := false.B
      cacheBankIO(i).bits  := 0.U.asTypeOf(new Dcache_Enq_Bundle)
    }
  }

  if( hasL2 ) {
    val acquireArb  = Module(new Arbiter(new TLBundleA(dEdge(0).bundle), n = bk))
    val grantAckArb = Module(new Arbiter(new TLBundleE(dEdge(0).bundle), n = bk))
    val releaseArb  = Module(new Arbiter(new TLBundleC(dEdge(0).bundle), n = bk))
    io.missUnit_dcache_acquire.get.valid := false.B
    io.missUnit_dcache_acquire.get.bits := 0.U.asTypeOf(new TLBundleA(dEdge(0).bundle))
    io.missUnit_dcache_grant.get.ready := false.B
    io.missUnit_dcache_grantAck.get.valid := false.B
    io.missUnit_dcache_grantAck.get.bits := 0.U.asTypeOf(new TLBundleE(dEdge(0).bundle))
    io.probeUnit_dcache_probe.get.ready := false.B
    io.writeBackUnit_dcache_release.get.valid := false.B
    io.writeBackUnit_dcache_release.get.bits := 0.U.asTypeOf(new TLBundleC(dEdge(0).bundle))
    io.writeBackUnit_dcache_grant.get.ready := false.B

    for ( i <- 0 until bk ) yield {
      cache(i).io.missUnit_dcache_acquire.get.ready  := false.B
      cache(i).io.missUnit_dcache_grant.get.valid    := false.B
      cache(i).io.missUnit_dcache_grant.get.bits     := 0.U.asTypeOf(new TLBundleD(dEdge(0).bundle))
      cache(i).io.missUnit_dcache_grantAck.get.ready := false.B
      cache(i).io.probeUnit_dcache_probe.get.valid   := false.B
      cache(i).io.probeUnit_dcache_probe.get.bits    := 0.U.asTypeOf(new TLBundleB(dEdge(0).bundle))
      cache(i).io.writeBackUnit_dcache_release.get.ready := false.B
      cache(i).io.writeBackUnit_dcache_grant.get.valid   := false.B
      cache(i).io.writeBackUnit_dcache_grant.get.bits    := 0.U.asTypeOf(new TLBundleD(dEdge(0).bundle))
    }

    io.missUnit_dcache_acquire.get  <>  acquireArb.io.out
    io.missUnit_dcache_grantAck.get <> grantAckArb.io.out
    for ( i <- 0 until bk ) {
      acquireArb.io.in(i) <> cache(i).io.missUnit_dcache_acquire.get

      when( io.missUnit_dcache_grant.get.bits.source === i.U ) {
        cache(i).io.missUnit_dcache_grant.get <> io.missUnit_dcache_grant.get
      }

      grantAckArb.io.in(i) <> cache(i).io.missUnit_dcache_grantAck.get

      when( (if( bk > 1 ) {io.probeUnit_dcache_probe.get.bits.address(addr_lsb+bk_w-1,addr_lsb)} else {0.U}) === i.U ) {
        cache(i).io.probeUnit_dcache_probe.get <> io.probeUnit_dcache_probe.get  
      }

      when( io.writeBackUnit_dcache_grant.get.bits.source === i.U ) {
        cache(i).io.writeBackUnit_dcache_grant.get <> io.writeBackUnit_dcache_grant.get
      }
    }

      /** @note chn-C will carry multi-beat data from client to manager, which is a N to 1 transation, and may be grabbed
        * @note however there will be one cycle latency for chn-c
        */
      val is_chnc_busy = RegInit(VecInit(Seq.fill(bk)(false.B)))
      val is_release_done = for ( i <- 0 until bk ) yield { dEdge(0).count(cache(i).io.writeBackUnit_dcache_release.get)._3 }
      for( i <- 0 until bk ) yield {
        when( cache(i).io.writeBackUnit_dcache_release.get.fire ) { is_chnc_busy(i) := Mux( is_release_done(i), false.B, true.B )}
      }

      for ( i <- 0 until bk ) {
        releaseArb.io.in(i) <> cache(i).io.writeBackUnit_dcache_release.get
      }
      when( is_chnc_busy.forall((x:Bool) => (x === false.B)) ) {
        io.writeBackUnit_dcache_release.get <> releaseArb.io.out
      } .otherwise {
        releaseArb.io.out.ready := false.B
        for ( i <- 0 until bk ) {
          when( is_chnc_busy(i) === true.B  ) { io.writeBackUnit_dcache_release.get <> cache(i).io.writeBackUnit_dcache_release.get }
        }
      }
      assert( PopCount(is_chnc_busy) <= 1.U, "Assert Failed at dcache chn-c Mux, sel should be one-hot" )
  } else {
    // val dcache_getPut = if ( hasL2 ) { None } else { Some(        new DecoupledIO(new TLBundleA(edge.bundle)) ) }
    // val dcache_access = if ( hasL2 ) { None } else { Some(Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))) }
    val getPutArb  = Module(new Arbiter(new TLBundleA(dEdge(0).bundle), n = bk))

    io.dcache_getPut.get.valid := false.B
    io.dcache_getPut.get.bits := 0.U.asTypeOf(new TLBundleA(dEdge(0).bundle))
    io.dcache_access.get.ready := false.B

    for ( i <- 0 until bk ) yield {
      cache(i).io.dcache_getPut.get.ready  := false.B
      cache(i).io.dcache_access.get.valid  := false.B
      cache(i).io.dcache_access.get.bits   := 0.U.asTypeOf(new TLBundleD(dEdge(0).bundle))
    }

    io.dcache_getPut.get  <>  getPutArb.io.out

    for ( i <- 0 until bk ) {
      getPutArb.io.in(i) <> cache(i).io.dcache_getPut.get

      when( io.dcache_access.get.bits.source === i.U ) {
        cache(i).io.dcache_access.get <> io.dcache_access.get
      }
    }    
  }





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
    lu_wb_arb.io.in(i).bits := Mux( lu_wb_arb.io.in(i).valid, cache(i).io.deq.bits, 0.U.asTypeOf(new Dcache_Deq_Bundle) )
    cache(i).io.deq.ready := lu_wb_arb.io.in(i).ready | ~cache(i).io.deq.bits.is_load_amo
    when( cache(i).io.deq.bits.is_load_amo ) { assert(cache(i).io.deq.fire === lu_wb_arb.io.in(i).fire) }
  }

  lu_wb_arb.io.in(bk).valid := system.io.deq.valid & system.io.deq.bits.is_load_amo
  lu_wb_arb.io.in(bk).bits := Mux( lu_wb_arb.io.in(bk).valid, system.io.deq.bits, 0.U.asTypeOf(new Dcache_Deq_Bundle) )
  system.io.deq.ready := lu_wb_arb.io.in(bk).ready | ~system.io.deq.bits.is_load_amo
  when( system.io.deq.bits.is_load_amo ) { assert(system.io.deq.fire === lu_wb_arb.io.in(bk).fire) }

  lu_wb_arb.io.in(bk+1).valid := periph.io.deq.valid & periph.io.deq.bits.is_load_amo
  lu_wb_arb.io.in(bk+1).bits := Mux( lu_wb_arb.io.in(bk+1).valid, periph.io.deq.bits, 0.U.asTypeOf(new Dcache_Deq_Bundle) )
  periph.io.deq.ready := lu_wb_arb.io.in(bk+1).ready | ~periph.io.deq.bits.is_load_amo
  when( periph.io.deq.bits.is_load_amo ) { assert(periph.io.deq.fire === lu_wb_arb.io.in(bk+1).fire) }



  lu_wb_fifo.io.enq.valid := lu_wb_arb.io.out.valid & lu_wb_arb.io.out.bits.is_iwb &  ~trans_kill 
  lu_wb_fifo.io.enq.bits.rd0 := lu_wb_arb.io.out.bits.wb.rd0
  lu_wb_fifo.io.enq.bits.res := lu_wb_arb.io.out.bits.wb.res
  if(hasVector) {lu_wb_fifo.io.enq.bits.vAttach.get := lu_wb_arb.io.out.bits.vAttach.get}
  lu_wb_fifo.reset := reset.asBool | io.flush


  flu_wb_fifo.io.enq.valid := lu_wb_arb.io.out.valid & lu_wb_arb.io.out.bits.is_fwb & ~trans_kill 
  flu_wb_fifo.io.enq.bits.rd0 := lu_wb_arb.io.out.bits.wb.rd0
  flu_wb_fifo.io.enq.bits.res := 
      Mux1H(Seq(
        lu_wb_arb.io.out.bits.is_flw -> box(recode(lu_wb_arb.io.out.bits.wb.res, 0), FType.D),
        lu_wb_arb.io.out.bits.is_fld -> box(recode(lu_wb_arb.io.out.bits.wb.res, 1), FType.D),
      ))
  flu_wb_fifo.io.deq <> frtn_arb.io.in(0)
  flu_wb_fifo.reset := reset.asBool | io.flush


  lu_wb_arb.io.out.ready := (lu_wb_fifo.io.enq.ready & flu_wb_fifo.io.enq.ready) | trans_kill


  su_wb_fifo.io.enq.valid    := opStIO.fire
  su_wb_fifo.io.enq.bits.rd0 := opStIO.bits.param.rd0
  su_wb_fifo.io.enq.bits.res := 0.U
  if(hasVector) {su_wb_fifo.io.enq.bits.vAttach.get := opStIO.bits.vAttach.get}
  su_wb_fifo.reset := reset.asBool | io.flush

  opStIO.ready := su_wb_fifo.io.enq.ready & stQueue.io.enq.ready
  opAmIO.ready := stQueue.io.enq.ready
  

  fe_wb_fifo.reset := reset.asBool | io.flush
  fe_wb_fifo.io.enq.valid := is_empty & io.lsu_iss_exe.bits.fun.is_fence & io.lsu_iss_exe.valid
  fe_wb_fifo.io.enq.bits.rd0 := io.lsu_iss_exe.bits.param.rd0
  fe_wb_fifo.io.enq.bits.res := 0.U(64.W)
  if(hasVector) {fe_wb_fifo.io.enq.bits.vAttach.get := io.lsu_iss_exe.bits.vAttach.get}

  fe_wb_fifo.reset := reset.asBool | io.flush



  irtn_arb.io.in(0) <> su_wb_fifo.io.deq
  irtn_arb.io.in(1) <> lu_wb_fifo.io.deq
  irtn_arb.io.in(2) <> fe_wb_fifo.io.deq
  irtn_arb.io.out <> io.lsu_exe_iwb

  frtn_arb.io.out <> io.lsu_exe_fwb
}



trait LSU_Fault { this: LsuBase =>

  irtn_arb.io.in(3).valid := 
    io.mmu_lsu.valid &
      (io.mmu_lsu.bits.is_fault | io.lsu_iss_exe.bits.is_misAlign) & ~trans_kill &
      io.lsu_iss_exe.bits.fun.is_iwb

  irtn_arb.io.in(3).bits.rd0 := io.lsu_iss_exe.bits.param.rd0
  irtn_arb.io.in(3).bits.res := 0.U


  frtn_arb.io.in(1).valid := 
    io.mmu_lsu.valid &
      (io.mmu_lsu.bits.is_fault | io.lsu_iss_exe.bits.is_misAlign) & ~trans_kill &
      io.lsu_iss_exe.bits.fun.is_fwb

  frtn_arb.io.in(1).bits.rd0 := io.lsu_iss_exe.bits.param.rd0
  frtn_arb.io.in(1).bits.res := 0.U


  io.lsu_cWriteBack.valid := io.lsu_iss_exe.fire
    // (io.mmu_lsu.fire     &  (io.mmu_lsu.bits.is_access_fault | io.mmu_lsu.bits.is_paging_fault | io.lsu_iss_exe.bits.is_misAlign)) |
    // (io.mmu_lsu.fire     & ~(io.mmu_lsu.bits.is_access_fault | io.mmu_lsu.bits.is_paging_fault | io.lsu_iss_exe.bits.is_misAlign)) |
    // (io.lsu_iss_exe.fire & io.lsu_iss_exe.bits.fun.is_fence )
  
  io.lsu_cWriteBack.bits.addr := "hfff".U
  io.lsu_cWriteBack.bits.dati := 
    Cat(
      io.mmu_lsu.bits.is_access_fault,
      io.mmu_lsu.bits.is_paging_fault, 
      io.lsu_iss_exe.bits.is_misAlign,
      (io.lsu_iss_exe.bits.param.dat.op1(vlen-1, 0) | 0.U(61.W))
    ); require(vlen <= 61)
  io.lsu_cWriteBack.bits.op_rw := io.mmu_lsu.bits.is_access_fault | io.mmu_lsu.bits.is_paging_fault | io.lsu_iss_exe.bits.is_misAlign
  io.lsu_cWriteBack.bits.op_rs := false.B
  io.lsu_cWriteBack.bits.op_rc := false.B
  io.lsu_cWriteBack.bits.idx   := io.lsu_iss_exe.bits.param.csrw(log2Ceil(cRegNum)-1, 0 )
















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
  
