

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
import chisel3.util.random._

import rift2Core.define._

import rift2Chip._
import base._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._




/** the operation that dcache support
  * @note Lsu isa
  * @note probe
  * @note grant
  * @note preft
  */
class Cache_op extends Lsu_isa {

  val probe = Bool()
  val grant = Bool()
  val preft = Bool()

  def is_atom = is_amo
  def is_access = is_atom | is_lu | is_su | is_lr | is_sc
  def is_tag_r = is_atom | is_lu | is_su | is_lr | is_sc | grant | probe | preft
  def is_dat_r = is_atom | is_lu | is_lr | grant | probe
  def is_tag_w = grant
  def is_dat_w = is_atom | is_su | is_sc | grant
  def isDirtyOp = is_atom | is_su | is_sc
  def is_wb = is_atom | is_lu | is_lr

}


abstract class Dcache_ScoreBoard_Bundle(implicit p: Parameters) extends DcacheBundle {
  val chkIdx = UInt((log2Ceil(sbEntry)).W)
}

class Dcache_Enq_Bundle(implicit p: Parameters) extends Dcache_ScoreBoard_Bundle {
  val paddr = UInt(plen.W)
  val wdata  = UInt(dw.W)
  val wstrb  = UInt((dw/8).W)

  val fun    = new Cache_op
  val rd = new RD_PHY

  def tagSel = paddr(plen-1,plen-tag_w)
  def clSel  = paddr(addr_lsb+bk_w + line_w-1, addr_lsb+bk_w)

  def wmask = Strb2Mask(wstrb)

}

class Dcache_Deq_Bundle(implicit p: Parameters) extends Dcache_ScoreBoard_Bundle {
  val wb = new WriteBack_info(dw=64)
  val is_load_amo = Bool()

  val is_flw = Bool()
  val is_fld = Bool()

  def is_iwb = ~is_fwb
  def is_fwb = is_flw | is_fld

}


abstract class DcacheBase(edge: TLEdgeOut, id: Int)(implicit p: Parameters) extends DcacheModule {
  val dEdge = edge

  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Dcache_Enq_Bundle))
    val deq = new DecoupledIO(new Dcache_Deq_Bundle)
    val is_empty = Output(Bool())

    val flush = Input(Bool())

    // val dm        = if (hasDebugger) {Some(Flipped(new Info_DM_cmm))} else {None}

    val missUnit_dcache_acquire      = if( hasL2 ) Some(new DecoupledIO(new TLBundleA(dEdge.bundle)))          else {None}
    val missUnit_dcache_grant        = if( hasL2 ) Some(Flipped(new DecoupledIO(new TLBundleD(dEdge.bundle)))) else {None}
    val missUnit_dcache_grantAck     = if( hasL2 ) Some(DecoupledIO(new TLBundleE(dEdge.bundle)))              else {None}
    val probeUnit_dcache_probe       = if( hasL2 ) Some(Flipped(new DecoupledIO(new TLBundleB(dEdge.bundle)))) else {None}
    val writeBackUnit_dcache_release = if( hasL2 ) Some(new DecoupledIO(new TLBundleC(dEdge.bundle))         ) else {None}
    val writeBackUnit_dcache_grant   = if( hasL2 ) Some(Flipped(new DecoupledIO(new TLBundleD(dEdge.bundle)))) else {None}

    val dcache_getPut = if ( hasL2 ) { None } else { Some(        new DecoupledIO(new TLBundleA(edge.bundle)) ) }
    val dcache_access = if ( hasL2 ) { None } else { Some(Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))) }
  })

  val missUnit      = if( hasL2 ) Some(Module(new MissUnit(edge = edge, setting = 2, id = id)))      else {None}
  val probeUnit     = if( hasL2 ) Some(Module(new ProbeUnit(edge = edge, id = id)))                  else {None}
  val writeBackUnit = if( hasL2 ) Some(Module(new WriteBackUnit(edge = edge, setting = 2, id = id))) else {None}

  val getUnit = if( hasL2 ) {None} else {Some(Module(new GetUnit(edge = edge, id = id)))}
  val putUnit = if( hasL2 ) {None} else {Some(Module(new PutUnit(edge = edge, id = id)))}

  val lsEntry = Module(new Queue(new Dcache_Enq_Bundle, sbEntry, pipe = false, flow = true))


  val rtn_fifo = Module(new Queue(new Dcache_Deq_Bundle, sbEntry, false, true)) // bypass Mode
  val op_arb = Module(new Arbiter( new Dcache_Enq_Bundle, 2))
  val rd_arb = Module(new Arbiter( new Dcache_Enq_Bundle, 3))

  val reload_fifo = Module( new Queue( new Dcache_Enq_Bundle, 1, true, false) )

  val stage = Module(new DcacheStage(id))
}

trait DcacheScoreBoard { this: DcacheBase =>
  // val cache_buffer = Module(new Cache_buffer)

  val sbBuff = RegInit(VecInit(Seq.fill(sbEntry)(0.U.asTypeOf(new Dcache_Enq_Bundle))))
  val sbValid = RegInit(VecInit(Seq.fill(sbEntry)(false.B)))


  val isHazard = VecInit(sbBuff.map( x => (x.paddr(plen-1,3) === io.enq.bits.paddr(plen-1,3)) )).reduce(_|_)
  val sbIdx = sbValid.indexWhere( (x:Bool) => (x === false.B) )


  val sbEnqReady =
    sbValid.exists( (x:Bool) => (x === false.B) ) &
    ~isHazard

  when( io.enq.fire ) {
    sbBuff(sbIdx)  := io.enq.bits
    sbValid(sbIdx) := true.B
  }

  when( io.deq.fire ) {
    sbBuff (io.deq.bits.chkIdx) := 0.U.asTypeOf( new Dcache_Enq_Bundle )
    sbValid(io.deq.bits.chkIdx) := false.B
  }

  val isSBEmpty = sbValid.forall((x:Bool) => (x === false.B))
  when( io.deq.fire ) { assert(~isSBEmpty) }
  // io.buf_deq.ready := ~isSBEmpty



  for ( i <- 0 until sbEntry ) yield {
    when( sbValid(i) === true.B ) {
      assert( sbBuff.count( (x: Dcache_Enq_Bundle) => (x.paddr === sbBuff(i).paddr) ) === 1.U )
    }
  }

}



class Dcache(edge: TLEdgeOut, id: Int)(implicit p: Parameters) extends DcacheBase(edge, id) with DcacheScoreBoard{






  if ( hasL2 ) {
    io.missUnit_dcache_acquire.get  <> missUnit.get.io.cache_acquire
    missUnit.get.io.cache_grant <> io.missUnit_dcache_grant.get
    io.missUnit_dcache_grantAck.get <> missUnit.get.io.cache_grantAck
    probeUnit.get.io.cache_probe <> io.probeUnit_dcache_probe.get
    io.writeBackUnit_dcache_release.get <> writeBackUnit.get.io.cache_release
    writeBackUnit.get.io.cache_grant <> io.writeBackUnit_dcache_grant.get  







  } else {
    val getPutArb = Module(new Arbiter(new TLBundleA(edge.bundle), 2) )
    io.dcache_getPut.get <> getPutArb.io.out
    getPutArb.io.in(0) <> getUnit.get.io.getPut
    getPutArb.io.in(1) <> putUnit.get.io.getPut

    getUnit.get.io.access.valid := io.dcache_access.get.valid & io.dcache_access.get.bits.opcode === TLMessages.AccessAckData
    getUnit.get.io.access.bits  := io.dcache_access.get.bits
    putUnit.get.io.access.valid := io.dcache_access.get.valid & io.dcache_access.get.bits.opcode === TLMessages.AccessAck
    putUnit.get.io.access.bits  := io.dcache_access.get.bits

    io.dcache_access.get.ready := 
      Mux1H(Seq(
        ( io.dcache_access.get.bits.opcode === TLMessages.AccessAckData ) -> getUnit.get.io.access.ready,
        ( io.dcache_access.get.bits.opcode === TLMessages.AccessAck     ) -> putUnit.get.io.access.ready,
      ))
  }

  if ( hasL2 ) {
    stage.io.missUnit_req      <> missUnit.get.io.req
    stage.io.wb_req <> writeBackUnit.get.io.wb_req
  
    missUnit.get.io.miss_ban := writeBackUnit.get.io.miss_ban
    writeBackUnit.get.io.release_ban := missUnit.get.io.release_ban

    rd_arb.io.in(0).bits := pkg_Dcache_Enq_Bundle( missUnit.get.io.rsp.bits )
    rd_arb.io.in(0).valid := missUnit.get.io.rsp.valid
    missUnit.get.io.rsp.ready := rd_arb.io.in(0).ready

    rd_arb.io.in(1).bits := pkg_Dcache_Enq_Bundle(probeUnit.get.io.req.bits) 
    rd_arb.io.in(1).valid := probeUnit.get.io.req.valid
    probeUnit.get.io.req.ready := rd_arb.io.in(1).ready
  } else {
    stage.io.missUnit_req      <> getUnit.get.io.req
    stage.io.wb_req <> putUnit.get.io.wb_req
  
    getUnit.get.io.miss_ban := putUnit.get.io.miss_ban
    putUnit.get.io.release_ban := getUnit.get.io.release_ban

    rd_arb.io.in(0).bits := pkg_Dcache_Enq_Bundle( getUnit.get.io.rsp.bits )
    rd_arb.io.in(0).valid := getUnit.get.io.rsp.valid
    getUnit.get.io.rsp.ready := rd_arb.io.in(0).ready

    rd_arb.io.in(1).bits := DontCare
    rd_arb.io.in(1).valid := false.B
  }



  reload_fifo.io.enq.valid := stage.io.reload.valid
  reload_fifo.io.enq.bits  := stage.io.reload.bits
  

  reload_fifo.io.deq <> op_arb.io.in(0)
  io.enq.ready := op_arb.io.in(1).ready & sbEnqReady
  op_arb.io.in(1).valid := io.enq.fire
  op_arb.io.in(1).bits  := io.enq.bits
  op_arb.io.in(1).bits.chkIdx := sbIdx //override
  op_arb.io.out <> lsEntry.io.enq 

  stage.io.flush := io.flush




  lsEntry.io.deq <> rd_arb.io.in(2)
  stage.io.enq <> rd_arb.io.out




  rtn_fifo.io.enq.valid := stage.io.deq.valid
  rtn_fifo.io.enq.bits  := stage.io.deq.bits
 
  rtn_fifo.io.deq <> io.deq
  // rtn_fifo.io.deq <> Decoupled1toN( VecInit( io.deq, cache_buffer.io.buf_deq ) )



  io.is_empty := isSBEmpty
  stage.io.isCacheEmpty := isSBEmpty

  when(reload_fifo.io.enq.valid) {assert(reload_fifo.io.enq.ready, "Assert Failed at Dcache, Pipeline stuck!")}
  when(rtn_fifo.io.enq.valid) {assert(rtn_fifo.io.enq.ready, "Assert Failed at Dcache, Pipeline stuck!")}

}


