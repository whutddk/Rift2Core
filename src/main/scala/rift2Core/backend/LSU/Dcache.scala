

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

import rift._
import base._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._





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

    val missUnit_dcache_acquire = new DecoupledIO(new TLBundleA(dEdge.bundle))
    val missUnit_dcache_grant   = Flipped(new DecoupledIO(new TLBundleD(dEdge.bundle)))
    val missUnit_dcache_grantAck  = DecoupledIO(new TLBundleE(dEdge.bundle))

    val probeUnit_dcache_probe = Flipped(new DecoupledIO(new TLBundleB(dEdge.bundle)))

    val writeBackUnit_dcache_release = new DecoupledIO(new TLBundleC(dEdge.bundle))
    val writeBackUnit_dcache_grant   = Flipped(new DecoupledIO(new TLBundleD(dEdge.bundle)))
  })

  val missUnit = Module(new MissUnit(edge = edge, setting = 2, id = id))
  val probeUnit = Module(new ProbeUnit(edge = edge, id = id))
  val writeBackUnit = Module(new WriteBackUnit(edge = edge, setting = 2, id = id))

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







  io.missUnit_dcache_acquire  <> missUnit.io.cache_acquire
  missUnit.io.cache_grant <> io.missUnit_dcache_grant
  io.missUnit_dcache_grantAck <> missUnit.io.cache_grantAck
  probeUnit.io.cache_probe <> io.probeUnit_dcache_probe
  io.writeBackUnit_dcache_release <> writeBackUnit.io.cache_release
  writeBackUnit.io.cache_grant <> io.writeBackUnit_dcache_grant






  stage.io.missUnit_req      <> missUnit.io.req
  stage.io.wb_req <> writeBackUnit.io.wb_req
  stage.io.pb_req <> writeBackUnit.io.pb_req
  stage.io.flush := io.flush

  missUnit.io.miss_ban := writeBackUnit.io.miss_ban
  writeBackUnit.io.release_ban := missUnit.io.release_ban




  reload_fifo.io.enq.valid := stage.io.reload.valid
  reload_fifo.io.enq.bits  := stage.io.reload.bits
  

  reload_fifo.io.deq <> op_arb.io.in(0)

  io.enq.ready := op_arb.io.in(1).ready & sbEnqReady
  op_arb.io.in(1).valid := io.enq.fire
  op_arb.io.in(1).bits  := io.enq.bits
  op_arb.io.in(1).bits.chkIdx := sbIdx //override

  op_arb.io.out <> lsEntry.io.enq 

  rd_arb.io.in(0).bits := pkg_Dcache_Enq_Bundle( missUnit.io.rsp.bits )
  rd_arb.io.in(0).valid := missUnit.io.rsp.valid
  missUnit.io.rsp.ready := rd_arb.io.in(0).ready

  rd_arb.io.in(1).bits := pkg_Dcache_Enq_Bundle(probeUnit.io.req.bits) 
  rd_arb.io.in(1).valid := probeUnit.io.req.valid
  probeUnit.io.req.ready := rd_arb.io.in(1).ready

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


