/*
* @Author: Ruige Lee
* @Date:   2021-03-29 14:37:18
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-08 16:22:42
*/

/*
  Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

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

package rift2Core.backend


import chisel3._
import chisel3.util._
import chisel3.util.random._
import rift2Core.define._
import rift2Core.cache._
import tilelink._
import axi._
import chisel3.experimental.ChiselEnum
import rift2Core.L1Cache._

import chisel3.experimental._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._

class LsuPendingIO[T <: Data](private val gen: T, val entries: Int) extends Bundle {
  val enq = Flipped(EnqIO(gen))
  /** I/O to dequeue data (client is consumer and Queue object is producer), is [[Chisel.DecoupledIO]]*/
  val deq = Flipped(DeqIO(gen))
  /** The current amount of data in the queue */
  val count = Output(UInt(log2Ceil(entries + 1).W))

  val cmm = Flipped(EnqIO(Bool()))
  val flush = Input(Bool())
}

class lsu_pending_fifo[T <: Data](val gen: T, val entries: Int)(implicit compileOptions: chisel3.CompileOptions) extends Module{

require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")
  val genType = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }

  val io = IO(new LsuPendingIO(genType, entries))

  val ram = Mem(entries, genType)
  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val maybe_full = RegInit(false.B)

  val ptr_match = enq_ptr.value === deq_ptr.value
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val do_enq = WireDefault(io.enq.fire())
  val do_deq = WireDefault(io.deq.fire())

  when (do_enq) {
    ram(enq_ptr.value) := io.enq.bits
    enq_ptr.inc()
  }
  when (do_deq) {
    deq_ptr.inc()
  }
  when (do_enq =/= do_deq) {
    maybe_full := do_enq
  }

  io.deq.valid := !empty
  io.enq.ready := !full
  io.deq.bits := ram(deq_ptr.value)



  val ptr_diff = enq_ptr.value - deq_ptr.value
  if (isPow2(entries)) {
    io.count := Mux(maybe_full && ptr_match, entries.U, 0.U) | ptr_diff
  } else {
    io.count := Mux(ptr_match,
                    Mux(maybe_full,
                      entries.asUInt, 0.U),
                    Mux(deq_ptr.value > enq_ptr.value,
                      entries.asUInt + ptr_diff, ptr_diff))
  }




  val cmm_ptr = Counter(entries)

  when( io.cmm.fire ) { cmm_ptr.value := cmm_ptr.value + Mux(io.cmm.bits, 2.U, 1.U) }

  val cmm_maybe_full = RegInit(false.B)
  val do_cmm         = WireDefault( io.cmm.fire )
  when( do_cmm =/= do_deq ) { cmm_maybe_full := do_cmm }

  val cmm_empty = cmm_ptr.value === deq_ptr.value & ~cmm_maybe_full
  val cmm_full  = cmm_ptr.value === deq_ptr.value &  cmm_maybe_full

  io.cmm.ready := true.B
  io.deq.valid := !empty & !cmm_empty

  when( io.flush ) {
    enq_ptr.value := cmm_ptr.value
    assert (!do_enq )
  }

  assert( ~(cmm_full & do_cmm) )
}


class lsu_scoreBoard(implicit p: Parameters) extends DcacheModule {

  val io = IO(new Bundle{
    val lsu_push = Flipped(new DecoupledIO(new Info_cache_sb))
    val lsu_pop = new DecoupledIO(new Exe_iwb_info)

    val dcache_push = new DecoupledIO(new Info_cache_s0s1)
    val dcache_pop = Flipped(new DecoupledIO(new Info_cache_retn))

    // val periph_push = new DecoupledIO(new Info_cache_s0s1)
    // val periph_pop = Flipped(new DecoupledIO(new Info_cache_retn))

    val empty = Output(Bool())
  })



  val paddr = RegInit(VecInit(Seq.fill(16)(0.U(64.W)) ))
  val rd0 = RegInit(VecInit(Seq.fill(16)(0.U(6.W)) ))
  val valid = RegInit(VecInit(Seq.fill(16)(false.B)))

  val empty_idx = valid.indexWhere((x:Bool) => (x === false.B))

  val full = valid.forall((x:Bool) => (x === true.B))
  io.empty := valid.forall((x:Bool) => (x === false.B))
  val hazard = valid.zip(paddr).map{ case(a,b) => (a === true.B) & (b === io.lsu_push.bits.param.op1) }.reduce(_|_)  


  when( io.lsu_push.fire ) {
    valid(empty_idx)  := true.B
    paddr(empty_idx)  := io.lsu_push.bits.param.op1
    rd0(empty_idx)    := io.lsu_push.bits.param.rd0_phy
  }
  when( io.dcache_pop.fire ) {
    valid(io.dcache_pop.bits.chk_idx) := false.B  
  }




  io.dcache_push.bits.paddr   := io.lsu_push.bits.param.op1
  io.dcache_push.bits.wmask   := {
    val paddr = io.lsu_push.bits.param.op1
    val op = io.lsu_push.bits.fun
    val lsu_wstrb_align = (Mux1H( Seq(
          op.is_byte -> "b00000001".U,
          op.is_half -> "b00000011".U,
          op.is_word -> "b00001111".U,
          op.is_dubl -> "b11111111".U
          )) << paddr(2,0))
    lsu_wstrb_align
  }
  for ( j <- 0 until bk ) yield {
    io.dcache_push.bits.wdata(j) := {
      val res = Wire(UInt(64.W))
      val paddr = io.lsu_push.bits.param.op1
      val shift = Wire(UInt(6.W))
      shift := Cat( paddr(2,0), 0.U(3.W) )

      res := io.lsu_push.bits.param.op2 << shift
      res
    }
  }

  io.dcache_push.bits.op.fun <> io.lsu_push.bits.fun
  io.dcache_push.bits.op.grant := false.B
  io.dcache_push.bits.op.probe := false.B
  io.dcache_push.bits.chk_idx := empty_idx


  io.lsu_pop.bits.rd0_phy := rd0(io.dcache_pop.bits.chk_idx)
  io.lsu_pop.bits.res := io.dcache_pop.bits.res


  io.lsu_pop.valid := io.dcache_pop.fire & io.dcache_pop.bits.is_load_amo
  io.dcache_push.valid := io.lsu_push.fire

  io.lsu_push.ready := ~full & io.dcache_push.ready & ~hazard
  io.dcache_pop.ready := io.lsu_pop.ready

}

class Lsu(tlc_edge: TLEdgeOut)(implicit p: Parameters) extends DcacheModule{

  val io = IO( new Bundle{
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val lsu_exe_iwb = new DecoupledIO(new Exe_iwb_info)

    val cmm_lsu = Input(new Info_cmm_lsu)
    val lsu_cmm = Output( new Info_lsu_cmm )

    val icache_fence_req = Output(Bool())
    // val dcache_fence_req = Output(Bool())


    val missUnit_dcache_acquire = Decoupled(new TLBundleA(tlc_edge.bundle))
    val missUnit_dcache_grant   = Flipped(DecoupledIO(new TLBundleD(tlc_edge.bundle)))
    val missUnit_dcache_grantAck  = Decoupled(new TLBundleE(tlc_edge.bundle))
    val probeUnit_dcache_probe = Flipped(DecoupledIO(new TLBundleB(tlc_edge.bundle)))
    val writeBackUnit_dcache_release = DecoupledIO(new TLBundleC(tlc_edge.bundle))
    val writeBackUnit_dcache_grant   = Flipped(DecoupledIO(new TLBundleD(tlc_edge.bundle)))




    val flush = Input(Bool())
  })

  val pending_fifo = Module(new lsu_pending_fifo( new Info_cache_sb, 16))
  val scoreBoard_arb = Module(new Arbiter(new Info_cache_sb, 2 ))
  val lsu_scoreBoard = Module(new lsu_scoreBoard)
  val dcache = Module(new Dcache(tlc_edge))
  val su_exe_iwb_fifo = Module( new Queue( new Exe_iwb_info, 1, true, false ) )
  val lu_exe_iwb_fifo = Module( new Queue( new Exe_iwb_info, 1, true, false ) )


  val trans_kill = RegInit(false.B)
  val is_fence_i  = RegInit(false.B)
  val fence_op  = RegInit(false.B)

  val iwb_arb = Module(new Arbiter(new Exe_iwb_info, 2))
  iwb_arb.io.in(0) <> su_exe_iwb_fifo.io.deq
  iwb_arb.io.in(1) <> lu_exe_iwb_fifo.io.deq
  iwb_arb.io.out <> io.lsu_exe_iwb

  pending_fifo.io.flush := io.flush

  when( io.flush ) { trans_kill := true.B }
  .elsewhen( lsu_scoreBoard.io.empty ) { trans_kill := false.B }

  when( io.lsu_iss_exe.fire & io.lsu_iss_exe.bits.fun.is_fence ) {
    fence_op := true.B
    is_fence_i := io.lsu_iss_exe.bits.fun.fence_i
  }
  .elsewhen( lsu_scoreBoard.io.empty ) {
    fence_op := false.B
    is_fence_i := false.B
  }

  io.icache_fence_req := fence_op === true.B & lsu_scoreBoard.io.empty & is_fence_i


  su_exe_iwb_fifo.io.enq.valid := pending_fifo.io.enq.fire | (io.lsu_iss_exe.valid & io.lsu_iss_exe.bits.fun.is_fence)
  su_exe_iwb_fifo.io.enq.bits.rd0_phy := io.lsu_iss_exe.bits.param.rd0_phy
  su_exe_iwb_fifo.io.enq.bits.res     := 0.U



  pending_fifo.io.enq.valid := ~fence_op & io.lsu_iss_exe.valid & (io.lsu_iss_exe.bits.fun.is_su | io.lsu_iss_exe.bits.fun.is_amo )
  pending_fifo.io.enq.bits <> io.lsu_iss_exe.bits
  io.lsu_iss_exe.ready :=
    (pending_fifo.io.enq.fire & su_exe_iwb_fifo.io.enq.fire) |
    scoreBoard_arb.io.in(1).fire      



  pending_fifo.io.cmm.valid := io.cmm_lsu.is_store_commit | io.cmm_lsu.is_fence_commit
  pending_fifo.io.cmm.bits := io.cmm_lsu.is_store_commit & io.cmm_lsu.is_fence_commit

  pending_fifo.io.deq <> scoreBoard_arb.io.in(0)
  scoreBoard_arb.io.in(1).valid := io.lsu_iss_exe.valid & io.lsu_iss_exe.bits.fun.is_lu & ~trans_kill & ~fence_op
  scoreBoard_arb.io.in(1).bits := io.lsu_iss_exe.bits

  scoreBoard_arb.io.out <> lsu_scoreBoard.io.lsu_push

  //when kill trans the uncommited write back will be ignore, no atom will appear here when flush
  lu_exe_iwb_fifo.io.enq.valid := lsu_scoreBoard.io.lsu_pop.valid & ~trans_kill
  lu_exe_iwb_fifo.io.enq.bits := lsu_scoreBoard.io.lsu_pop.bits
  lsu_scoreBoard.io.lsu_pop.ready := lu_exe_iwb_fifo.io.enq.ready | trans_kill

  lsu_scoreBoard.io.dcache_push <> dcache.io.dcache_push
  lsu_scoreBoard.io.dcache_pop <> dcache.io.dcache_pop






  def is_accessFault = 
      (io.lsu_iss_exe.bits.fun.is_lu | io.lsu_iss_exe.bits.fun.is_su | io.lsu_iss_exe.bits.fun.is_amo) & 
      (io.lsu_iss_exe.bits.param.op1(63,32) =/= 0.U  ) 



  io.lsu_cmm.is_accessFault :=
    io.lsu_iss_exe.valid & is_accessFault
    

  io.lsu_cmm.is_misAlign :=
    io.lsu_iss_exe.valid & io.lsu_iss_exe.bits.is_misAlign

  io.lsu_cmm.trap_addr := io.lsu_iss_exe.bits.param.op1




//    SSSSSSSSSSSSSSS YYYYYYY       YYYYYYY   SSSSSSSSSSSSSSS 
//  SS:::::::::::::::SY:::::Y       Y:::::Y SS:::::::::::::::S
// S:::::SSSSSS::::::SY:::::Y       Y:::::YS:::::SSSSSS::::::S
// S:::::S     SSSSSSSY::::::Y     Y::::::YS:::::S     SSSSSSS
// S:::::S            YYY:::::Y   Y:::::YYYS:::::S            
// S:::::S               Y:::::Y Y:::::Y   S:::::S            
//  S::::SSSS             Y:::::Y:::::Y     S::::SSSS         
//   SS::::::SSSSS         Y:::::::::Y       SS::::::SSSSS    
//     SSS::::::::SS        Y:::::::Y          SSS::::::::SS  
//        SSSSSS::::S        Y:::::Y              SSSSSS::::S 
//             S:::::S       Y:::::Y                   S:::::S
//             S:::::S       Y:::::Y                   S:::::S
// SSSSSSS     S:::::S       Y:::::Y       SSSSSSS     S:::::S
// S::::::SSSSSS:::::S    YYYY:::::YYYY    S::::::SSSSSS:::::S
// S:::::::::::::::SS     Y:::::::::::Y    S:::::::::::::::SS 
//  SSSSSSSSSSSSSSS       YYYYYYYYYYYYY     SSSSSSSSSSSSSSS 
  



//mem






  // dcache.io.missUnit_dcache_grant.bits := tlc_bus.d.bits
  // dcache.io.missUnit_dcache_grant.valid := tlc_bus.d.valid & ( tlc_bus.d.bits.opcode === TLMessages.Grant | tlc_bus.d.bits.opcode === TLMessages.GrantData )

  // dcache.io.writeBackUnit_dcache_grant.bits := tlc_bus.d.bits
  // dcache.io.writeBackUnit_dcache_grant.valid := tlc_bus.d.valid & ( tlc_bus.d.bits.opcode === TLMessages.ReleaseAck )

  // tlc_bus.d.ready := 
  //   Mux1H(Seq(
  //     ( tlc_bus.d.bits.opcode === TLMessages.Grant || tlc_bus.d.bits.opcode === TLMessages.GrantData ) -> dcache.io.missUnit_dcache_grant.ready,
  //     ( tlc_bus.d.bits.opcode === TLMessages.ReleaseAck ) -> dcache.io.writeBackUnit_dcache_grant.ready
  //   ))

  // tlc_bus.a <> dcache.io.missUnit_dcache_acquire

  // dcache.io.probeUnit_dcache_probe.valid := tlc_bus.b.valid
  // dcache.io.probeUnit_dcache_probe.bits := tlc_bus.b.bits
  // tlc_bus.b.ready := dcache.io.probeUnit_dcache_probe.ready



  // tlc_bus.c <> dcache.io.writeBackUnit_dcache_release
  // tlc_bus.e <> dcache.io.missUnit_dcache_grantAck



  io.missUnit_dcache_acquire  <> dcache.io.missUnit_dcache_acquire
  dcache.io.missUnit_dcache_grant <> io.missUnit_dcache_grant
  io.missUnit_dcache_grantAck <> dcache.io.missUnit_dcache_grantAck
  dcache.io.probeUnit_dcache_probe <> io.probeUnit_dcache_probe
  io.writeBackUnit_dcache_release <> dcache.io.writeBackUnit_dcache_release
  dcache.io.writeBackUnit_dcache_grant <> io.writeBackUnit_dcache_grant


}