
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

    val sys_chn_ar = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    val sys_chn_r = Flipped( new DecoupledIO(new AXI_chn_r( 64, 1, 1)) )
    val sys_chn_aw = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    val sys_chn_w = new DecoupledIO(new AXI_chn_w( 64, 1 )) 
    val sys_chn_b = Flipped( new DecoupledIO(new AXI_chn_b( 1, 1 )))


    val flush = Input(Bool())
  })

  /** the pending fifo pend the store op and amo op, until the store instr is commited, the amo instr is waiting to commit */
  val pending_fifo = Module(new lsu_pending_fifo(16))

  /** the in(0) is the deq of pending fifo, the other one is load request */
  val scoreBoard_arb = Module(new Arbiter(new Info_cache_sb, 2 ))
  val lsu_scoreBoard = Module(new lsu_scoreBoard)
  val dcache = Module(new Dcache(tlc_edge))
  val periph = Module(new periph_mst()) 

  val su_exe_iwb_fifo = Module( new Queue( new Exe_iwb_info, 1, false, true ) )
  val lu_exe_iwb_fifo = Module( new Queue( new Exe_iwb_info, 1, false, true ) )

  su_exe_iwb_fifo.reset := reset.asBool | io.flush
  lu_exe_iwb_fifo.reset := reset.asBool | io.flush

  /** when a flush comes, flush all uncommit write & amo request in pending-fifo, and block all request from issue until scoreboard is empty */
  val trans_kill = RegInit(false.B)

  /** indicate if the fence is an icache fence */
  val is_fence_i  = RegInit(false.B)

  /** when a fence request comes, commit it and block all request from issue until scoreboard is empty */ 
  val fence_op  = RegInit(false.B)

  /** merge load write-back and store write-back port together */ 
  val iwb_arb = Module(new Arbiter(new Exe_iwb_info, 2))
  iwb_arb.io.in(0) <> su_exe_iwb_fifo.io.deq
  iwb_arb.io.in(1) <> lu_exe_iwb_fifo.io.deq
  iwb_arb.io.out <> io.lsu_exe_iwb

  pending_fifo.io.flush := io.flush

  when( io.flush ) { trans_kill := true.B }
  .elsewhen( lsu_scoreBoard.io.is_empty ) { trans_kill := false.B }

  when( io.lsu_iss_exe.fire & io.lsu_iss_exe.bits.fun.is_fence ) {
    fence_op := true.B
    is_fence_i := io.lsu_iss_exe.bits.fun.fence_i
  }
  .elsewhen( lsu_scoreBoard.io.is_empty ) {
    fence_op := false.B
    is_fence_i := false.B
  }

  io.icache_fence_req := fence_op === true.B & lsu_scoreBoard.io.is_empty & is_fence_i


  su_exe_iwb_fifo.io.enq.valid :=
    io.lsu_iss_exe.valid & ~trans_kill & ~fence_op & ~is_Fault & (
      (pending_fifo.io.enq.ready & io.lsu_iss_exe.bits.fun.is_su) | //when is store, the pending fifo should ready
      (io.lsu_iss_exe.bits.fun.is_fence)                            //when is fence, don't care about pending fifo 
    )


  su_exe_iwb_fifo.io.enq.bits.rd0_phy := io.lsu_iss_exe.bits.param.rd0_phy
  su_exe_iwb_fifo.io.enq.bits.res     := 0.U



  pending_fifo.io.enq.valid :=
    io.lsu_iss_exe.valid & ~trans_kill & ~fence_op & ~is_Fault & (
      (su_exe_iwb_fifo.io.enq.ready & io.lsu_iss_exe.bits.fun.is_su) | //when is store, store wb should ready
      (io.lsu_iss_exe.bits.fun.is_amo )                                //when is atom, don't care wb fifo
    )
    
  pending_fifo.io.enq.bits := io.lsu_iss_exe.bits

  io.lsu_iss_exe.ready :=
    ~trans_kill & ~fence_op & ~is_Fault & (
      ( io.lsu_iss_exe.bits.fun.is_su  & pending_fifo.io.enq.ready & su_exe_iwb_fifo.io.enq.ready) | //when store, both pending fifo and store wb should ready
      ( ~pending_fifo.io.is_hazard & io.lsu_iss_exe.bits.fun.is_lu  & scoreBoard_arb.io.in(1).ready) | //when load, scoreBoard should ready
      ( io.lsu_iss_exe.bits.fun.is_amo & pending_fifo.io.enq.ready) |                                //when amo, the pending fifo should ready
      ( io.lsu_iss_exe.bits.fun.is_fence & su_exe_iwb_fifo.io.enq.ready)                               //when fence, the store wb shoudl ready      
    )



  pending_fifo.io.cmm.valid := io.cmm_lsu.is_store_commit
  pending_fifo.io.cmm.bits := false.B

  pending_fifo.io.deq.ready := scoreBoard_arb.io.in(0).ready
  scoreBoard_arb.io.in(0).bits := pending_fifo.io.deq.bits
  scoreBoard_arb.io.in(0).valid := pending_fifo.io.deq.valid



  scoreBoard_arb.io.in(1).valid := io.lsu_iss_exe.valid & io.lsu_iss_exe.bits.fun.is_lu & ~trans_kill & ~fence_op & ~is_Fault & ~pending_fifo.io.is_hazard
  scoreBoard_arb.io.in(1).bits := io.lsu_iss_exe.bits

  scoreBoard_arb.io.out <> lsu_scoreBoard.io.lsu_push

  //when kill trans the uncommited write back will be ignore, no atom will appear here when flush
  lu_exe_iwb_fifo.io.enq.valid := lsu_scoreBoard.io.lsu_pop.valid & ~trans_kill
  lu_exe_iwb_fifo.io.enq.bits := lsu_scoreBoard.io.lsu_pop.bits
  lsu_scoreBoard.io.lsu_pop.ready := lu_exe_iwb_fifo.io.enq.ready | trans_kill

  lsu_scoreBoard.io.dcache_push <> dcache.io.dcache_push
  lsu_scoreBoard.io.dcache_pop <> dcache.io.dcache_pop

  lsu_scoreBoard.io.periph_push <> periph.io.periph_push
  lsu_scoreBoard.io.periph_pop <> periph.io.periph_pop


  def is_accessFault = 
      (io.lsu_iss_exe.bits.fun.is_lu | io.lsu_iss_exe.bits.fun.is_su | io.lsu_iss_exe.bits.fun.is_amo) & 
     ( (io.lsu_iss_exe.bits.param.op1(63,32) =/= 0.U ) | (io.lsu_iss_exe.bits.param.op1(31,30) =/= "b01".U & io.lsu_iss_exe.bits.param.op1(31) =/= 1.U))

  def is_Fault = is_accessFault | io.lsu_iss_exe.bits.is_misAlign



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

  periph.io.sys_chn_ar <> io.sys_chn_ar
  periph.io.sys_chn_r  <> io.sys_chn_r
  periph.io.sys_chn_aw <> io.sys_chn_aw
  periph.io.sys_chn_w  <> io.sys_chn_w
  periph.io.sys_chn_b  <> io.sys_chn_b


}