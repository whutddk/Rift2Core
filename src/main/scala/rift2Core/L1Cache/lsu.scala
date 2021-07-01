package rift2Core.L1Cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import rift2Core.define._


import base._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._
// import freechips.rocketchip.diplomaticobjectmodel.model.M


// case class DcacheParameters(
//   dw: Int,
//   bk: Int,
//   cb: Int,
//   cl: Int,
//   aw: Int = 32
// ) extends L1CacheParameters

// trait HasDcacheParameters extends HasL1CacheParameters {
//   val cacheParams = dcacheParameters
// }

// abstract class DcacheModule(implicit p: Parameters) extends L1CacheModule
//   with HasDcacheParameters

// abstract class DcacheBundle(implicit p: Parameters) extends L1CacheBundle
//   with HasDcacheParameters


class Lsu()(implicit p: Parameters) extends LazyModule with HasL1CacheParameters{
  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "dcache",
      sourceId = IdRange(0, 1),
      supportsProbe = TransferSizes(32)
    ))
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new LsuImp(this)
}

class LsuImp(outer: Lsu) extends LazyModuleImp(outer)  with HasL1CacheParameters {

  val ( bus, edge ) = outer.clientNode.out.head

  val io = IO(new Bundle{
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val lu_exe_iwb = new DecoupledIO(new Exe_iwb_info)



    // val cmm_lsu = Input(new Info_cmm_lsu)
    // val lsu_cmm = Output( new Info_lsu_cmm )

    // val il1_fence_req = Output(Bool())


    val flush = Input(Bool())
  })

  // io.lsu_exe_iwb <> lsu_exe_iwb_fifo.io.deq
  // lsu_exe_iwb_fifo.reset := reset.asBool | io.flush


  val cache_dat = new Cache_dat( dw, aw, bk, cb, cl )
  val cache_tag = new Cache_tag( dw, aw, bk, cb, cl ) 
  val missUnit = Module(new MissUnit(edge = edge, entry = 8))
  val probeUnit = Module(new ProbeUnit(edge = edge))
  val writeBackUnit = Module(new WriteBackUnit(edge = edge))

  val lsEntry = new LS_entry()
  val rd_stage = Module(new L1_rd_stage( cache_dat, cache_tag ))
  val wr_stage = Module(new L1_wr_stage( cache_dat, cache_tag, missUnit, writeBackUnit ))

  val lsu_exe_iwb_fifo = Module( new Queue( new Exe_iwb_info, 1, true, false ) )

  when( bus.d.bits.opcode === TLMessages.Grant || bus.d.bits.opcode === TLMessages.GrantData ) {
    bus.d <> missUnit.io.dcache_grant
  } .elsewhen ( bus.d.bits.opcode === TLMessages.ReleaseAck ) {
    bus.d <> writeBackUnit.io.dcache_grant
  } .otherwise {
    assert(~bus.d.fire)
  }

  bus.a <> missUnit.io.dcache_acquire
  bus.b <> probeUnit.io.dcache_probe 
  bus.c <> writeBackUnit.io.dcache_release
  bus.e <> missUnit.io.dcache_grantAck

  
  missUnit.io.miss_ban := writeBackUnit.io.miss_ban
  writeBackUnit.io.release_ban := missUnit.io.release_ban


  val ls_arb = Module(new Arbiter( new Info_cache_s0s1, 2))
  val rd_arb = Module(new Arbiter( new Info_cache_s0s1, 2))
  val wr_arb = Module(new Arbiter( new Info_cache_s1s2, 2))

  wr_stage.io.wr_lsReload <> ls_arb.io.in(0)
  io.lsu_iss_exe <> ls_arb.io.in(1)
  ls_arb.io.out <> lsEntry.io.in 

  probeUnit.io.req <> rd_arb.io.in(0)
  lsEntry.io.out <> rd_arb.io.in(1)
  rd_arb.io.out <> rd_stage.io.rd_in

  missUnit.io.rsp <> wr_arb.io.in(0)
  rd_stage.io.rd_out <> wr_arb.io.in(1)
  wr_arb.io.out <> wr_stage.io.wr_in



  wr_stage.io.lu_exe_iwb <> io.lu_exe_iwb

}


// class wrapper_lsu(implicit p: Parameters) extends LazyModule with HasDcacheParameters{
//   // implicit val p: Parameters

  
//   val mdl = LazyModule(new Lsu()) 

//   lazy val module = new LazyModuleImp(this) {
//     val io = IO(new Bundle{
//       val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
//       val lu_exe_iwb = new DecoupledIO(new Exe_iwb_info)
//       val flush = Input(Bool())
//     })   

//     io <> mdl.module.io
//   } 




// }




