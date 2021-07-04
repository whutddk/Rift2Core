package rift2Core.L1Cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import rift2Core.define._


import base._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import sifive.blocks.inclusivecache._


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
    val lsu_iss_exe = Flipped(new DecoupledIO(new Info_cache_s0s1))
    val lu_exe_iwb = new DecoupledIO(new Exe_iwb_info)
    val flush = Input(Bool())
  })

  val cache_dat = new Cache_dat( dw, aw, bk, cb, cl )
  val cache_tag = new Cache_tag( dw, aw, bk, cb, cl ) 
  val missUnit = Module(new MissUnit(edge = edge, entry = 8))
  val probeUnit = Module(new ProbeUnit(edge = edge))
  val writeBackUnit = Module(new WriteBackUnit(edge = edge))

  val lsEntry = Module(new LS_entry())
  val rd_stage = Module(new L1_rd_stage())
  val wr_stage = Module(new L1_wr_stage())



  rd_stage.io.dat_addr_r <> cache_dat.dat_addr_r
  rd_stage.io.dat_en_r   <> cache_dat.dat_en_r
  cache_dat.dat_info_r   <> rd_stage.io.dat_info_r
  rd_stage.io.tag_addr_r <> cache_tag.tag_addr_r
  rd_stage.io.tag_en_r   <> cache_tag.tag_en_r
  cache_tag.tag_info_r   <> rd_stage.io.tag_info_r

  wr_stage.io.tag_addr_w        <> cache_tag.tag_addr_w
  wr_stage.io.tag_en_w          <> cache_tag.tag_en_w
  wr_stage.io.dat_addr_w        <> cache_dat.dat_addr_w
  wr_stage.io.dat_en_w          <> cache_dat.dat_en_w
  wr_stage.io.dat_info_wstrb    <> cache_dat.dat_info_wstrb
  wr_stage.io.dat_info_w        <> cache_dat.dat_info_w
  wr_stage.io.missUnit_req      <> missUnit.io.req
  wr_stage.io.writeBackUnit_req <> writeBackUnit.io.req




  // val lsu_exe_iwb_fifo = Module( new Queue( new Exe_iwb_info, 1, true, false ) )



  missUnit.io.dcache_grant.bits := bus.d.bits
  missUnit.io.dcache_grant.valid := bus.d.valid & ( bus.d.bits.opcode === TLMessages.Grant | bus.d.bits.opcode === TLMessages.GrantData )

  writeBackUnit.io.dcache_grant.bits := bus.d.bits
  writeBackUnit.io.dcache_grant.valid := bus.d.valid & ( bus.d.bits.opcode === TLMessages.ReleaseAck )

  bus.d.ready := 
    Mux1H(Seq(
      ( bus.d.bits.opcode === TLMessages.Grant || bus.d.bits.opcode === TLMessages.GrantData ) -> missUnit.io.dcache_grant.ready,
      ( bus.d.bits.opcode === TLMessages.ReleaseAck ) -> writeBackUnit.io.dcache_grant.ready
    ))

  // when( bus.d.bits.opcode === TLMessages.Grant || bus.d.bits.opcode === TLMessages.GrantData ) {
  //   bus.d.ready <> missUnit.io.dcache_grant.ready
  // } .elsewhen ( bus.d.bits.opcode === TLMessages.ReleaseAck ) {
  //   bus.d <> writeBackUnit.io.dcache_grant
  // } .otherwise {
  //   assert(~bus.d.fire)
  // }

  bus.a <> missUnit.io.dcache_acquire

  probeUnit.io.dcache_probe <> bus.b

  //  probeUnit.io.dcache_probe.valid := bus.b.valid
  //  probeUnit.io.dcache_probe.bits := bus.b.bits
  //  bus.b.ready := probeUnit.io.dcache_probe.ready



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



class wrapper_lsu(implicit p: Parameters) extends LazyModule {
  
  val mdl = LazyModule(new Lsu()) 

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle{
      val lsu_iss_exe = Flipped(new DecoupledIO(new Info_cache_s0s1))
      val lu_exe_iwb = new DecoupledIO(new Exe_iwb_info)
      val flush = Input(Bool())
    })   
    io <> mdl.module.io   
  } 

  val l2cache = LazyModule(new InclusiveCache(
      cache = CacheParameters( level = 2, ways = 4, sets = 64, blockBytes = 256*4/8, beatBytes = 128/8 ),
      micro = InclusiveCacheMicroParameters( writeBytes = 128/8, memCycles = 40, portFactor = 4),
      control = None
    ))

  val managerParameters = TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address = Seq(AddressSet(0x1000, 0xfff)),
        regionType = RegionType.CACHED,
        supportsAcquireT = TransferSizes(32),
        supportsAcquireB = TransferSizes(32),
        alwaysGrantsT = true
      )),
      beatBytes = 256/8,
      endSinkId = 1
  )

  val managerNode = TLManagerNode(portParams = Seq(managerParameters))
  val l2xbar = TLXbar()
  
  val memory1 = InModuleBody {
    managerNode.makeIOs()
  }
      



  managerNode := l2xbar := TLBuffer() := mdl.clientNode
  


  // val tlram = LazyModule(new TLRAM(
  //   address = AddressSet(0x1000, 0xfff)))



  // val memory1 = InModuleBody {
  //   axiram.node.makeIOs()
  // }

}




