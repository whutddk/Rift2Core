
/*
  Copyright (c) 2020 - 2024 Wuhan University of Technology <295054118@whut.edu.cn>

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


package rift2Chip

import chisel3._
import rift2Core._

import debug._


import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import sifive.blocks.inclusivecache._


import constellation.noc._
import constellation.channel._
import constellation.protocol._
import constellation.router._
import constellation.topology._
import constellation.routing._
import scala.collection.immutable.ListMap








class Rift2NoC(isFlatten: Boolean = false)(implicit p: Parameters) extends LazyModule with HasRiftParameters{

  val i_rift2Core = LazyModule( new Rift2Core(isFlatten) )
  val i_debugger = if ( hasDebugger) {Some(LazyModule(new Debugger(nComponents = 1)))} else {None}
  val i_aclint = LazyModule( new AClint( nTiles = 1 ) )
  val nDevices = 31
  val i_plic = LazyModule( new Plic( nHarts = 1, nPriorities = 8, nDevices = nDevices ))
  val sifiveCache = LazyModule(new InclusiveCache(
      cache = CacheParameters( level = 2, ways = 2, sets = 8, blockBytes = l1DW/8, beatBytes = l1BeatBits/8, hintsSkipProbe = false ),
      micro = InclusiveCacheMicroParameters( writeBytes = memBeatBits/8, memCycles = 40, portFactor = 4),
      control = None
    ))




  val memRange = AddressSet(0x00000000L, 0xffffffffL).subtract(AddressSet(0x0L, 0x7fffffffL))
  val memAXI4SlaveNode = AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address = memRange,
          regionType = RegionType.UNCACHED,
          executable = true,
          supportsRead = TransferSizes(1, 256/8),
          supportsWrite = TransferSizes(1, 256/8)
        )
      ),
      beatBytes = 128 / 8
    )
  ))

  val sysRange = AddressSet(0x00000000L, 0x7fffffffL).subtract(AddressSet(0x00000000L, 0x1fffffffL))
  val sysAXI4SlaveNode = AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address = sysRange,
          regionType = RegionType.UNCACHED,
          executable = true,
          supportsRead = TransferSizes(1, 256/8),
          supportsWrite = TransferSizes(1, 256/8)
        )
      ),
      beatBytes = 64 / 8
    )
  ))



  val noc = LazyModule(new TLNoC(
    SimpleTLNoCParams(
      nodeMappings = DiplomaticNetworkNodeMapping(
        inNodeMapping  = ListMap( "icache[0]" -> 0, "dcache[0]" -> 1, "mmu[0]" -> 2, "prefetch[0]" -> 3, "system[0]"-> 4, "periph[0]"-> 4 ),
        outNodeMapping = ListMap( "soc[0]" -> 5, "soc[1]" -> 6, "dm[0]" -> 7, "i_aclint[0]" -> 8, "i_plic[0]" -> 8 ),
      ),
      nocParams = 
      
      
      NoCParams(
        // Physical specifications
        topology = BidirectionalTorus2D(3, 3),
        channelParamGen = (_, _) => UserChannelParams(
          virtualChannelParams = Seq.fill(16)(UserVirtualChannelParams(16)),
          crossingType = NoCrossing,
          srcSpeedup = 2,
          destSpeedup = 2
        ),
        // ingresses = Seq(
        //   UserIngressParams( destId = 0, payloadBits = 8 ),
        //   UserIngressParams( destId = 0, payloadBits = 8 ),
        //   UserIngressParams( destId = 0, payloadBits = 8 ),
        //   UserIngressParams( destId = 0, payloadBits = 8 ),
        //   UserIngressParams( destId = 0, payloadBits = 8 ),
        //   UserIngressParams( destId = 0, payloadBits = 8 ),
        //   UserIngressParams( destId = 0, payloadBits = 8 ),
        //   UserIngressParams( destId = 0, payloadBits = 8 ),
        //   UserIngressParams( destId = 0, payloadBits = 8 ),
        //   UserIngressParams( destId = 0, payloadBits = 8 ),
        //   UserIngressParams( destId = 0, payloadBits = 8 ),
        //   // UserIngressParams( destId = 4, payloadBits = 8 ),
        // ),
        // egresses = Seq(
        //   // UserEgressParams( srcId = 0, payloadBits = 8 ),
        //   UserEgressParams( srcId = 8, payloadBits = 8 ),
        //   UserEgressParams( srcId = 8, payloadBits = 8 ),
        //   UserEgressParams( srcId = 8, payloadBits = 8 ),
        //   UserEgressParams( srcId = 8, payloadBits = 8 ),
        //   UserEgressParams( srcId = 8, payloadBits = 8 ),
        //   UserEgressParams( srcId = 8, payloadBits = 8 ),
        //   UserEgressParams( srcId = 8, payloadBits = 8 ),
        //   UserEgressParams( srcId = 8, payloadBits = 8 ),
        //   UserEgressParams( srcId = 8, payloadBits = 8 ),
        // ),
        routerParams = (_) => UserRouterParams(// Payload width. Must match payload width on all channels attached to this routing node
          payloadBits = 128,
          combineSAST = false,// Combines SA and ST stages (removes pipeline register)
          combineRCVA = false,// Combines RC and VA stages (removes pipeline register)
          coupleSAVA = false,// Adds combinational path from SA to VA
          vcAllocator = (vP) => (p) => new RotatingSingleVCAllocator(vP)(p)
        ),
        // Flow specification
        // (blocker, blockee) => bool
        // If true, then blocker must be able to proceed when blockee is blocked
        vNetBlocking = (blocker, blockee) => blocker < blockee,
        // flows           = Seq.tabulate(9, 9) { (s, d) => FlowParams(s, d, 0) }.flatten,
        flows           = Seq(
          FlowParams(0, 5, 0),
          FlowParams(0, 6, 0),
          FlowParams(0, 7, 0),
          FlowParams(1, 5, 0),
          FlowParams(2, 5, 0),
          FlowParams(3, 5, 0),
          FlowParams(4, 6, 0),
          FlowParams(4, 7, 0),
          FlowParams(4, 8, 0),
          ),

        // Routing specification
        routingRelation = DimensionOrderedBidirectionalTorus2DDatelineRouting(),

        // other
        nocName = "Noc",
        skipValidationChecks = false,
        hasCtrl = false,
      )
    ), name = "Nnoc"))




  memAXI4SlaveNode := 
    AXI4UserYanker() := 
    AXI4IdIndexer(4) :=
    AXI4Deinterleaver(256/8) :=
    TLToAXI4() :=
    TLWidthWidget(memBeatBits / 8) := TLBuffer() :=* TLCacheCork() := sifiveCache.node := TLBuffer() := noc.node

  sysAXI4SlaveNode := 
    AXI4UserYanker() := 
    AXI4IdIndexer(4) :=
    AXI4Deinterleaver(256/8) :=
    TLToAXI4() :=
    TLWidthWidget(64 / 8) := noc.node



    noc.node := TLBuffer() := i_rift2Core.icacheClientNode
    noc.node := TLBuffer() := i_rift2Core.dcacheClientNode
    noc.node := TLBuffer() := i_rift2Core.mmuClientNode
    noc.node := TLBuffer() := i_rift2Core.prefetchClinetNode

    noc.node := TLBuffer() := i_rift2Core.systemClientNode
    noc.node := TLBuffer() := i_rift2Core.periphClientNode

    if( hasDebugger ) {
      // l1_xbar64 := TLBuffer() := i_debugger.get.dm.sbaClientNode
      i_debugger.get.dm.peripNode := TLBuffer():= TLFragmenter(8, 32) := TLBuffer() := noc.node      
    }

    i_aclint.node := TLBuffer() := noc.node
    i_plic.node   := TLBuffer() := noc.node



  val memory = InModuleBody {
    memAXI4SlaveNode.makeIOs()
  }

  val system = InModuleBody {
    sysAXI4SlaveNode.makeIOs()
  }
  


  lazy val module = new LazyModuleImp(this) {
    val io = IO( new Bundle{
      val JtagIO  = if (hasDebugger) {Some(new JtagIO())} else { None }
      val ndreset = if (hasDebugger) {Some(Output(Bool()))}  else { None }


      val interrupt = Input( Vec(nDevices, Bool()) )
      val rtc_clock = Input(Bool())
    })
  
    if( hasDebugger ) {
      i_debugger.get.module.io.JtagIO <> io.JtagIO.get
      io.ndreset.get := i_debugger.get.module.io.ndreset
      i_debugger.get.module.io.dm_cmm(0) <> i_rift2Core.module.io.dm.get
    }


    i_rift2Core.module.io.rtc_clock := io.rtc_clock


    i_aclint.module.io.rtc_clock := io.rtc_clock
    i_rift2Core.module.io.aclint := i_aclint.module.io.int(0)
    i_rift2Core.module.io.plic.mei := i_plic.module.io.context(0)
    i_rift2Core.module.io.plic.sei := false.B

    i_plic.module.io.interrupt := io.interrupt
  }




}











// class Rift2LinkA(isFlatten: Boolean = false)(implicit p: Parameters) extends LazyModule with HasRiftParameters{
//   def hasNoc = true

//   val i_rift2Core = LazyModule( new Rift2Core(isFlatten) )

//   val sifiveCache = if( hasL2 ) { Some(    LazyModule(new InclusiveCache(
//       cache = CacheParameters( level = 2, ways = 2, sets = 4, blockBytes = l1DW/8, beatBytes = l1BeatBits/8, hintsSkipProbe = false ),
//       micro = InclusiveCacheMicroParameters( writeBytes = memBeatBits/8, memCycles = 40, portFactor = 4),
//       control = None
//     )))
//   } else { None }


//   val chipLinkMst = LazyModule( new ChipLinkMaster)


//   val noc = LazyModule(new TLNoC(
//     TLNoCParams(
//       nodeMappings = DiplomaticNetworkNodeMapping(
//         inNodeMapping  = ListMap( "TLFragmenter[0]" -> 0, "system[0]"-> 0, "periph[0]"-> 0 ),
//         outNodeMapping = ListMap( "chipLinkMst[0]"  -> 4 ),
//       ),
//       nocParams = NoCParams(
//         // Physical specifications
//         topology = UnidirectionalTorus2D(3,3),
//         channelParamGen = (_, _) => UserChannelParams(
//           virtualChannelParams = Seq.fill(2)(UserVirtualChannelParams(2)),
//           crossingType = NoCrossing,
//           srcSpeedup = 1,
//           destSpeedup = 1
//         ),
//         ingresses = Seq(
//           UserIngressParams( destId = 0, payloadBits = 8 ),
//           UserIngressParams( destId = 0, payloadBits = 8 ),
//           UserIngressParams( destId = 0, payloadBits = 8 ),
//           UserIngressParams( destId = 0, payloadBits = 8 ),
//           UserIngressParams( destId = 0, payloadBits = 8 ),
//           UserIngressParams( destId = 0, payloadBits = 8 ),
//           UserIngressParams( destId = 0, payloadBits = 8 ),
//           UserIngressParams( destId = 0, payloadBits = 8 ),
//           UserIngressParams( destId = 0, payloadBits = 8 ),
//           // UserIngressParams( destId = 4, payloadBits = 8 ),
//         ),
//         egresses = Seq(
//           // UserEgressParams( srcId = 0, payloadBits = 8 ),
//           UserEgressParams( srcId = 4, payloadBits = 8 ),
//         ),
//         routerParams = (i: Int) => UserRouterParams(// Payload width. Must match payload width on all channels attached to this routing node
//           payloadBits = 8,
//           combineSAST = false,// Combines SA and ST stages (removes pipeline register)
//           combineRCVA = false,// Combines RC and VA stages (removes pipeline register)
//           coupleSAVA = false,// Adds combinational path from SA to VA
//           vcAllocator = (vP) => (p) => new RotatingSingleVCAllocator(vP)(p)
//         ),
//         // Flow specification
//         // (blocker, blockee) => bool
//         // If true, then blocker must be able to proceed when blockee is blocked
//         vNetBlocking = (_, _) => true,
//         flows           = Seq.tabulate(9, 9) { (s, d) => FlowParams(s, d, 0) }.flatten,

//         // Routing specification
//         routingRelation = DimensionOrderedUnidirectionalTorus2DDatelineRouting(),

//         // other
//         nocName = "Noc",
//         skipValidationChecks = false,
//         hasCtrl = false,
//       )
//     ), name = "Nnoc"))

//   val l1_xbarMem = TLXbar()
//   val linkMXbar = 
//     if( hasNoc ) { noc.node }
//     else {  TLXbar() }

//   // println(s"LinkXbar name is ${linkMXbar.name}")




//     chipLinkMst.chipLinkMasterNode := linkMXbar

//     if ( hasL2 ) {
//       linkMXbar :=* TLFIFOFixer() :=
//       TLFragmenter(minSize = 64/8, maxSize = 256/8, alwaysMin = true) :=
//       TLWidthWidget(memBeatBits / 8) :=* TLCacheCork() := sifiveCache.get.node := l1_xbarMem
//     } else {
//       linkMXbar :=* TLFIFOFixer() := TLFragmenter(minSize = 64/8, maxSize = 128/8, alwaysMin = true) := TLWidthWidget(l1BeatBits / 8) := l1_xbarMem
//     }
//       l1_xbarMem := TLBuffer() := i_rift2Core.icacheClientNode
//       l1_xbarMem := TLBuffer() := i_rift2Core.dcacheClientNode
//       l1_xbarMem := TLBuffer() := i_rift2Core.mmuClientNode
//       l1_xbarMem := i_rift2Core.prefetchClinetNode
 




//     linkMXbar := i_rift2Core.systemClientNode
//     linkMXbar := i_rift2Core.periphClientNode




//   lazy val module = new LazyModuleImp(this) {
//     val io = IO( new Bundle{

//       val rtc_clock = Input(Bool())

//       val dm        = if (hasDebugger) {Some(Flipped(new Info_DM_cmm))} else {None}

//       val aclint    = Input(new AClint_Bundle)
//       val plic      = Input(new Plic_Bundle)

//       val hspi_clk   = Output(Bool())//Input(Bool())
//       // val hspi_reset = Input(Bool())
//       val hspi_rx    = Input ( new HexSpiIO_Bundle )
//       val hspi_tx   = Output( new HexSpiIO_Bundle )
//       val hspi_oen = Output(Bool())
//     })
  

//     if( hasDebugger ) {
//       io.dm.get <> i_rift2Core.module.io.dm.get
//     }
//     i_rift2Core.module.io.aclint := io.aclint
//     i_rift2Core.module.io.plic := io.plic


//     io.hspi_tx := chipLinkMst.module.io.hspi_tx
//     chipLinkMst.module.io.hspi_rx := io.hspi_rx 
//     io.hspi_clk := chipLinkMst.module.io.hspi_clk// := 
//     // chipLinkMst.module.io.hspi_reset := io.hspi_reset
//     io.hspi_oen := chipLinkMst.module.io.hspi_oen

//     i_rift2Core.module.io.rtc_clock := io.rtc_clock


//   }

// }


// class Rift2LinkB(implicit p: Parameters) extends LazyModule with HasRiftParameters{
//   def hasNoc = true

//   val chipLinkSlv = LazyModule( new ChiplinkSlave)
//   val i_debugger = if ( hasDebugger) {Some(LazyModule(new Debugger(nComponents = 1)))} else {None}
//   val i_aclint = LazyModule( new AClint( nTiles = 1 ) )
//   val nDevices = 31
//   val i_plic = LazyModule( new Plic( nHarts = 1, nPriorities = 8, nDevices = nDevices ))

//   val memRange = AddressSet(0x00000000L, 0xffffffffL).subtract(AddressSet(0x0L, 0x7fffffffL))
//   val memAXI4SlaveNode = AXI4SlaveNode(Seq(
//     AXI4SlavePortParameters(
//       slaves = Seq(
//         AXI4SlaveParameters(
//           address = memRange,
//           regionType = RegionType.UNCACHED,
//           executable = true,
//           supportsRead = TransferSizes(1, 256/8),
//           supportsWrite = TransferSizes(1, 256/8)
//         )
//       ),
//       beatBytes = 128 / 8
//     )
//   ))

//   val sysRange = AddressSet(0x00000000L, 0x7fffffffL).subtract(AddressSet(0x00000000L, 0x1fffffffL))
//   val sysAXI4SlaveNode = AXI4SlaveNode(Seq(
//     AXI4SlavePortParameters(
//       slaves = Seq(
//         AXI4SlaveParameters(
//           address = sysRange,
//           regionType = RegionType.UNCACHED,
//           executable = true,
//           supportsRead = TransferSizes(1, 256/8),
//           supportsWrite = TransferSizes(1, 256/8)
//         )
//       ),
//       beatBytes = 64 / 8
//     )
//   ))


//   val linkSXbar = TLXbar()

//   linkSXbar := chipLinkSlv.ChipLinkSlaveNode

//   memAXI4SlaveNode := 
//     AXI4UserYanker() := 
//     AXI4IdIndexer(4) :=
//     AXI4Deinterleaver(256/8) :=
//     TLToAXI4() := TLWidthWidget(64 / 8) := linkSXbar

//   sysAXI4SlaveNode := 
//     AXI4UserYanker() := 
//     AXI4IdIndexer(4) :=
//     AXI4Deinterleaver(256/8) :=
//     TLToAXI4() := linkSXbar

//     if( hasDebugger ) {
//       i_debugger.get.dm.peripNode := TLFragmenter(8, 32) := linkSXbar
//     }
//     i_aclint.node := linkSXbar
//     i_plic.node   := linkSXbar


//   lazy val module = new LazyModuleImp(this) {
//     val io = IO( new Bundle{
//       val rtc_clock = Input(Bool())

//       val JtagIO  = if (hasDebugger) {Some(new JtagIO())} else { None }
//       val ndreset = if (hasDebugger) {Some(Output(Bool()))}  else { None }
//       val interrupt = Input( Vec(nDevices, Bool()) )

//       val dm        = if (hasDebugger) {Some(new Info_DM_cmm)} else {None}
//       val aclint    = Output(new AClint_Bundle)
//       val plic      = Output(new Plic_Bundle)

//       val hspi_clk   = Input(Bool())
//       val hspi_reset = Input(Bool())

//       val hspi_rx    = Input ( new HexSpiIO_Bundle )
//       val hspi_tx   = Output( new HexSpiIO_Bundle )
//     })
  
//     if( hasDebugger ) {
//       i_debugger.get.module.io.JtagIO <> io.JtagIO.get
//       io.ndreset.get := i_debugger.get.module.io.ndreset
//       i_debugger.get.module.io.dm_cmm(0) <> io.dm.get
//     }

//     i_aclint.module.io.rtc_clock := io.rtc_clock
//     io.aclint := i_aclint.module.io.int(0)

//     io.plic.mei := i_plic.module.io.context(0)
//     io.plic.sei := false.B

//     i_plic.module.io.interrupt := io.interrupt




//     chipLinkSlv.module.io.hspi_rx := io.hspi_rx
//     chipLinkSlv.module.io.hspi_clk := io.hspi_clk
//     chipLinkSlv.module.io.hspi_reset := io.hspi_reset

//     io.hspi_tx := chipLinkSlv.module.io.hspi_tx


//     val memory = memAXI4SlaveNode.makeIOs()
//     val system = sysAXI4SlaveNode.makeIOs()

//   }

// }



// class Rift2Link(implicit p: Parameters) extends LazyModule with HasRiftParameters{
//   def hasNoc = true

//   val rift2LinkA = LazyModule( new Rift2LinkA )
//   val rift2LinkB = LazyModule( new Rift2LinkB )
//   val nDevices = 31
//   lazy val module = new LazyModuleImp(this) {
//     override val desiredName = "Rift2Chip"
//     val io = IO(new Bundle{
//       val JtagIO  = if (hasDebugger) {Some(new JtagIO())} else { None }
//       val ndreset = if (hasDebugger) {Some(Output(Bool()))}  else { None }

//       val interrupt = Input( Vec(nDevices, Bool()) )
//       val rtc_clock = Input(Bool())

//       val hspi_clk   = Output(Bool())//Input(Bool())
//       val hspi_reset = Input(Bool())
//       val hspi_oen = Output(Bool())  

  
//     })



//     val memory = IO(chiselTypeOf(rift2LinkB.module.memory))
//     val system = IO(chiselTypeOf(rift2LinkB.module.system))
//     memory <> rift2LinkB.module.memory
//     system <> rift2LinkB.module.system

//     if( hasDebugger ) {
//       rift2LinkB.module.io.JtagIO.get <> io.JtagIO.get
//       io.ndreset.get := rift2LinkB.module.io.ndreset.get
//       rift2LinkA.module.io.dm.get <> rift2LinkB.module.io.dm.get
//     }

    
//     rift2LinkA.module.io.aclint := rift2LinkB.module.io.aclint
//     rift2LinkA.module.io.plic := rift2LinkB.module.io.plic


//     rift2LinkB.module.io.interrupt := io.interrupt
//     rift2LinkB.module.io.rtc_clock := io.rtc_clock

//     rift2LinkA.module.io.rtc_clock := io.rtc_clock

//     io.hspi_clk := rift2LinkA.module.io.hspi_clk
//     // rift2LinkA.module.io.hspi_reset := io.hspi_reset
//     rift2LinkA.module.io.hspi_rx <> rift2LinkB.module.io.hspi_tx
//     rift2LinkA.module.io.hspi_tx <> rift2LinkB.module.io.hspi_rx
//     io.hspi_oen := rift2LinkA.module.io.hspi_oen


//     rift2LinkB.module.io.hspi_clk := rift2LinkA.module.io.hspi_clk//io.hspi_clk
//     rift2LinkB.module.io.hspi_reset := io.hspi_reset

//   }
// }
