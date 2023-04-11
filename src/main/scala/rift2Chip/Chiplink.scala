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


package rift2Chip

import chisel3._
import chisel3.util._

import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import freechips.rocketchip.util._

class TLReq_Bundle extends Bundle{
  val paddr = UInt(32.W)
  val wdata = UInt(64.W)
  val wmask = UInt(8.W)
}

class TLRsp_Bundle extends Bundle{
  val rdata = UInt(64.W)
}

class HexSpiIO_Bundle extends Bundle{
  val enable = Bool()
  val data   = UInt(16.W)
}


class ChipLinkMaster(implicit p: Parameters) extends LazyModule{
  val chipLinkMasterNode = TLManagerNode(Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address = Seq(AddressSet(0x00000000L, 0xffffffffL)),
        regionType = RegionType.UNCACHED,
        executable = true,
        supportsGet         = TransferSizes(64/8, 64/8),
        supportsPutFull     = TransferSizes(64/8, 64/8),
        supportsPutPartial  = TransferSizes(64/8, 64/8)

      )),
      beatBytes = 64/8)))

  lazy val module = new ChipLinkMasterImp(this)

  class TLSlave(edge: TLEdgeIn) extends Module{

    class TLSlaveIO extends Bundle{
      val req = DecoupledIO(new TLReq_Bundle)
      val rsp = Flipped(DecoupledIO(new TLRsp_Bundle))

      val a = Flipped(new DecoupledIO(new TLBundleA(edge.bundle)))
      val d = new DecoupledIO(new TLBundleD(edge.bundle))
    }

    val io = IO(new TLSlaveIO)

    // val ( tlMstBus, tlMstEdge ) = chipLinkMasterNode.in.head


    val reqInfo = Reg(new TLReq_Bundle)
    val rspInfo = Reg(new TLRsp_Bundle)
    val tlaInfo = Reg(new TLBundleA(edge.bundle))

    val reqValid = RegInit(false.B)
    val rspReady = RegInit(false.B)
    val tlAReady  = RegInit(true.B)
    val tlDValid = RegInit(false.B)

    val isRead  = Reg(Bool())

    io.req.valid := reqValid
    io.req.bits  := reqInfo
    io.rsp.ready := rspReady
    io.a.ready := tlAReady
    io.d.valid := tlDValid


    when( io.a.fire ) {
      tlaInfo := io.a.bits
      reqInfo.paddr := io.a.bits.address
      when( (io.a.bits.opcode === 0.U) || (io.a.bits.opcode === 1.U) ) {
        reqInfo.wdata := io.a.bits.data
        reqInfo.wmask := io.a.bits.mask
        isRead := false.B
      } .elsewhen( io.a.bits.opcode === 4.U ) {
        reqInfo.wdata := 0.U
        reqInfo.wmask := 0.U
        isRead := true.B
      } .otherwise{
        assert( false.B, "Assert Failed! Unsupport Operation!" )
      }
    }

    when( io.a.fire ){
      reqValid := true.B
      tlAReady   := false.B
    } .elsewhen( io.req.fire ) {
      reqValid := false.B
      rspReady := true.B
    } .elsewhen( io.rsp.fire ) {
      rspReady := false.B
      tlDValid := true.B
    } .elsewhen( io.d.fire ) {
      tlAReady := true.B
      tlDValid := false.B
    }


    when( io.rsp.fire ) {
      rspInfo := io.rsp.bits
    }

    when(isRead) {
      io.d.bits := edge.AccessAck(tlaInfo, rspInfo.rdata)
    } .otherwise {
      io.d.bits := edge.AccessAck(tlaInfo)
    }
  }


  class HexSpiMaster extends Module{

    class HexSpiMasterIO extends Bundle{
      val hspi_clk   = Output(Bool())//Input(Bool())
      // val hspi_reset = Input(Bool())
      val hspi_rx    = Input ( new HexSpiIO_Bundle )
      val hspi_tx   = Output( new HexSpiIO_Bundle )
      val hspi_oen = Output(Bool())

      val req = Flipped(DecoupledIO(new TLReq_Bundle))
      val rsp = DecoupledIO(new TLRsp_Bundle)
    }

    val io: HexSpiMasterIO = IO(new HexSpiMasterIO)

    // withClockAndReset( (~io.hspi_clk).asClock, io.hspi_reset ) {
      io.hspi_clk := clock.asBool()

      val txCounter = RegInit(15.U(4.W))
      val rxCounter = RegInit(0.U(3.W))
      val isTxRxn = RegInit(true.B)

      val reqInfo = Reg( UInt( (16*7).W ) )
      val rspInfo = Reg( UInt( (16*4).W) )

      val reqReady = RegInit(true.B)
      val rspValid = RegInit(false.B)

      io.hspi_oen := ~isTxRxn

      io.req.ready := reqReady
      io.rsp.valid := rspValid
      io.rsp.bits.rdata  := rspInfo


      
      when( io.req.fire ) {
        assert( isTxRxn )
        val updateInfo = Cat(io.req.bits.paddr, io.req.bits.wdata, io.req.bits.wmask, 0.U(8.W) )
        reqReady := false.B
        reqInfo := updateInfo
        txCounter := 1.U

        io.hspi_tx.data := updateInfo( 15, 0 )
        io.hspi_tx.enable := true.B
      } .elsewhen( txCounter < 7.U ) {
        assert( isTxRxn )
        txCounter := txCounter + 1.U
        val updateInfo = reqInfo >> 16
        reqInfo := updateInfo

        io.hspi_tx.data   := updateInfo( 15, 0 )
        io.hspi_tx.enable := true.B

      } .elsewhen( txCounter === 7.U  ) {
          assert( isTxRxn )
          isTxRxn := false.B
          // rxCounter := 0.U
          txCounter := 15.U

          io.hspi_tx.data   := 0.U
          io.hspi_tx.enable := false.B   
      } .otherwise {
        io.hspi_tx.data   := 0.U
        io.hspi_tx.enable := false.B       
      }


      when( io.hspi_rx.enable ) {
        assert( ~isTxRxn )
        assert( rxCounter < 4.U )
        rxCounter := rxCounter + 1.U
        rspInfo := (Cat(io.hspi_rx.data, rspInfo )) >> 16
      } .elsewhen( rxCounter === 4.U ) {
        assert( ~isTxRxn )
        isTxRxn := true.B
        rspValid := true.B
        rxCounter := 0.U
      } .elsewhen( io.rsp.fire ) {
        rspValid := false.B
        reqReady := true.B
      }

    

    // }

  }


  class ChipLinkMasterImp(outer: ChipLinkMaster) extends LazyModuleImp(outer) {

    class ChipLinkMasterIO extends Bundle{
      val hspi_clk   = Output(Bool())
      //val hspi_reset = Input(Bool())
      val hspi_rx    = Input ( new HexSpiIO_Bundle )
      val hspi_tx   = Output( new HexSpiIO_Bundle )
      val hspi_oen = Output(Bool())
    }

    val io: ChipLinkMasterIO = IO(new ChipLinkMasterIO)


    val ( tlMstBus, tlMstEdge ) = chipLinkMasterNode.in.head

    val tlSlave = Module(new TLSlave(tlMstEdge))
    val hspiMst = Module(new HexSpiMaster)

    tlSlave.io.a.valid := tlMstBus.a.valid 
    tlSlave.io.a.bits := tlMstBus.a.bits 
    tlMstBus.a.ready := tlSlave.io.a.ready

    tlMstBus.d.valid := tlSlave.io.d.valid
    tlMstBus.d.bits := tlSlave.io.d.bits
    tlSlave.io.d.ready := tlMstBus.d.ready

    io.hspi_clk := hspiMst.io.hspi_clk
    // hspiMst.io.hspi_reset := io.hspi_reset
    hspiMst.io.hspi_rx := io.hspi_rx
    io.hspi_tx   := hspiMst.io.hspi_tx
    io.hspi_oen  := hspiMst.io.hspi_oen

    tlSlave.io.rsp <> hspiMst.io.rsp
    tlSlave.io.req <> hspiMst.io.req

    // val req_ToAsync = Wire(new AsyncBundle(new TLReq_Bundle))
    // val resp_ToAsync = Wire(new AsyncBundle(new TLRsp_Bundle))

    // tlSlave.io.rsp <> FromAsyncBundle( resp_ToAsync, 2 )
    // req_ToAsync <> ToAsyncBundle( tlSlave.io.req, AsyncQueueParams(sync=2) )
      
    // withClockAndReset( (~io.hspi_clk).asClock, io.hspi_reset ) {
    //   hspiMst.io.req <> FromAsyncBundle( req_ToAsync, 2 )      
    //   resp_ToAsync <> ToAsyncBundle( hspiMst.io.rsp, AsyncQueueParams(sync=2) )
    // }


  }


}
























class ChiplinkSlave(implicit p: Parameters) extends LazyModule{
  val ChipLinkSlaveNode = TLClientNode( Seq( TLMasterPortParameters.v1( Seq(TLMasterParameters.v1(
      name = "ChipLink_slave",
      sourceId = IdRange(0,1)))
    )
  ))

  lazy val module = new ChipLinkSlaveImp(this)


  class TLMaster(edge: TLEdgeOut) extends Module{

    class TLMasterIO extends Bundle{
      val req = Flipped(DecoupledIO(new TLReq_Bundle))
      val rsp = DecoupledIO(new TLRsp_Bundle)

      val a = new DecoupledIO(new TLBundleA(edge.bundle))
      val d = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
    }

    val io: TLMasterIO = IO(new TLMasterIO)

    // val (tlSlvBus, tlSlvEdge) = ChipLinkSlaveNode.out.head

    val reqInfo = Reg(new TLReq_Bundle)
    val rspInfo = Reg(new TLRsp_Bundle)

    val reqReady  = RegInit(true.B)
    val rspValid  = RegInit(false.B)
    val tlAValid = RegInit(false.B)
    val tlDReady = RegInit(false.B)

    val isRead = Reg(Bool())



    io.req.ready := reqReady
    io.rsp.valid := rspValid
    io.a.valid := tlAValid
    io.d.ready := tlDReady

    when( reqInfo.wmask === 0.U ) {
      io.a.bits := 
        edge.Get(
          fromSource = 0.U,
          toAddress = reqInfo.paddr,
          lgSize = log2Ceil(64/8).U
        )._2
    } .otherwise{
      io.a.bits :=
        edge.Put(
          fromSource = 0.U,
          toAddress = reqInfo.paddr,
          lgSize = log2Ceil(64/8).U,
          data = reqInfo.wdata,
          mask = reqInfo.wmask
        )._2  
    }

    when ( io.req.fire ){
      tlAValid := true.B
      reqReady := false.B
    } .elsewhen( io.a.fire ) {
      tlAValid := false.B
      tlDReady := true.B
    } .elsewhen( io.d.fire ) {
      rspValid := true.B
      tlDReady := false.B
    } .elsewhen( io.rsp.fire ) {
      rspValid := false.B
      reqReady := true.B
    }

    when( io.req.fire ) {
      reqInfo := io.req.bits
      isRead := Mux( io.req.bits.wmask === 0.U, true.B, false.B )
    }

    when( io.d.fire ) {
      rspInfo.rdata := Mux( isRead, io.d.bits.data , 0.U )
    }

    io.rsp.bits := rspInfo

  }


  class HexSpiSlave extends RawModule{

    class HexSpiSlaveIO extends Bundle{
      val hspi_clk   = Input(Bool())
      val hspi_reset = Input(Bool())
      val hspi_rx    = Input ( new HexSpiIO_Bundle )
      val hspi_tx   = Output( new HexSpiIO_Bundle )

      val req = DecoupledIO(new TLReq_Bundle)
      val rsp = Flipped(DecoupledIO(new TLRsp_Bundle))
    }

    val io: HexSpiSlaveIO = IO(new HexSpiSlaveIO)

    withClockAndReset( io.hspi_clk.asClock, io.hspi_reset ) {
      val txCounter = RegInit(15.U(4.W))
      val rxCounter = RegInit(0.U(3.W))
      val isTxRxn = RegInit(false.B)

      val reqInfo = Reg( UInt( (16*7).W ) )
      val rspInfo = Reg( UInt( (16*4).W) )

      val reqValid = RegInit(false.B)
      val rspReady = RegInit(true.B)

      io.req.valid := reqValid
      io.req.bits.paddr := reqInfo(111, 80)
      io.req.bits.wdata := reqInfo(79, 16)
      io.req.bits.wmask := reqInfo(15,8)
      
      io.rsp.ready := rspReady

      when( io.hspi_rx.enable ){
        assert( ~isTxRxn )
        assert( rxCounter < 7.U )
        rxCounter := rxCounter + 1.U
        reqInfo := Cat( io.hspi_rx.data , reqInfo ) >> 16
      } .elsewhen( rxCounter === 7.U & ~isTxRxn) {
        isTxRxn := true.B
        reqValid := true.B
      } .elsewhen( io.req.fire ) {
        reqValid := false.B
        rspReady := true.B
      }

      when( io.rsp.fire ) {
        val updateInfo = io.rsp.bits.rdata
        rspReady := false.B
        rspInfo := updateInfo
        txCounter := 1.U
        
        io.hspi_tx.data := updateInfo( 15, 0 )
        io.hspi_tx.enable := true.B
      } .elsewhen( txCounter < 4.U ) {
        txCounter := txCounter + 1.U
        val updateInfo = rspInfo >> 16
        rspInfo := updateInfo

        io.hspi_tx.data   := updateInfo( 15, 0 )
        io.hspi_tx.enable := true.B
      } .elsewhen( txCounter === 4.U & isTxRxn ) {
        isTxRxn := false.B
        rxCounter := 0.U

        io.hspi_tx.data   := 0.U
        io.hspi_tx.enable := false.B  
      } .otherwise {
        io.hspi_tx.data   := 0.U
        io.hspi_tx.enable := false.B       
      }

    }
  }

  class ChipLinkSlaveImp(outer: ChiplinkSlave) extends LazyModuleImp(outer) {

    class ChipLinkSlaveIO extends Bundle{
      val hspi_clk   = Input(Bool())
      val hspi_reset = Input(Bool())
      val hspi_rx    = Input ( new HexSpiIO_Bundle )
      val hspi_tx   = Output( new HexSpiIO_Bundle )
    }

    val io: ChipLinkSlaveIO = IO(new ChipLinkSlaveIO)

    val (tlSlvBus, tlSlvEdge) = ChipLinkSlaveNode.out.head
    val tlMaster = Module(new TLMaster(tlSlvEdge))
    val hspiSlv = Module(new HexSpiSlave)


    tlSlvBus.a.valid := tlMaster.io.a.valid
    tlSlvBus.a.bits := tlMaster.io.a.bits
    tlMaster.io.a.ready := tlSlvBus.a.ready

    tlMaster.io.d.valid := tlSlvBus.d.valid
    tlMaster.io.d.bits := tlSlvBus.d.bits
    tlSlvBus.d.ready := tlMaster.io.d.ready

    hspiSlv.io.hspi_clk := io.hspi_clk
    hspiSlv.io.hspi_reset := io.hspi_reset
    hspiSlv.io.hspi_rx := io.hspi_rx
    io.hspi_tx   := hspiSlv.io.hspi_tx


    val req_ToAsync = Wire(new AsyncBundle(new TLReq_Bundle))
    val resp_ToAsync = Wire(new AsyncBundle(new TLRsp_Bundle))

    tlMaster.io.req  <> FromAsyncBundle( req_ToAsync, 2 )
    resp_ToAsync <> ToAsyncBundle( tlMaster.io.rsp, AsyncQueueParams(sync=2) )
      
    withClockAndReset( io.hspi_clk.asClock, io.hspi_reset ) {
      hspiSlv.io.rsp <> FromAsyncBundle( resp_ToAsync, 2 )      
      req_ToAsync <> ToAsyncBundle( hspiSlv.io.req, AsyncQueueParams(sync=2) )
    }   

  }


}


