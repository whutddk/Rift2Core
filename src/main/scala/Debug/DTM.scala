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

package Debug

import chisel3._
import chisel3.util._

class DMIAccess extends Bundle {
  val addr = UInt(8.W)
  val data = UInt(32.W)
  val op = UInt(2.W)
}

class DTMcs extends Bundle {
  val reserved1 = UInt(15.W)
  val dmireset = Bool()
  val reserved0 = UInt(1.W)
  val dmiIdleCycles = UInt(3.W)
  val dmiStatus = UInt(2.W)
  val debugAddrBits = UInt(6.W)
  val debugVersion = UInt(4.W)
}


class DMIReq extends DMIAccess
class DMIResp extends DMIAccess

class DMIIO() extends Bundle {
  val req = new DecoupledIO(new DMIReq())
  val resp = Flipped(new DecoupledIO(new DMIResp))
}


class ClockedDMIIO(implicit val p: Parameters) extends ParameterizedBundle()(p){
  val dmi      = new DMIIO()(p)
  val dmiClock = Output(Clock())
  val dmiReset = Output(Reset())
}



class DebugTransportModuleJTAG extends RawModule {
  val io = IO(new Bundle {
    val dmi = new DMIIO
    val JtagIO = Flipped(new JTAGIO(hasTRSTn = false)) // TODO: re-use SystemJTAGIO here?
  })

  withClockAndReset(io.JtagIO.TCK, io.JtagIO.jtag_reset) {

    val dmiInFlight = RegInit(false.B)
    val stickyBusyReg = RegInit(false.B)
    val stickyNonzeroRespReg = RegInit(false.B)

    val downgradeOpReg = RegInit(false.B) // downgrade op because prev. failed.

    val nonzeroResp = Wire(Bool())

    val busyResp    = Wire(new DMIAccess)
    busyResp.addr  := 0.U
    busyResp.resp  := 3.U
    busyResp.data  := 0.U

    val dmiResp     = Wire(new DMIAccess)
    val nopResp     = Wire(0.U.asTypeOf(new DMIAccess))


    val dmiReqReg  = RegInit(0.U.asTypeOf(new DMIReq(8)))
    val dmiReqValidReg = RegInit(false.B)

    val dmiStatus = Wire(UInt(2.W))

    //--------------------------------------------------------
    // DTM Info Chain Declaration

    dmiStatus := Cat(stickyNonzeroRespReg, stickyNonzeroRespReg | stickyBusyReg)

    val dtmcs = Wire(new DTMcs)
    dtmcs.debugVersion  := 1.U
    dtmcs.debugAddrBits := 8.U
    dtmcs.dmiStatus     := dmiStatus
    dtmcs.dmiIdleCycles := c.debugIdleCycles.U
    dtmcs.reserved0     := 0.U
    dtmcs.dmireset      := false.B // This is write-only
    dtmcs.reserved1     := 0.U

    val dtmcsChain = Module(new CaptureUpdateChain(gen = new DTMcs))
    dtmcsChain.io.capture.bits := dtmcs

    //--------------------------------------------------------
    // Debug Access Chain Declaration

    val dmiAccessChain = Module(new CaptureUpdateChain(gen = new DMIAccess))

    //--------------------------------------------------------
    // Debug Access Support

    // Busy Register. We become busy when we first try to send a request.
    // We stop being busy when we accept a response.

    when (io.dmi.req.valid) { dmiInFlight := true.B }
    when (io.dmi.resp.fire) { dmiInFlight := false.B }

    // We are busy during a given CAPTURE
    // if we haven't received a valid response yet or if we
    // were busy last time without a reset.
    // dmiInFlight will still be set when we check it,
    // so the logic for checking busy looks ahead.
    val is_busy = (dmiInFlight & ~io.dmi.resp.valid) | stickyBusyReg;

    // Downgrade/Skip. We make the decision to downgrade or skip
    // during every CAPTURE_DR, and use the result in UPDATE_DR.
    // The sticky versions are reset by write to dmiReset in DTM_INFO.
    when (dmiAccessChain.io.update.valid) {
      downgradeOpReg := false.B
    }
    when (dmiAccessChain.io.capture.is_valid) {
      downgradeOpReg := (~is_busy & nonzeroResp) 
      stickyBusyReg := is_busy
      stickyNonzeroRespReg := nonzeroResp
    }

    //these two kind of sticky error can only clear by debugger dmireset
    when (dtmcsChain.io.update.valid) {
      when (dtmcsChain.io.update.bits.dmireset) {
        stickyNonzeroRespReg := false.B
        stickyBusyReg := false.B
      }
    }

    // Especially for the first request, we must consider dtmResp.valid,
    // so that we don't consider junk in the FIFO to be an error response.
    // The current specification says that any non-zero response is an error.
    nonzeroResp := stickyNonzeroRespReg | (io.dmi.resp.valid & (io.dmi.resp.bits.resp =/= 0.U))
   


    dmiResp.addr := dmiReqReg.addr
    dmiResp.resp := io.dmi.resp.bits.resp
    dmiResp.data := io.dmi.resp.bits.data

    //--------------------------------------------------------
    // Debug Access Chain Implementation

    dmiAccessChain.io.capture.bits := Mux(is_busy, busyResp, Mux(io.dmi.resp.valid, dmiResp, nopResp))

    //--------------------------------------------------------
    // Drive Ready Valid Interface

    when (dmiAccessChain.io.update.valid) {
      when (stickyBusyReg) {
        // Do Nothing
      }.elsewhen (downgradeOpReg || (dmiAccessChain.io.update.bits.op === 0.U)) {
        //Do Nothing
        dmiReqReg.addr := 0.U
        dmiReqReg.data := 0.U
        dmiReqReg.op   := 0.U
      }.otherwise {
        dmiReqReg := dmiAccessChain.io.update.bits
        dmiReqValidReg := true.B
        assert( ~io.dmi.req.fire, "Conflicting updates for dmiReqValidReg, should not happen.");
      }
    }

    when (io.dmi.req.fire) {
      dmiReqValidReg := false.B
    }

    io.dmi.resp.ready := Mux1H(Seq(
      (dmiReqReg.op === 2.U) -> io.dmi.resp.valid, // for write operations confirm resp immediately because we don't care about data
      (dmiReqReg.op === 1.U) -> dmiAccessChain.io.capture.is_valid & ~is_busy,// for read operations confirm resp when we capture the data
    ))
 
    io.dmi.req.valid := dmiReqValidReg
    io.dmi.req.bits := dmiReqReg

    //--------------------------------------------------------
    // Actual JTAG TAP
    val idcode = WireDefault(0.U.asTypeOf(new JTAGIdcodeBundle()))
    idcode.always1    := 1.U
    idcode.version    := 0.U(4.W)
    idcode.partNumber := 0.U(16.W)
    idcode.mfrId      := 0.U(11.W)

    val tapIO = JtagTapGenerator(
      instructions = Map(
        dtmJTAGAddrs.DMI_ACCESS -> dmiAccessChain,
        dtmJTAGAddrs.DTM_INFO   -> dtmcsChain
      )
    )

    tapIO.idcode := idcode
    tapIO.JtagIO <> io.JtagIO

    //--------------------------------------------------------
    // TAP Test-Logic-Reset state synchronously resets the debug registers.

    when (tapIO.output.tapIsInTestLogicReset) {
      dmiInFlight := false.B
      stickyBusyReg := false.B
      stickyNonzeroRespReg := false.B
      downgradeOpReg := false.B
      dmiReqValidReg := false.B
    }

  }
}



