/* chisel-jtag license terms

Copyright (c) 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/

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
import chisel3.experimental.DataMirror
import chisel3.internal.firrtl.KnownWidth
import chisel3.util._

class JtagIO() extends Bundle {
  val jtag_reset = Input(AsyncReset())

  val TCK        = Input(Clock())
  val TMS        = Input(Bool())
  val TDI        = Input(Bool())
  val TDO        = Output(Bool())
  val TDO_driven = Output(Bool())
}



object JtagState {
  sealed abstract class State(val id: Int) {
    def U: UInt = id.U(State.width.W)
  }

  object State {
    import scala.language.implicitConversions

    implicit def toInt(x: State) = x.id
    implicit def toBigInt(x: State):BigInt = x.id

    // TODO: this could be automatically generated with macros and stuff
    val all: Set[State] = Set(
      TestLogicReset,
      RunTestIdle,
      SelectDRScan,
      CaptureDR,
      ShiftDR,
      Exit1DR,
      PauseDR,
      Exit2DR,
      UpdateDR,
      SelectIRScan,
      CaptureIR,
      ShiftIR,
      Exit1IR,
      PauseIR,
      Exit2IR,
      UpdateIR
    )
    val width = log2Ceil(all.size)
    def chiselType() = UInt(width.W)
  }

  // States as described in 6.1.1.2, numeric assignments from example in Table 6-3
  case object TestLogicReset extends State(15)  // no effect on system logic, entered when TMS high for 5 TCK rising edges
  case object RunTestIdle extends State(12)  // runs active instruction (which can be idle)
  case object SelectDRScan extends State(7)
  case object CaptureDR extends State(6)  // parallel-load DR shifter when exiting this state (if required)
  case object ShiftDR extends State(2)  // shifts DR shifter from TDI towards TDO, last shift occurs on rising edge transition out of this state
  case object Exit1DR extends State(1)
  case object PauseDR extends State(3)  // pause DR shifting
  case object Exit2DR extends State(0)
  case object UpdateDR extends State(5)  // parallel-load output from DR shifter on TCK falling edge while in this state (not a rule?)
  case object SelectIRScan extends State(4)
  case object CaptureIR extends State(14)  // parallel-load IR shifter with fixed logic values and design-specific when exiting this state (if required)
  case object ShiftIR extends State(10)  // shifts IR shifter from TDI towards TDO, last shift occurs on rising edge transition out of this state
  case object Exit1IR extends State(9)
  case object PauseIR extends State(11)  // pause IR shifting
  case object Exit2IR extends State(8)
  case object UpdateIR extends State(13)  // latch IR shifter into IR (changes to IR may only occur while in this state, latch on TCK falling edge)
}

class JTAGIdcodeBundle extends Bundle {
  val version = UInt(4.W)
  val partNumber = UInt(16.W)
  val mfrId = UInt(11.W)
  val always1 = UInt(1.W)
}

class JtagOutput(irLength: Int = 5) extends Bundle {
  val state = Output(JtagState.State.chiselType())  // state, transitions on TCK rising edge
  val instruction = Output(UInt(irLength.W))  // current active instruction
  val tapIsInTestLogicReset = Output(Bool())  // synchronously asserted in Test-Logic-Reset state, should NOT hold the FSM in reset
}

class JtagStateMachine() extends Module{
  val io = IO(new Bundle{
    val tms = Input(Bool())
    val currState = Output(JtagState.State.chiselType)
  })

  //work with TCK and jtag_reset
  val nextState = WireDefault(JtagState.TestLogicReset.U)
  val currState = RegNext(next=nextState, init=JtagState.TestLogicReset.U)
  switch (currState) {
    is (JtagState.TestLogicReset.U) {
      nextState := Mux(io.tms, JtagState.TestLogicReset.U, JtagState.RunTestIdle.U)
    } 
    is (JtagState.RunTestIdle.U) {
      nextState := Mux(io.tms, JtagState.SelectDRScan.U, JtagState.RunTestIdle.U)
    } 
    is (JtagState.SelectDRScan.U) {
      nextState := Mux(io.tms, JtagState.SelectIRScan.U, JtagState.CaptureDR.U)
    } 
    is (JtagState.CaptureDR.U) {
      nextState := Mux(io.tms, JtagState.Exit1DR.U, JtagState.ShiftDR.U)
    } 
    is (JtagState.ShiftDR.U) {
      nextState := Mux(io.tms, JtagState.Exit1DR.U, JtagState.ShiftDR.U)
    } 
    is (JtagState.Exit1DR.U) {
      nextState := Mux(io.tms, JtagState.UpdateDR.U, JtagState.PauseDR.U)
    } 
    is (JtagState.PauseDR.U) {
      nextState := Mux(io.tms, JtagState.Exit2DR.U, JtagState.PauseDR.U)
    } 
    is (JtagState.Exit2DR.U) {
      nextState := Mux(io.tms, JtagState.UpdateDR.U, JtagState.ShiftDR.U)
    } 
    is (JtagState.UpdateDR.U) {
      nextState := Mux(io.tms, JtagState.SelectDRScan.U, JtagState.RunTestIdle.U)
    } 
    is (JtagState.SelectIRScan.U) {
      nextState := Mux(io.tms, JtagState.TestLogicReset.U, JtagState.CaptureIR.U)
    } 
    is (JtagState.CaptureIR.U) {
      nextState := Mux(io.tms, JtagState.Exit1IR.U, JtagState.ShiftIR.U)
    } 
    is (JtagState.ShiftIR.U) {
      nextState := Mux(io.tms, JtagState.Exit1IR.U, JtagState.ShiftIR.U)
    } 
    is (JtagState.Exit1IR.U) {
      nextState := Mux(io.tms, JtagState.UpdateIR.U, JtagState.PauseIR.U)
    } 
    is (JtagState.PauseIR.U) {
      nextState := Mux(io.tms, JtagState.Exit2IR.U, JtagState.PauseIR.U)
    } 
    is (JtagState.Exit2IR.U) {
      nextState := Mux(io.tms, JtagState.UpdateIR.U, JtagState.ShiftIR.U)
    } 
    is (JtagState.UpdateIR.U) {
      nextState := Mux(io.tms, JtagState.SelectDRScan.U, JtagState.RunTestIdle.U)
    }
  }

    io.currState := currState

}

class Capture[+T <: Data](gen: T) extends Bundle {
  val bits = Input(gen)  // data to capture, should be always valid
  val is_valid = Output(Bool())  // will be high in capture state (single cycle), captured on following rising edge
}


class ShifterIO extends Bundle {
  val shift = Bool()  // advance the scan chain on clock high
  val data = Bool()  // as input: bit to be captured into shifter MSB on next rising edge; as output: value of shifter LSB
  val capture = Bool()  // high in the CaptureIR/DR state when this chain is selected
  val update = Bool()  // high in the UpdateIR/DR state when this chain is selected

  /** Sets a output shifter IO's control signals from a input shifter IO's control signals.
    */
  def chainControlFrom(in: ShifterIO): Unit = {
    shift := in.shift
    capture := in.capture
    update := in.update
  }
}

trait ChainIO extends Bundle {
  val chainIn = Input(new ShifterIO)
  val chainOut = Output(new ShifterIO)
}

trait Chain extends Module {
  val io: ChainIO
}


class JtagBypassChain extends Chain {
  class JtagBypassChainIO extends ChainIO {
    val JtagIO = new JtagIO
  }
  val io = IO(new JtagBypassChainIO)

  withClockAndReset(io.JtagIO.TCK, io.JtagIO.jtag_reset) {
    io.chainOut.chainControlFrom(io.chainIn)

    val reg = Reg(Bool())  // 10.1.1a single shift register stage

    io.chainOut.data := reg

    when (io.chainIn.capture) {
      reg := false.B  // 10.1.1b capture logic 0 on TCK rising
    } .elsewhen (io.chainIn.shift) {
      reg := io.chainIn.data
    }
    assert(!(io.chainIn.capture && io.chainIn.update)
        && !(io.chainIn.capture && io.chainIn.shift)
        && !(io.chainIn.update && io.chainIn.shift))
  }
}

class CaptureChain[+T <: Data](gen: T) extends Chain {
  class CaptureChainIO extends ChainIO {
    val capture = new Capture(gen)
    val JtagIO = new JtagIO    
  }
  val io = IO(new CaptureChainIO)

  withClockAndReset(io.JtagIO.TCK, io.JtagIO.jtag_reset) {
    io.chainOut.chainControlFrom(io.chainIn)

    val n = DataMirror.widthOf(gen) match {
      case KnownWidth(x) => x
      case _ => require(false, s"can't generate chain for unknown width data type $gen"); -1
    }

    val regs = (0 until n).map(x => Reg(Bool()))

    io.chainOut.data := regs(0)
    
    when (io.chainIn.capture) {
      (0 until n) map (x => regs(x) := io.capture.bits.asUInt()(x))
      io.capture.is_valid := true.B
    } .elsewhen (io.chainIn.shift) {
      regs(n-1) := io.chainIn.data
      (0 until n-1) map (x => regs(x) := regs(x+1))
      io.capture.is_valid := false.B
    } .otherwise {
      io.capture.is_valid := false.B
    }
    assert(!(io.chainIn.capture && io.chainIn.update)
        && !(io.chainIn.capture && io.chainIn.shift)
        && !(io.chainIn.update && io.chainIn.shift))
  }
}

class CaptureUpdateChain[+T <: Data](gen: T) extends Chain {
  class CaptureUpdateChainIO extends ChainIO {
    val capture = new Capture(gen)
    val update = Valid(gen)    
  }
  val io = IO(new CaptureUpdateChainIO)

    val n = DataMirror.widthOf(gen) match {
      case KnownWidth(x) => x
      case _ => require(false, s"can't generate chain for unknown width data type $gen"); -1
    }

    val regs = (0 until n).map(x => Reg(Bool()))
    io.chainOut.data := regs(0)
    io.chainOut.chainControlFrom(io.chainIn)
    io.update.bits := Cat(regs.reverse)

    when(io.chainIn.capture) {
      for( i <- 0 until n ) yield { regs(i) := io.capture.bits.asUInt()(i) }
      io.capture.is_valid := true.B
      io.update.valid := false.B
    } .elsewhen(io.chainIn.update) {
      io.capture.is_valid := false.B
      io.update.valid := true.B
    } .elsewhen(io.chainIn.shift) {
      regs(n-1) := io.chainIn.data
      for ( i <- 0 until n-1 ) yield { regs(i) := regs(i+1) }
      io.capture.is_valid := false.B
      io.update.valid := false.B
    } .otherwise {
      io.capture.is_valid := false.B
      io.update.valid := false.B
    }

    assert(!(io.chainIn.capture && io.chainIn.update)
        && !(io.chainIn.capture && io.chainIn.shift)
        && !(io.chainIn.update && io.chainIn.shift))    

}

class JtagBlockIO(irLength: Int = 5) extends Bundle {
  val JtagIO = new JtagIO()
  val out = new JtagOutput(irLength)
  val idcode = Input(new JTAGIdcodeBundle())
}

class JtagControllerIO(irLength: Int = 5) extends JtagBlockIO(irLength) {
  val dataChainOut = Output(new ShifterIO)
  val dataChainIn = Input(new ShifterIO)
}


class JtagTapController() extends Module {
  val io = IO(new JtagControllerIO)

  // val io = IO(new JtagControllerIO(5))
  val tdo = Wire(Bool())  // 4.4.1c TDI should appear here uninverted after shifting
  val tdo_driven = Wire(Bool())

  val clock_falling = WireDefault((~io.JtagIO.TCK.asBool).asClock)

  val tapIsInTestLogicReset = Wire(Bool())

  val currState = Wire(JtagState.State.chiselType)

  io.out.state := currState

  val stateMachine = {
    val mdl = Module(new JtagStateMachine)
    mdl.suggestName("stateMachine")
    mdl.clock := io.JtagIO.TCK
    mdl.reset := io.JtagIO.jtag_reset
    mdl.io.tms := io.JtagIO.TMS
    currState := mdl.io.currState
    mdl
  }
  
  withClockAndReset(clock_falling, io.JtagIO.jtag_reset) {
    io.JtagIO.TDO   := RegNext(next=tdo, init=false.B).suggestName("tdoReg")
    io.JtagIO.TDO_driven := RegNext(next=tdo_driven, init=false.B).suggestName("tdoeReg")
  }

  val irChain = {
    val mdl = Module(new CaptureUpdateChain(UInt(5.W)))
    mdl.suggestName("irChain")
    mdl.clock := io.JtagIO.TCK
    mdl.reset := io.JtagIO.jtag_reset

    mdl.io.chainIn.shift := currState === JtagState.ShiftIR.U
    mdl.io.chainIn.data := io.JtagIO.TDI
    mdl.io.chainIn.capture := currState === JtagState.CaptureIR.U
    mdl.io.chainIn.update := currState === JtagState.UpdateIR.U
    mdl.io.capture.bits := "b01".U
    mdl
  }

  withClockAndReset(clock_falling, io.JtagIO.jtag_reset) {
    val activeInstruction = RegInit("b00001".U(5.W))
    when (tapIsInTestLogicReset) {
      activeInstruction := "h01".U
    } .elsewhen (currState === JtagState.UpdateIR.U) {
      activeInstruction := irChain.io.update.bits
    }
    io.out.instruction := activeInstruction
  }

  tapIsInTestLogicReset := currState === JtagState.TestLogicReset.U
  io.out.tapIsInTestLogicReset := tapIsInTestLogicReset


  io.dataChainOut.shift   := currState === JtagState.ShiftDR.U
  io.dataChainOut.data    := io.JtagIO.TDI
  io.dataChainOut.capture := currState === JtagState.CaptureDR.U
  io.dataChainOut.update  := currState === JtagState.UpdateDR.U

  when (currState === JtagState.ShiftDR.U) {
    tdo := io.dataChainIn.data
    tdo_driven := true.B
  } .elsewhen (currState === JtagState.ShiftIR.U) {
    tdo := irChain.io.chainOut.data
    tdo_driven := true.B
  } .otherwise {
    tdo := DontCare
    tdo_driven := false.B
  }
}



object JtagTapGenerator {
  /** JTAG TAP generator, enclosed module must be clocked from TCK and reset from output of this
    * block.
    *
    * @param instructions map of instruction codes to data register chains that select that data
    * register; multiple instructions may map to the same data chain
    * @param idcode optional idcode instruction. idcode UInt will come from outside this core.
    *
    * @note all other instruction codes (not part of instructions or idcode) map to BYPASS
    * @note initial instruction is idcode (if supported), otherwise all ones BYPASS
    */
  def apply(instructions: Map[BigInt, Chain]): JtagBlockIO = {

    val internalIo = Wire(new JtagBlockIO(irLength = 5))

    val allInstructions = {
        require(!(instructions contains icode), "instructions may not contain IDCODE")

        val idcodeChain = Module(new CaptureChain(new JTAGIdcodeBundle()))
        idcodeChain.suggestName("idcodeChain")
        idcodeChain.io.capture.bits := internalIo.idcode

        instructions + (0x01 -> idcodeChain)
      }


    val bypassIcode = "h1f".U
    val initialInstruction = "h01".U



    val controllerInternal = Module(new JtagTapController())
    val bypassChain = Module(new JtagBypassChain())
    val unusedChainOut = Wire(new ShifterIO)
    unusedChainOut := 0.U.asTypeOf(new ShifterIO)




    bypassChain.io.chainIn := controllerInternal.io.dataChainOut  // for simplicity, doesn't visibly affect anything else


    // Need to ensure that this mapping is ordered to produce deterministic verilog,
    // and neither Map nor groupBy are deterministic.
    // Therefore, we first sort by IDCODE, then sort the groups by the first IDCODE in each group.
    val chainToIcode = (SortedMap(allInstructions.toList:_*).groupBy { case (icode, chain) => chain } map {
      case (chain, icodeToChain) => chain -> icodeToChain.keys
    }).toList.sortBy(_._2.head)

    val chainToSelect = chainToIcode map {
      case (chain, icodes) => {
        assume(icodes.size > 0)
        val icodeSelects = icodes map { controllerInternal.io.out.instruction === _.asUInt(5.W) }
        chain -> icodeSelects.reduceLeft(_||_)
      }
    }

    // controllerInternal.io.dataChainIn := bypassChain.io.chainOut  // default

    val emptyWhen = when (false.B) { }  // Empty WhenContext to start things off
    chainToSelect.toSeq.foldLeft(emptyWhen)(foldOutSelect).otherwise {
      controllerInternal.io.dataChainIn := bypassChain.io.chainOut
    }



    chainToSelect.map(mapInSelect)

    internalIo.JtagIO <> controllerInternal.io.JtagIO
    internalIo.out    <> controllerInternal.io.out



    def foldOutSelect(res: WhenContext, x: (Chain, Bool)): WhenContext = {
      val (chain, select) = x
      // Continue the WhenContext with if this chain is selected
      res.elsewhen(select) {
        controllerInternal.io.dataChainIn := chain.io.chainOut
      }
    }

    def mapInSelect(x: (Chain, Bool)): Unit = {
      val (chain, select) = x
      when (select) {
        chain.io.chainIn := controllerInternal.io.dataChainOut
      } .otherwise {
        chain.io.chainIn := unusedChainOut
      }
    }

    require(!(allInstructions contains bypassIcode), "instructions may not contain BYPASS code")
    require(allInstructions.size > 0, "Seriously? JTAG TAP with no instructions?")

    return internalIo
  }

}

