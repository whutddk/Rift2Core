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

object WNotifyVal {
  def apply(n: Int, rVal: UInt, wVal: UInt, wNotify: Bool, desc: RegFieldDesc): RegField = {
    RegField(n, rVal, RegWriteFn((valid, data) => {
      wNotify := valid; wVal := data
      true.B
    }
    ), desc)
  }
}


class DebugCtrlBundle(nComponents: Int) extends Bundle {
  val debugUnavail    = Input(Vec(nComponents, Bool()))
  val ndreset         = Output(Bool())
  val dmactive        = Output(Bool())
  val dmactiveAck     = Input(Bool())
}

class DebugExtTriggerOut (val nExtTriggers: Int) extends Bundle {
  val req = Output(UInt(nExtTriggers.W))
  val ack = Input(UInt(nExtTriggers.W))
}

class DebugExtTriggerIn (val nExtTriggers: Int) extends Bundle {
  val req = Input(UInt(nExtTriggers.W))
  val ack = Output(UInt(nExtTriggers.W))
}

class DebugExtTriggerIO(nExtTriggers: Int) extends Bundle {
  val out = new DebugExtTriggerOut(nExtTriggers)
  val in  = new DebugExtTriggerIn (nExtTriggers)
}

class DebugAuthenticationIO() extends Bundle {
  val dmactive    = Output(Bool())
  val dmAuthWrite = Output(Bool())
  val dmAuthRead  = Output(Bool())
  val dmAuthWdata = Output(UInt(32.W))
  val dmAuthBusy  = Input(Bool())
  val dmAuthRdata = Input(UInt(32.W))
  val dmAuthenticated = Input(Bool())
}

class DMSTATUSFields extends Bundle {
  // val reserved0 = UInt(9.W)
  // val impebreak = Bool()
  // val reserved1 = UInt(2.W)
  val allhavereset = Bool()
  val anyhavereset = Bool()
  val allresumeack = Bool()
  val anyresumeack = Bool()
  val allnonexistent = Bool()
  val anynonexistent = Bool()
  val allunavail = Bool()
  val anyunavail = Bool()
  val allrunning = Bool()
  val anyrunning = Bool()
  val allhalted = Bool()
  val anyhalted = Bool()
  // val authenticated = Bool()
  // val authbusy = Bool()
  // val hasresethaltreq = Bool()
  // val confstrptrvalid = Bool()
  // val version = UInt(4.W)
}


class DebugModule(device: Device, nComponents: Int = 1) extends Module{
  require( nComponents <= 10 )

  val io = IO(new Bundle{
    val ctrl = new DebugCtrlBundle(nComponents)
    val dmi = new DMIIO()

    val extTrigger = new DebugExtTriggerIO()
    val hartIsInReset = Input(Vec(nComponents, Bool()))
    val hartResetReq = Output(Vec(nComponents, Bool()))
    val auth = new DebugAuthenticationIO()
  })


  val dmiNode = TLRegisterNode(
  
    address = AddressSet.misaligned(0, "h40".U << 2): Seq[AddressSet],
    device = device,
    beatBytes = 4,
    executable = false
  )

  val peripNode = TLRegisterNode(
    address = AddressSet.misaligned(0, "hffff".U): Seq[AddressSet],
    device = device,
    beatBytes = 4,
    executable = true
  )

  val DMSTATUSRdData = WireInit(0.U.asTypeOf(new DMSTATUSFields()))
  val dmactive = RegInit(false.B)
  val ndmreset = RegInit(false.B)
  val hartsel = RegInit(0.U( (log2Ceil(nComponents) max 1).W))
  val havereset = RegInit(0.U(1.W))
  val resumereq = WireDefault(1.W))
  val resumeack = RegInit(0.U(1.W))
  val haltreq = RegInit(0.U(1.W))
  val is_halt = Wire( UInt(nComponents.W) )
  val busy = RegInit(false.B)
  val cmderr = RegInit(0.U(3.W))
  val sberror = RegInit(0.U(3.W))
  val sbreadondata = RegInit(0.U(1.W))
  val sbautoincrement = RegInit(0.U(1.W))
  val sbaccess = RegInit(2.U(3.W))
  val sbreadonaddr = RegInit(0.U(1.W))
  val sbbusy = Wire(UInt(1.W))
  val sbbusyerror = RegInit(0.U(1.W))


  val dataRegFields = for ( i <- 0 until 12 ) yield {
      Seq(
        RegField(32, ,  ,      RegFieldDesc("data("+i+")",         "Abstract Data "+i,         reset=Some(0)))
      )
  }


  val dmcontrolRegFields = RegFieldGroup("dmcontrol", Some("debug module control register"), Seq(
    RegField(1,  dmactive, RegFieldDesc("dmactive",         "dmactive",         reset=Some(0))),
    RegField(1,  ndmreset, RegFieldDesc("ndmreset",         "ndmreset",         reset=Some(0))),
    RegField(1, RegFieldDesc("clrresethaltreq", "clrresethaltreq(ignore)")),
    RegField(1, RegFieldDesc("setresethaltreq", "setresethaltreq(ignore)")),
    RegField(2),
    RegField.r(10, 0.U, RegFieldDesc("hartselhi",         "hartselhi(ignore)")),
    RegField(10, hartsel, hartsel, RegFieldDesc("hartsello",         "hartsello(ignore)")),
    RegField.r(1, 0.U, RegFieldDesc("hasel",         "hasel",         reset=Some(0))),
    RegField(1),
    RegField.w(1, RegWriteFn((valid, data) => { when(valid & data === 1.U) { havereset := havereset & ~(1.U << hartsel) }; true.B }), RegFieldDesc("ackhavereset", "0: No effect; 1: Clear havereset for any selected harts")),
    RegField.r(1,  0.U, RegFieldDesc("hartreset",         "hartreset(ignore)",         reset=Some(0))),
    RegField.w(1, RegWriteFn((valid, data) => { when(haltreq === 0.U & valid & data === 1.U ) { resumereq := (1.U << hartsel); resumeack := resumeack & ~(1.U << hartsel)}; true.B }), RegFieldDesc("resumereq", "Writing 1 causes the currently selected harts to resume once, if they are halted when the write occurs. It also clears the resume ack bit for those harts.resumereq is ignored if haltreq is set.")),
    RegField.w(1, haltreq, RegFieldDesc("haltreq",         "haltreq"))
  ))

  val dmstatusRegFields = RegFieldGroup("dmstatus", Some("debug module status register"), Seq(
    RegField.r(4, 2.U, RegFieldDesc("version",         "version",         reset=Some(2))),
    RegField.r(1, 0.U, RegFieldDesc("confstrptrvalid", "0: confstrptr0–confstrptr3 hold information which is not relevant to the configuration string", reset=Some(0))),
    RegField.r(1, 1.U, RegFieldDesc("hasresethaltreq", "Debug Module supports halt-on-reset functionality controllable by the setresethaltreq and clrresethaltreq bits", reset=Some(1))),
    RegField.r(1, 0.U, RegFieldDesc("authbusy",        "authbusy(ignore)",        reset=Some(0))),
    RegField.r(1, 1.U, RegFieldDesc("authenticated",   "authenticated(ignore)",   reset=Some(1))),
    RegField.r(1, DMSTATUSRdData.anyhalted,       RegFieldDesc("anyhalted",       "anyhalted",       reset=Some(0))),
    RegField.r(1, DMSTATUSRdData.allhalted,       RegFieldDesc("allhalted",       "allhalted",       reset=Some(0))),
    RegField.r(1, DMSTATUSRdData.anyrunning,      RegFieldDesc("anyrunning",      "anyrunning",      reset=Some(1))),
    RegField.r(1, DMSTATUSRdData.allrunning,      RegFieldDesc("allrunning",      "allrunning",      reset=Some(1))),
    RegField.r(1, DMSTATUSRdData.anyunavail,      RegFieldDesc("anyunavail",      "anyunavail",      reset=Some(0))),
    RegField.r(1, DMSTATUSRdData.allunavail,      RegFieldDesc("allunavail",      "allunavail",      reset=Some(0))),
    RegField.r(1, DMSTATUSRdData.anynonexistent,  RegFieldDesc("anynonexistent",  "anynonexistent",  reset=Some(0))),
    RegField.r(1, DMSTATUSRdData.allnonexistent,  RegFieldDesc("allnonexistent",  "allnonexistent",  reset=Some(0))),
    RegField.r(1, DMSTATUSRdData.anyresumeack,    RegFieldDesc("anyresumeack",    "anyresumeack",    reset=Some(1))),
    RegField.r(1, DMSTATUSRdData.allresumeack,    RegFieldDesc("allresumeack",    "allresumeack",    reset=Some(1))),
    RegField.r(1, DMSTATUSRdData.anyhavereset,    RegFieldDesc("anyhavereset",    "anyhavereset",    reset=Some(0))),
    RegField.r(1, DMSTATUSRdData.allhavereset,    RegFieldDesc("allhavereset",    "allhavereset",    reset=Some(0))),
    RegField(2),
    RegField.r(1, 0.U,       RegFieldDesc("impebreak",       "There is no implicit ebreak after Program Buffer",       reset=Some(0)))
  ))

  val hartInfoRegFields = RegFieldGroup("hartInfo", Some("Hart Info"), Seq(
    RegField.r(12, "h10".U, RegFieldDesc("dataaddr",         "dataaddr",         reset=Some(16))),
    RegField.r(4, 12.U, RegFieldDesc("datasize",         "datasize",         reset=Some(12))),
    RegField.r(1, 1.U, RegFieldDesc("dataaccess",         "dataaccess",         reset=Some(1))),
    RegField(3),
    RegField.r(4, 14.U, RegFieldDesc("nscratch",         "nscratch",         reset=Some(14))),
  ))

  val abstractcsRegFields = RegFieldGroup("abstractcs", Some("Abstract Control and Status"), Seq(
    RegField.r(4, 12.U, RegFieldDesc("datacount", "datacount", reset=Some(12))),
    RegField(4),
    RegField(3, cmderr, RegWriteFn((valid, data) => { when(valid & data === 1.U) { cmderr := 0.U }; true.B }), RegFieldDesc("cmderr", "cmderr", reset=Some(0))),
    RegField(1),
    RegField.r(1,  busy, RegFieldDesc("busy", "busy", reset=Some(0))),
    RegField(11),
    RegField.r(5, 16.U, RegFieldDesc("progbufsize", "progbufsize")),
  ))

  val commandExeFn: RegWriteFn
  val commandRegFields = RegFieldGroup("command", Some("Abstract Command"), Seq(
    RegField.w(32, commandExeFn, RegFieldDesc("cmdTpye + control", "cmdTpye + control")),
  ))

  val abstractautoRegFields = Seq( RegField(0) )

  val nextdmRegFields = Seq( RegField.r(32, 0.U,  RegFieldDesc("nextdm", "Next Debug Module", reset=Some(0))) )

  val progbufRegFields = for( i <- 0 until 16 ) yield {
    Seq( RegField(32, , , RegFieldDesc("progbuf("+i+")",         "Program Buffer",         reset=Some(0))) )
  }

  val sbcsRegFields = RegFieldGroup("sbcs", Some("System Bus Access Control and Status"), Seq(
    RegField.r(1, 0.U, RegFieldDesc("sbaccess8",   "sbaccess8" )),
    RegField.r(1, 0.U, RegFieldDesc("sbaccess16",  "sbaccess16")),
    RegField.r(1, 0.U, RegFieldDesc("sbaccess32",  "sbaccess32")),
    RegField.r(1, 1.U, RegFieldDesc("sbaccess64",  "sbaccess64")),
    RegField.r(1, 0.U, RegFieldDesc("sbaccess128", "sbaccess128")),
    RegField.r(7, 64.U, RegFieldDesc("sbasize",         "sbasize")),
    RegField(3, sberror, RegWriteFn((valid, data) => { when (valid & data === 1.U) { sberror := 0.U }; true.B }), RegFieldDesc("sberror", "sberror", reset=Some(0))),
    RegField(1, sbreadondata, RegFieldDesc("sbreadondata", "sbreadondata", reset=Some(0))),
    RegField(1, sbautoincrement, RegFieldDesc("sbautoincrement", "sbautoincrement", reset=Some(0))),
    RegField(3, sbaccess, RegFieldDesc("sbaccess", "sbaccess", reset=Some(2))),
    RegField(1, sbreadonaddr, RegFieldDesc("abreadonaddr", "abreadonaddr", reset=Some(0))),
    RegField.r(1, sbbusy, RegFieldDesc("sbbusy", "sbbusy", reset=Some(0))),
    RegField(1, sbbusyerror, RegWriteFn((valid, data) => { when (valid & data === 1.U) { sbbusyerror := 0.U }; true.B }), RegFieldDesc("sbbusyerror", "sbbusyerror", reset=Some(0))),
    RegField(6),
    RegField.r(3, 1.U, RegFieldDesc("sbversion", "sbversion")),
  ))

  val sbaddress = RegInit(VecInit(Seq.fill(2)(0.U(32.W))))
  val sbaddressRegFields = RegFieldGroup("sbaddress", Some("System Bus Address"), Seq(
    RegField(32, sbaddress(0), RegFieldDesc("sbaddress0", "sbaddress[31:0]", reset=Some(0))),
    RegField(32, sbaddress(1), RegFieldDesc("sbaddress1", "sbaddress[63:32]", reset=Some(0))),
   ))

  val sbdata = RegInit(VecInit(Seq.fill(2)(0.U(32.W))))
  val sbdataRegFields = RegFieldGroup("sbdata", Some("System Bus Data"), Seq(
    RegField(32, sbdata(0),                RegFieldDesc("sbdata0",         "sbdata[31:0]",         reset=Some(0))),
    RegField(32, sbdata(1),                RegFieldDesc("sbdata1",         "sbdata[63:32]",        reset=Some(0))),
    // RegField(32,  , ,                RegFieldDesc("sbdata2",         "sbdata[95:64]",        reset=Some(0))),
    // RegField(32,  , ,                RegFieldDesc("sbdata3",         "sbdata[127:96]",       reset=Some(0))),
   ))

  val haltsum0RegFields =
    Seq( RegField.r(32, is_halt, RegFieldDesc("haltsum", "halt summmary")) )


  dmiNode.remap(
    ("h04".U << 2) -> dataRegFields(0),
    ("h05".U << 2) -> dataRegFields(1),
    ("h06".U << 2) -> dataRegFields(2),
    ("h07".U << 2) -> dataRegFields(3),
    ("h08".U << 2) -> dataRegFields(4),
    ("h09".U << 2) -> dataRegFields(5),
    ("h0A".U << 2) -> dataRegFields(6),
    ("h0B".U << 2) -> dataRegFields(7),
    ("h0C".U << 2) -> dataRegFields(8),
    ("h0D".U << 2) -> dataRegFields(9),
    ("h0E".U << 2) -> dataRegFields(10),
    ("h0F".U << 2) -> dataRegFields(11),

    ("h10".U << 2) -> dmcontrolRegFields,
    ("h11".U << 2) -> dmstatusRegFields,
    ("h12".U << 2) -> hartInfoRegFields,

    ("h16".U << 2) -> abstractcsRegFields,
    ("h17".U << 2) -> commandRegFields,
    ("h18".U << 2) -> abstractautoRegFields,

    ("h1D".U << 2) -> nextdmRegFields,
    ("h20".U << 2) -> progbufRegFields(0),
    ("h21".U << 2) -> progbufRegFields(1),
    ("h22".U << 2) -> progbufRegFields(2),
    ("h23".U << 2) -> progbufRegFields(3),
    ("h24".U << 2) -> progbufRegFields(4),
    ("h25".U << 2) -> progbufRegFields(5),
    ("h26".U << 2) -> progbufRegFields(6),
    ("h27".U << 2) -> progbufRegFields(7),
    ("h28".U << 2) -> progbufRegFields(8),
    ("h29".U << 2) -> progbufRegFields(9),
    ("h2A".U << 2) -> progbufRegFields(10),
    ("h2B".U << 2) -> progbufRegFields(11),
    ("h2C".U << 2) -> progbufRegFields(12),
    ("h2D".U << 2) -> progbufRegFields(13),
    ("h2E".U << 2) -> progbufRegFields(14),
    ("h2F".U << 2) -> progbufRegFields(15),

    ("h38".U << 2) -> sbcsRegFields,
    ("h39".U << 2) -> sbaddressRegFields,
    ("h3C".U << 2) -> sbdataRegFields,

    ("h40".U << 2) -> haltsum0RegFields,
  )






  val instructionFields = for( i <- 0 until 16 ) yield {
    Seq( RegField(32, , , RegFieldDesc("instruction("+i+")",         "Program Buffer for instruction",         reset=Some(0))) )
  }

  val exchangeFields = for ( i <- 0 until 12 ) yield {
    Seq( RegField(32, ,  ,      RegFieldDesc("exchange("+i+")",         "Abstract Data for exchange"+i,         reset=Some(0))))
  }





  peripNode.remap(
    ("h00".U << 2) -> instructionFields(0),
    ("h01".U << 2) -> instructionFields(1),
    ("h02".U << 2) -> instructionFields(2),
    ("h03".U << 2) -> instructionFields(3),
    ("h04".U << 2) -> instructionFields(4),
    ("h05".U << 2) -> instructionFields(5),
    ("h06".U << 2) -> instructionFields(6),
    ("h07".U << 2) -> instructionFields(7),
    ("h08".U << 2) -> instructionFields(8),
    ("h09".U << 2) -> instructionFields(9),
    ("h0A".U << 2) -> instructionFields(10),
    ("h0B".U << 2) -> instructionFields(11),
    ("h0C".U << 2) -> instructionFields(12),
    ("h0D".U << 2) -> instructionFields(13),
    ("h0E".U << 2) -> instructionFields(14),
    ("h0F".U << 2) -> instructionFields(15),

    ("h10".U << 2) -> exchangeFields(0),
    ("h11".U << 2) -> exchangeFields(1),
    ("h12".U << 2) -> exchangeFields(2),
    ("h13".U << 2) -> exchangeFields(3),
    ("h14".U << 2) -> exchangeFields(4),
    ("h15".U << 2) -> exchangeFields(5),
    ("h16".U << 2) -> exchangeFields(6),
    ("h17".U << 2) -> exchangeFields(7),
    ("h18".U << 2) -> exchangeFields(8),
    ("h19".U << 2) -> exchangeFields(9),
    ("h1a".U << 2) -> exchangeFields(10),
    ("h1b".U << 2) -> exchangeFields(11),
  )

}
