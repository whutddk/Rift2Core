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
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

object WNotifyVal {
  def apply(n: Int, rVal: UInt, wVal: UInt, wNotify: Bool, desc: RegFieldDesc = RegFieldDesc("N/A", "N/A") ): RegField = {
    RegField(n, rVal, RegWriteFn((valid, data) => {
      wNotify := valid
      wVal := data
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

object DebugRomNonzeroContents {

  def apply() : Array[Byte] = { Array (
  0x73, 0x90, 0x34, 0x7b, 0x6f, 0x00, 0xc0, 0x00, 0xf3, 0x94, 0x34, 0x7b,
  0x23, 0xa6, 0x04, 0x90, 0x0f, 0x00, 0xf0, 0x0f, 0x73, 0x10, 0x24, 0x7b,
  0x97, 0x04, 0x00, 0x00, 0x93, 0x84, 0x84, 0xfe, 0x73, 0x24, 0x40, 0xf1,
  0x23, 0xa0, 0x84, 0x90, 0x33, 0x04, 0x94, 0x00, 0x03, 0x44, 0x04, 0xc0,
  0x13, 0x74, 0x34, 0x00, 0xe3, 0x06, 0x04, 0xfe, 0x13, 0x74, 0x14, 0x00,
  0x63, 0x0c, 0x04, 0x00, 0x73, 0x24, 0x20, 0x7b, 0x67, 0x80, 0x84, 0x04,
  0x23, 0xa2, 0x04, 0x90, 0xf3, 0x94, 0x34, 0x7b, 0x6f, 0xf0, 0x1f, 0xab,
  0x73, 0x24, 0x40, 0xf1, 0x23, 0xa4, 0x84, 0x90, 0x73, 0x24, 0x20, 0x7b,
  0xf3, 0x24, 0x30, 0x7b, 0x73, 0x00, 0x20, 0x7b
  ).map(_.toByte) }

}



class DebugModule(device: Device, nComponents: Int = 1) extends Module{
  require( nComponents <= 10 )

  val io = IO(new Bundle{
    val ctrl = new DebugCtrlBundle(nComponents)
    val dmi = Flipped(new DMIIO())

    val extTrigger = new DebugExtTriggerIO()
    val hartIsInReset = Input(Vec(nComponents, Bool()))
    val hartResetReq = Output(Vec(nComponents, Bool()))
    val auth = new DebugAuthenticationIO()
  })


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
  val resumereq = WireDefault(0.U(1.W))
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
  val sbaddress = RegInit(VecInit(Seq.fill(2)(0.U(32.W))))
  val sbdata = RegInit(VecInit(Seq.fill(2)(0.U(32.W))))

  val programBufferMem_qout = RegInit(VecInit(Seq.fill(16)(0.U(32.W))))
  val programBufferMem_dnxt1 = Wire(Vec(16, UInt(32.W)))
  val programBufferMem_dnxt2 = Wire(Vec(16, UInt(32.W)))
  val programBufferMem_en1   = Wire(Vec(16, Bool() ))
  val programBufferMem_en2   = Wire(Vec(16, Bool() ))

  for ( i <- 0 until 16 ) yield {
    when( programBufferMem_en1(i) ){ programBufferMem_qout(i) := programBufferMem_dnxt1(i) }
    .elsewhen( programBufferMem_en2(i) ) { programBufferMem_qout(i) := programBufferMem_dnxt2(i) }
  }


  val abstractDataMem_qout = RegInit(VecInit(Seq.fill(12)(0.U(32.W))))
  val abstractDataMem_dnxt1 = Wire(VecInit(Seq.fill(12)(0.U(32.W))))
  val abstractDataMem_dnxt2 = Wire(VecInit(Seq.fill(12)(0.U(32.W))))
  val abstractDataMem_en1   = Wire(Vec(16, Bool() ))
  val abstractDataMem_en2   = Wire(Vec(16, Bool() ))

  for ( i <- 0 until 12 ) yield {
    when( abstractDataMem_en1(i) ) { abstractDataMem_qout(i) := abstractDataMem_dnxt1(i) }
    .elsewhen( abstractDataMem_en2(i) ) { abstractDataMem_qout(i) := abstractDataMem_dnxt2(i) }
  }



  val hartHaltedId   = Wire(UInt(32.W))
  val hartHaltedWrEn = Wire(Bool())

  val hartGoingId   = Wire(UInt(32.W))
  val hartGoingWrEn = Wire(Bool())

  val hartResumingId = Wire(UInt(32.W))  
  val hartResumingWrEn = Wire(Bool())

  val hartExceptionId = Wire(UInt(32.W))
  val hartExceptionWrEn = Wire(Bool())





  val commandVal = Wire(UInt(32.W))
  val commandEn  = Wire(Bool())

  val whereTo = RegInit(0.U(32.W))
  val flags   = RegInit(0.U.asTypeOf(new Bundle{
      val is_resume = Bool()
      val is_going  = Bool()
  }))

  //abstract command
  when(commandEn) {
    val cmdTpye = commandVal(31,24)
    val control = commandVal(23,0)

    val is_access_register = (cmdTpye === 0.U)
    val is_quick_access    = (cmdTpye === 1.U)
    val is_access_memory   = (cmdTpye === 2.U)

    when( is_access_register ) {
      val aarsize = control(22,20)
      val aarpostincrement = control(19).asBool
      val postexec = control(18).asBool
      val transfer = control(17).asBool
      val write = control(16).asBool
      val regno = control(15,0)

      whereTo := 
      flags.is_resume := false.B
      flags.is_going  := true.B
    } 

    when( is_quick_access ) {
      whereTo := 
      flags.is_resume := false.B
      flags.is_going  := true.B
    } 

    when( is_access_memory ) {
      val aamvirtual = control(23).asBool
      val aamsize    = control(22,20)
      val aampostincrement = control(19).asBool
      val write = control(16).asBool
      val target_specific = control(15,14)

      whereTo := 
      flags.is_resume := false.B
      flags.is_going  := true.B
    }

  } 















































  val dmi_req = Wire(Decoupled(new RegMapperInput( params = RegMapperParams(indexBits = 8, maskBits = 4)) ))
  dmi_req.valid := io.dmi.req.valid
  io.dmi.req.ready := dmi_req.ready

  dmi_req.bits.read  := io.dmi.req.bits.op === 1.U
  dmi_req.bits.index := io.dmi.req.bits.addr
  dmi_req.bits.data  := io.dmi.req.bits.data
  dmi_req.bits.mask  := Mux(io.dmi.req.bits.op === 2.U, ~(0.U(8.W)), 0.U)

  val dmi_resp = RegMapper(bytes = 4, concurrency = 0, undefZero = true, in = dmi_req,
    mapping = 
      (0x04 << 2) -> RegFieldGroup("data", Some("Data used to communicate with Debug Module"), (0 to 11).map{ i => WNotifyVal(32, abstractDataMem_qout(i), abstractDataMem_dnxt1(i), abstractDataMem_en1(i))}),

      (0x10 << 2) -> RegFieldGroup("dmcontrol", Some("debug module control register"), Seq(
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
      )),

      (0x11 << 2) -> RegFieldGroup("dmstatus", Some("debug module status register"), Seq(
        RegField.r(4, 2.U,                            RegFieldDesc("version",         "version",                 reset=Some(2))),
        RegField.r(1, 0.U,                            RegFieldDesc("confstrptrvalid", "0: confstrptr0–confstrptr3 hold information which is not relevant to the configuration string", reset=Some(0))),
        RegField.r(1, 1.U,                            RegFieldDesc("hasresethaltreq", "Debug Module supports halt-on-reset functionality controllable by the setresethaltreq and clrresethaltreq bits", reset=Some(1))),
        RegField.r(1, 0.U,                            RegFieldDesc("authbusy",        "authbusy(ignore)",        reset=Some(0))),
        RegField.r(1, 1.U,                            RegFieldDesc("authenticated",   "authenticated(ignore)",   reset=Some(1))),
        RegField.r(1, DMSTATUSRdData.anyhalted,       RegFieldDesc("anyhalted",       "anyhalted",               reset=Some(0))),
        RegField.r(1, DMSTATUSRdData.allhalted,       RegFieldDesc("allhalted",       "allhalted",               reset=Some(0))),
        RegField.r(1, DMSTATUSRdData.anyrunning,      RegFieldDesc("anyrunning",      "anyrunning",              reset=Some(1))),
        RegField.r(1, DMSTATUSRdData.allrunning,      RegFieldDesc("allrunning",      "allrunning",              reset=Some(1))),
        RegField.r(1, DMSTATUSRdData.anyunavail,      RegFieldDesc("anyunavail",      "anyunavail",              reset=Some(0))),
        RegField.r(1, DMSTATUSRdData.allunavail,      RegFieldDesc("allunavail",      "allunavail",              reset=Some(0))),
        RegField.r(1, DMSTATUSRdData.anynonexistent,  RegFieldDesc("anynonexistent",  "anynonexistent",          reset=Some(0))),
        RegField.r(1, DMSTATUSRdData.allnonexistent,  RegFieldDesc("allnonexistent",  "allnonexistent",          reset=Some(0))),
        RegField.r(1, DMSTATUSRdData.anyresumeack,    RegFieldDesc("anyresumeack",    "anyresumeack",            reset=Some(1))),
        RegField.r(1, DMSTATUSRdData.allresumeack,    RegFieldDesc("allresumeack",    "allresumeack",            reset=Some(1))),
        RegField.r(1, DMSTATUSRdData.anyhavereset,    RegFieldDesc("anyhavereset",    "anyhavereset",            reset=Some(0))),
        RegField.r(1, DMSTATUSRdData.allhavereset,    RegFieldDesc("allhavereset",    "allhavereset",            reset=Some(0))),
        RegField(2),
        RegField.r(1, 0.U,       RegFieldDesc("impebreak",       "There is no implicit ebreak after Program Buffer",       reset=Some(0)))
      )),

      (0x12 << 2) -> RegFieldGroup("hartInfo", Some("Hart Info"), Seq(
        RegField.r(12, "h10".U, RegFieldDesc("dataaddr",         "dataaddr",         reset=Some(16))),
        RegField.r(4,  12.U,    RegFieldDesc("datasize",         "datasize",         reset=Some(12))),
        RegField.r(1,  1.U,     RegFieldDesc("dataaccess",       "dataaccess",       reset=Some(1))),
        RegField(3),
        RegField.r(4,  14.U,    RegFieldDesc("nscratch",         "nscratch",         reset=Some(14))),
      )),

      (0x16 << 2) -> RegFieldGroup("abstractcs", Some("Abstract Control and Status"), Seq(
        RegField.r(4, 12.U, RegFieldDesc("datacount", "datacount", reset=Some(12))),
        RegField(4),
        RegField(3, cmderr, RegWriteFn((valid, data) => { when(valid & data === 1.U) { cmderr := 0.U }; true.B }), RegFieldDesc("cmderr", "cmderr", reset=Some(0))),
        RegField(1),
        RegField.r(1,  busy, RegFieldDesc("busy", "busy", reset=Some(0))),
        RegField(11),
        RegField.r(5, 16.U, RegFieldDesc("progbufsize", "progbufsize")),
      )),

      (0x17 << 2) -> RegFieldGroup("command", Some("Abstract Command"), Seq(
        WNotifyVal(32, 0.U, wVal = commandVal, wNotify = commandEn, RegFieldDesc("cmdTpye + control", "cmdTpye + control")),
      )),

      (0x18 << 2) -> RegFieldGroup("abstractauto", Some("Abstract Command Auto execute"), Seq(
        RegField(0)
      )),

      (0x1D << 2) -> RegFieldGroup("nextdm", Some("Next Debug Module"), Seq(
        RegField.r(32, 0.U)
      )),

      (0x20 << 2) -> RegFieldGroup("progbuf", Some("Program buffer used to communicate with Debug Module"), (0 to 15).map{ i => WNotifyVal(32, programBufferMem_qout(i), programBufferMem_dnxt1(i), programBufferMem_en1(i))}),

      (0x38 << 2) -> RegFieldGroup("sbcs", Some("System Bus Access Control and Status"), Seq(
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
      )),

      (0x39 << 2) -> RegFieldGroup("sbaddress", Some("System Bus Address"), Seq(
        RegField(32, sbaddress(0), RegFieldDesc("sbaddress0", "sbaddress[31:0]", reset=Some(0))),
        RegField(32, sbaddress(1), RegFieldDesc("sbaddress1", "sbaddress[63:32]", reset=Some(0))),
      )),

      (0x3C << 2) -> RegFieldGroup("sbdata", Some("System Bus Data"), Seq(
        RegField(32, sbdata(0),                RegFieldDesc("sbdata0",         "sbdata[31:0]",         reset=Some(0))),
        RegField(32, sbdata(1),                RegFieldDesc("sbdata1",         "sbdata[63:32]",        reset=Some(0))),
        // RegField(32,  , ,                RegFieldDesc("sbdata2",         "sbdata[95:64]",        reset=Some(0))),
        // RegField(32,  , ,                RegFieldDesc("sbdata3",         "sbdata[127:96]",       reset=Some(0))),
      )),

      (0x40 << 2) -> RegFieldGroup("haltsum0", Some("Halt Summary"), Seq( 
        RegField.r(32, is_halt, RegFieldDesc("haltsum", "halt summmary"))
      ))
  )
  
  io.dmi.resp.valid := dmi_resp.valid
  dmi_resp.ready := io.dmi.resp.ready

  io.dmi.resp.bits.addr := DontCare
  io.dmi.resp.bits.data := dmi_resp.bits.data
  io.dmi.resp.bits.op   := DontCare




  peripNode.regmap(
    ("h000".U ) -> RegFieldGroup("debug_rom", Some("Debug ROM"), DebugRomContents().zipWithIndex.map{case (x, i) => RegField.r(8, (x & 0xFF).U(8.W))}),
    ("h100".U ) -> Seq(RegField.r(32, whereTo, RegFieldDesc("debug_whereto", "Instruction filled in by Debug Module to control hart in Debug Mode", volatile = true))),
    ("h200".U ) -> RegFieldGroup("debug_progbuf", Some("Program buffer used to communicate with Debug Module"), (0 to 15).map{ i => WNotifyVal(32, programBufferMem_qout(i), programBufferMem_dnxt2(i), programBufferMem_en2(i))}),
    ("h300".U ) -> RegFieldGroup("debug_abstract", Some("Instructions generated by Debug Module"), abstractGeneratedMem.zipWithIndex.map{ case (x,i) => RegField.r(32, x)}),

    ("h400".U ) -> Seq(WNotifyVal(32, 0.U, hartHaltedId, hartHaltedWrEn, "debug_hart_halted", "Debug ROM Causes hart to write its hartID here when it is in Debug Mode.")), //HALTED
    ("h404".U ) -> Seq(WNotifyVal(32, 0.U, hartGoingId,  hartGoingWrEn, "debug_hart_going", "Debug ROM causes hart to write 0 here when it begins executing Debug Mode instructions.")) //GOING
    ("h408".U ) -> Seq(WNotifyVal(32, 0.U, hartResumingId,  hartResumingWrEn, "debug_hart_resuming", "Debug ROM causes hart to write its hartID here when it leaves Debug Mode.")), //RESUMING
    ("h40C".U ) -> Seq(WNotifyVal(32, 0.U, hartExceptionId,  hartExceptionWrEn, "debug_hart_exception", "Debug ROM causes hart to write 0 here if it gets an exception in Debug Mode.")), //EXCEPTION
    ("h500".U ) -> RegFieldGroup("debug_data", Some("Data used to communicate with Debug Module"), (0 to 11),map{ i => WNotifyVal(32, abstractDataMem_qout(i), abstractDataMem_dnxt2(i), abstractDataMem_en2(i))}),
    ("h600".U ) -> RegFieldGroup("debug_flags", Some("Memory region used to control hart going/resuming in Debug Mode"), flags.zipWithIndex.map{case(x, i) => RegField.r(8, x.asUInt())}),
  )



// WNotifyVal {
//   def apply(n: Int, rVal: UInt, wVal: UInt, wNotify: Bool, desc: RegFieldDesc)

// }
}
