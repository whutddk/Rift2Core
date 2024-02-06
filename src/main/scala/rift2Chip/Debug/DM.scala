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

package debug

import chisel3._
import chisel3.util._

import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

object WNotifyVal {
  def apply(n: Int, rVal: UInt, wVal: UInt, wNotify: Bool, desc: RegFieldDesc = RegFieldDesc("NA", "NA") ): RegField = {
    RegField(n, rVal, RegWriteFn((valid, data) => {
      wNotify := valid
      wVal := data
      true.B
    }
    ), desc)
  }
}


// class DebugCtrlBundle(nComponents: Int) extends Bundle {
//   val debugUnavail    = Input(Vec(nComponents, Bool()))
//   val ndreset         = Output(Bool())
//   val dmactive        = Output(Bool())
//   val dmactiveAck     = Input(Bool())
// }

// class DebugExtTriggerOut (val nExtTriggers: Int) extends Bundle {
//   val req = Output(UInt(nExtTriggers.W))
//   val ack = Input(UInt(nExtTriggers.W))
// }

// class DebugExtTriggerIn (val nExtTriggers: Int) extends Bundle {
//   val req = Input(UInt(nExtTriggers.W))
//   val ack = Output(UInt(nExtTriggers.W))
// }

// class DebugExtTriggerIO(nExtTriggers: Int) extends Bundle {
//   val out = new DebugExtTriggerOut(nExtTriggers)
//   val in  = new DebugExtTriggerIn (nExtTriggers)
// }

// class DebugAuthenticationIO() extends Bundle {
//   val dmactive    = Output(Bool())
//   val dmAuthWrite = Output(Bool())
//   val dmAuthRead  = Output(Bool())
//   val dmAuthWdata = Output(UInt(32.W))
//   val dmAuthBusy  = Input(Bool())
//   val dmAuthRdata = Input(UInt(32.W))
//   val dmAuthenticated = Input(Bool())
// }

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










class Info_DM_cmm() extends Bundle{
  val hartIsInReset = Input(Bool())
  val hartResetReq = Output(Bool())
  val hartHaltReq = Output(Bool())
}

class DEBUG_ROM_FLAGS extends Bundle{
  val isResume = Bool()
  val isGoing  = Bool()
}






class DebugModule(device: Device, nComponents: Int = 1)(implicit p: Parameters) extends LazyModule{
  require( nComponents <= 10 )

  val peripNode = TLRegisterNode(
    address = Seq(AddressSet(0x00000000L, 0x0000ffffL)),
    device = device,
    concurrency = 1,
    beatBytes = 8,
    executable = true
  )


  // val sbaClientNode = {
  //   val sbaClientParameters = TLMasterPortParameters.v1( Seq(TLMasterParameters.v1( name = "sba", sourceId = IdRange(0, 1)  )))
  //   TLClientNode(Seq(sbaClientParameters))
  // }
  
  abstract class DMBase extends LazyModuleImp(this) {

    class DMIO extends Bundle{
      val dmi = Flipped(new DMIIO())

      val ndreset         = Output(Bool())
      // val dmactive        = Output(Bool())
      // val dmactiveAck     = Input(Bool())

      // val extTrigger = new DebugExtTriggerIO()
      val dm_cmm = Vec( nComponents, new Info_DM_cmm )
      // val debugResetReq  = Input(Vec(nComponents, Bool()))

      // val sba_getPut    = new DecoupledIO(new TLBundleA(edgeOut.bundle))
      // val sba_access = Flipped(new DecoupledIO(new TLBundleD(edgeOut.bundle)))      
    }

    val io: DMIO = IO(new DMIO)


    def HALTED       : Int = return 0x100
    def GOING        : Int = return 0x104
    def RESUMING     : Int = return 0x108
    def EXCEPTION    : Int = return 0x10C
    def WHERETO      : Int = return 0x300
    def DATA         : Int = return 0x380
    def PROGBUF      : Int = return DATA - (16 * 4) // if (cfg.hasImplicitEbreak) (tmp - 4) else tmp
      // 340 344 348 34c
      // 350 354 358 35c
      // 360 364 368 36c
      // 370 374 378 37c
    // def IMPEBREAK : Int = return { DATA - 4 }
    def ABSTRACT     : Int = return PROGBUF - (8 * 4)
      // 320 324 328 32c
      // 330 334 338 33c
    def FLAGS        : Int = return 0x400
    def ROMBASE      : Int = return 0x800


    val dmstatus = WireInit(0.U.asTypeOf(new DMSTATUSFields()))
      val dmactive = RegInit(false.B)
      val ndmreset = RegInit(false.B); io.ndreset := ndmreset
    val hartsel = WireDefault(0.U(1.W))//RegInit(0.U( (log2Ceil(nComponents) max 1).W))
    val havereset = RegInit(VecInit(Seq.fill(nComponents)(false.B)))
    val hartreset = RegInit(VecInit(Seq.fill(nComponents)(false.B)))
    // val resumereq = RegInit(VecInit(Seq.fill(nComponents)(false.B)))
    val resumeack = RegInit(VecInit(Seq.fill(nComponents)(false.B)))
    val haltreq = RegInit(VecInit(Seq.fill(nComponents)(false.B)))

    val resethaltreq = RegInit(VecInit(Seq.fill(nComponents)(false.B)))
    val is_halted = RegInit(VecInit(Seq.fill(nComponents)(false.B)))
    val busy = RegInit(false.B)
    val cmderr = RegInit(0.U(3.W))
    when( cmderr =/= 0.U ) { assert( RegNext(cmderr) === cmderr | RegNext(cmderr) === 0.U ) }







    val hartHaltedId   = Wire(UInt(32.W))
    val hartHaltedWrEn = Wire(Bool())

    val hartGoingId   = Wire(UInt(32.W))
    val hartGoingWrEn = Wire(Bool())

    val hartResumingId = Wire(UInt(32.W))  
    val hartResumingWrEn = Wire(Bool())

    val hartExceptionId = Wire(UInt(32.W))
    val hartExceptionWrEn = Wire(Bool())

    val abstract_hartId = RegInit(0.U( (log2Ceil(nComponents) max 1).W))

    // dontTouch(hartHaltedWrEn)
    // dontTouch(hartGoingWrEn)
    // dontTouch(hartResumingWrEn)

    val flags   = RegInit( VecInit(Seq.fill(nComponents)(0.U.asTypeOf(new DEBUG_ROM_FLAGS))))

    val setresethaltEn = Wire(Bool())
    val clrresethaltEn = Wire(Bool())

    val ackhavereset_W1 = Wire(Bool())

    val resumeReq_W1 = Wire(Bool())
  }

  /**
    * DMStatus control the halt and reset
    */
  trait DMStatus { this: DMBase =>
    when( clrresethaltEn ) { resethaltreq(hartsel) := false.B }
    .elsewhen( setresethaltEn ) { resethaltreq(hartsel) := true.B }


    for ( i <- 0 until nComponents) yield {
      when( ~dmactive ) {
        havereset(i) := false.B
      } .elsewhen( io.dm_cmm(i).hartIsInReset ) {
        havereset(i) := true.B       
      } .elsewhen( ackhavereset_W1 ) {
        havereset(hartsel) := false.B
      }
    }


    for ( i <- 0 until nComponents) yield {
      io.dm_cmm(i).hartResetReq := hartreset(i)
      io.dm_cmm(i).hartHaltReq  := haltreq(i) | resethaltreq(i)
    }


    
    for ( i <- 0 until nComponents ) yield {
      when( resumeReq_W1 & (i.U === hartsel) ){
        flags(i).isResume := true.B
      } .elsewhen( hartResumingWrEn & (i.U === hartResumingId) ) {
        flags(i).isResume := false.B
      }

      when( io.dm_cmm(i).hartIsInReset ) {
        is_halted(i) := false.B
        resumeack(i) := false.B
      } .elsewhen( resumeReq_W1 & (i.U === hartsel)) {
        resumeack(i) := false.B
      } .elsewhen( hartResumingWrEn & (i.U === hartResumingId)) {
        is_halted(i) := false.B
        resumeack(i) := true.B
      } .elsewhen( hartHaltedWrEn & (i.U === hartHaltedId)) {
        is_halted(i) := true.B
      }
        
    }



    dmstatus.anyhavereset := havereset(hartsel) === true.B
    dmstatus.allhavereset := havereset(hartsel) === true.B
    dmstatus.anyresumeack := resumeack(hartsel) === true.B
    dmstatus.allresumeack := resumeack(hartsel) === true.B
    dmstatus.anynonexistent := hartsel >= nComponents.U
    dmstatus.allnonexistent := hartsel >= nComponents.U
    dmstatus.anyunavail := io.dm_cmm(hartsel).hartIsInReset === true.B
    dmstatus.allunavail := io.dm_cmm(hartsel).hartIsInReset === true.B
    dmstatus.anyrunning := ~io.dm_cmm(hartsel).hartIsInReset & ~is_halted(hartsel)
    dmstatus.allrunning := ~io.dm_cmm(hartsel).hartIsInReset & ~is_halted(hartsel)
    dmstatus.anyhalted := is_halted(hartsel) === true.B
    dmstatus.allhalted := is_halted(hartsel) === true.B

    
    when(hartGoingWrEn) {
      flags(hartGoingId).isGoing := false.B
    } .elsewhen( hartHaltedWrEn & busy & abstract_hartId === hartHaltedId ){
      busy := false.B
    } .elsewhen( hartExceptionWrEn & busy & abstract_hartId === hartExceptionId ){
      busy := false.B
      cmderr := 3.U
    }
  }

  /**
    * Program Buffer instance, we have 16 program buffer, it can either be modified by  openocd through dtm-bus or be read by core through icache 
    */
  // trait DMPBuff{ this: DMBase =>

  // }

  /**
    * Abstract Command instance, 
    */
  trait DMAbstract{ this: DMBase =>
    val abstractDataMem_qout = RegInit(VecInit(Seq.fill(12)(0.U(32.W))))
    val abstractDataMem_dnxt1 = Wire(Vec(12,UInt(32.W)))
    val abstractDataMem_dnxt2 = Wire(Vec(12,UInt(32.W)))
    val abstractDataMem_wen1   = Wire(Vec(12, Bool() ))
    val abstractDataMem_wen2   = Wire(Vec(12, Bool() ))
    val abstractDataMem_ren1   = Wire(Vec(12, Bool() ))
    dontTouch(abstractDataMem_wen1)
    dontTouch(abstractDataMem_ren1)
    
    // val abstractDataMem_en2   = Wire(Vec(12, Bool() ))

    val programBufferMem_qout = RegInit(VecInit(Seq.fill(16)(0.U(32.W))))
    val programBufferMem_dnxt1 = Wire(Vec(16, UInt(32.W)))
    val programBufferMem_dnxt2 = Wire(Vec(16, UInt(32.W)))
    val programBufferMem_wen1   = Wire(Vec(16, Bool() ))
    val programBufferMem_wen2   = Wire(Vec(16, Bool() ))
    val programBufferMem_ren1   = Wire(Vec(16, Bool() ))
    dontTouch(programBufferMem_wen1)
    dontTouch(programBufferMem_ren1)

    val commandVal = Wire(UInt(32.W))
    val commandEn  = Wire(Bool())
    val command = RegEnable(commandVal, 0.U, commandEn)


    val cmdTpye = command(31,24)
    val control = command(23,0)

    val is_access_register = (cmdTpye === 0.U)
    val is_quick_access    = (cmdTpye === 1.U)
    val is_access_memory   = (cmdTpye === 2.U)

    val aarsize = control(22,20)
    val aarpostincrement = control(19).asBool
    val postexec = control(18).asBool
    val transfer = control(17).asBool
    val write = control(16).asBool
    val regno = control(15,0)

    val is_access_CSR = is_access_register & (regno(15,12) === 0.U )
    val is_access_GPR = is_access_register & (regno(15, 5) === "b00010000000".U )
    val is_access_FPR = is_access_register & (regno(15, 5) === "b00010000001".U )

    val aamvirtual = control(23).asBool
    val aamsize    = control(22,20)
    val aampostincrement = control(19).asBool
    val target_specific = control(15,14)
    
    val is_misalign = Mux1H(Seq(
      (aamsize === 0.U) -> false.B,                                 (aamsize === 1.U) -> (abstractDataMem_qout(2)(0) =/= 0.U ),
      (aamsize === 2.U) -> (abstractDataMem_qout(2)(1,0) =/= 0.U ), (aamsize === 3.U) -> (abstractDataMem_qout(2)(2,0) =/= 0.U ),
    ))



    
    // val programBufferMem_wen2   = Wire(Vec(16, Bool() ))

    for ( i <- 0 until 16 ) yield {
      when( programBufferMem_wen1(i) ){ programBufferMem_qout(i) := programBufferMem_dnxt1(i) }
      .elsewhen( programBufferMem_wen2(i) ) { programBufferMem_qout(i) := programBufferMem_dnxt2(i) }
    }

    val autoexecprogbuf = RegInit(0.U(16.W))
    val autoexecdata    = RegInit(0.U(12.W))

    val isProgramBufferMemAccessAuto = ( 0 until 16 ).map{ i => (programBufferMem_wen1(i) | programBufferMem_ren1(i)) & autoexecprogbuf(i)  }.reduce(_|_)
    val isAbstractDataMemAccessAuto  = ( 0 until 12 ).map{ i => (abstractDataMem_wen1(i)  | abstractDataMem_ren1(i))  & autoexecdata(i)     }.reduce(_|_)
    
    for ( i <- 0 until 12 ) yield {
      when( abstractDataMem_wen1(i) ) { abstractDataMem_qout(i) := abstractDataMem_dnxt1(i) }
      .elsewhen( abstractDataMem_wen2(i) ) { abstractDataMem_qout(i) := abstractDataMem_dnxt2(i) }
    }

    // when( hartHaltedWrEn & busy & (abstract_hartId === hartHaltedId) & is_access_memory & aampostincrement){
    //   abstractDataMem_qout(3) := (Cat(abstractDataMem_qout(3), abstractDataMem_qout(2)) + 1.U << control(22,20))(63,32)
    //   abstractDataMem_qout(2) := abstractDataMem_qout(2) + 1.U << control(22,20)
    // }

    

    val whereTo = RegInit("b000000000000100000000000001110011".U(32.W))  //ebreak
    val abstractGeneratedMem = WireDefault(VecInit(
      "b0010011".U(32.W), //addi 0, zero, 0 (nop)
      "b0010011".U(32.W),
      "b0010011".U(32.W),
      "b0010011".U(32.W),
      "b0010011".U(32.W),
      "b0010011".U(32.W),
      "b0010011".U(32.W),
      "b000000000000100000000000001110011".U  //ebreak
    ))
    

    when( is_access_register ) {

      whereTo := Cat(((ABSTRACT-WHERETO).U).extract(20), ((ABSTRACT-WHERETO).U)(10,1), ((ABSTRACT-WHERETO).U).extract(11), ((ABSTRACT-WHERETO).U)(19,12), "b000001101111".U(12.W) ) //to ABSTRACT(WHERETO+hxxx) jal hxxx (debug_abstarct)

      when( is_access_CSR ) {
        val reg_sel = regno(11,0)

        // csrw s0 dscratch1,
        abstractGeneratedMem(0) := Cat("h7b3".U(12.W), 8.U(5.W), "b001".U(3.W), 0.U(5.W), "b1110011".U(7.W))
        // csrr s0 regno
        abstractGeneratedMem(1) := Cat(       reg_sel, 0.U(5.W), "b010".U(3.W), 8.U(5.W), "b1110011".U(7.W))
        
        abstractGeneratedMem(2) := Cat(
          ((DATA).U)(11,5), // offset (DATA)
          Mux( transfer & write, ((DATA).U)(4,0), 8.U(5.W)),          // offset h500 / rs2 = s0
          0.U(5.W),         // rs1 = 0 base
          Mux( aarsize === 2.U, "b010".U(3.W), "b011".U(3.W)),       // word / double-word
          Mux( transfer & write, 8.U(5.W), ((DATA).U)(4,0)),       // rd = s0 / offset h500
          Mux( transfer & write, "b0000011".U(7.W),  "b0100011".U ), // ld or st
        )
        // csrw s0 regno / nop
        abstractGeneratedMem(3) := Mux(
          transfer & write,
          Cat(     reg_sel, 8.U(5.W), "b001".U(3.W), 0.U(5.W), "b1110011".U(7.W)),
          "b0010011".U(32.W)
        )
        // csrr s0 dscratch1   
        abstractGeneratedMem(4) := Cat("h7b3".U(12.W), 0.U(5.W), "b010".U(3.W), 8.U(5.W), "b1110011".U(7.W))
        abstractGeneratedMem(5) := Mux( postexec, 
        Cat(((PROGBUF-ABSTRACT-5*4).U).extract(20), ((PROGBUF-ABSTRACT-5*4).U)(10,1), ((PROGBUF-ABSTRACT-5*4).U).extract(11), ((PROGBUF-ABSTRACT-5*4).U)(19,12), "b000001101111".U(12.W) ), //to PROGBUF (ABSTRACT+5*4+hxx) jal hxx
        "b0010011".U(32.W) ) //nop
        abstractGeneratedMem(6) := "b0010011".U(32.W)  //nop
        abstractGeneratedMem(7) := "b000000000000100000000000001110011".U

      } .elsewhen( is_access_GPR ) {
        val reg_sel = regno(4,0)

        abstractGeneratedMem(0) := "b0010011".U(32.W) //addi 0, zero, 0 (nop)
        abstractGeneratedMem(1) := "b0010011".U(32.W)

        abstractGeneratedMem(2) := Cat(
          ((DATA).U)(11,5), // offset DATA
          Mux( transfer & write, ((DATA).U)(4,0), reg_sel),          // offset h500 / rs2 = s0
          0.U(5.W),         // rs1 = 0 base
          Mux( aarsize === 2.U, "b010".U(3.W), "b011".U(3.W)),       // word / double-word
          Mux( transfer & write, reg_sel, ((DATA).U)(4,0)),       // rd = s0 / offset h500
          Mux( transfer & write, "b0000011".U(7.W),  "b0100011".U(7.W) ), // ld or st
        )
        abstractGeneratedMem(3) := "b0010011".U(32.W)
        abstractGeneratedMem(4) := "b0010011".U(32.W)
        abstractGeneratedMem(5) := Mux( postexec, 
          Cat(((PROGBUF-ABSTRACT-5*4).U).extract(20), ((PROGBUF-ABSTRACT-5*4).U)(10,1), ((PROGBUF-ABSTRACT-5*4).U).extract(11), ((PROGBUF-ABSTRACT-5*4).U)(19,12), "b000001101111".U(12.W) ), //to PROGBUF (ABSTRACT+5*4+hxx) jal hxx
          "b0010011".U(32.W) ) //nop
        abstractGeneratedMem(6) := "b0010011".U(32.W)
        abstractGeneratedMem(7) := "b000000000000100000000000001110011".U  //ebreak


      } .elsewhen( is_access_FPR ) {
        val reg_sel = regno(4,0)

        abstractGeneratedMem(0) := "b0010011".U(32.W) //addi 0, zero, 0 (nop)
        abstractGeneratedMem(1) := "b0010011".U(32.W)

        abstractGeneratedMem(2) := Cat(
          ((DATA).U)(11,5), // offset h500
          Mux( transfer & write, ((DATA).U)(4,0), reg_sel),          // offset h500 / rs2 = s0
          0.U(5.W),         // rs1 = 0 base
          Mux( aarsize === 2.U, "b010".U(3.W), "b011".U(3.W)),       // word / double-word
          Mux( transfer & write, reg_sel, ((DATA).U)(4,0)),       // rd = s0 / offset h500
          Mux( transfer & write, "b0000111".U(7.W),  "b0100111".U(7.W) ), // ld or st
        )

        abstractGeneratedMem(3) := "b0010011".U(32.W)
        abstractGeneratedMem(4) := "b0010011".U(32.W)
        abstractGeneratedMem(5) := Mux( postexec, 
          Cat(((PROGBUF-ABSTRACT-5*4).U).extract(20), ((PROGBUF-ABSTRACT-5*4).U)(10,1), ((PROGBUF-ABSTRACT-5*4).U).extract(11), ((PROGBUF-ABSTRACT-5*4).U)(19,12), "b000001101111".U(12.W) ), //to PROGBUF (ABSTRACT+5*4+hxx) jal hxx
          "b0010011".U(32.W) ) //nop
        abstractGeneratedMem(6) := "b0010011".U(32.W)
        abstractGeneratedMem(7) := "b000000000000100000000000001110011".U  //ebreak

      }
    } .elsewhen( is_quick_access ) {
      printf("Warning, arriving unsupport region!")
      whereTo := Cat(((PROGBUF-WHERETO).U).extract(20), ((PROGBUF-WHERETO).U)(10,1), ((PROGBUF-WHERETO).U).extract(11), ((PROGBUF-WHERETO).U)(19,12), "b000001101111".U(12.W) ) //to PROGBUF (WHERETO+hxxx) jal hxxx
    } .elsewhen( is_access_memory ) {
      printf("Warning, arriving unsupport region!")
      whereTo := Cat(((ABSTRACT-WHERETO).U).extract(20), ((ABSTRACT-WHERETO).U)(10,1), ((ABSTRACT-WHERETO).U).extract(11), ((ABSTRACT-WHERETO).U)(19,12), "b000001101111".U(12.W) ) //to ABSTRACT (WHERETO+hxxx) jal hxxx

      // csrw s0 dscratch1,
      abstractGeneratedMem(0) := Cat("h7b3".U(12.W), 8.U(5.W), "b001".U(3.W), 0.U(5.W), "b1110011".U(7.W))
      // csrw s1 dscratch2,
      abstractGeneratedMem(1) := Cat("h7b4".U(12.W), 9.U(5.W), "b001".U(3.W), 0.U(5.W), "b1110011".U(7.W))
    
      //load s0, (DATA)+8 (arg1)
      abstractGeneratedMem(2) := Cat(
        ((DATA+8).U),     // offset (DATA+8)
        0.U(5.W),         // rs1 = 0 base
        "b011".U(3.W),    // word / double-word
        8.U(5.W),         //rd=s0
        "b0000011".U(7.W)
      )
      
      //write ld/lw/lh/lb s1 (DATA)(arg0) / read ld s1 s0 
      abstractGeneratedMem(3) := Cat(
        Mux(write, ((DATA).U(12.W)),  0.U(12.W)), // offset (DATA)
        Mux(write, 0.U(5.W), 8.U(5.W) ),          // rs1 = 0 base rs1 = s0
        aamsize,                                  // word / double-word
        9.U(5.W),                                 //rd=s1
        "b0000011".U(7.W)
      )

      //write st/sw/sh/sb s1 s0(0) / read st s1 h500
      abstractGeneratedMem(4) := Cat(
        Mux(write, 0.U(7.W), ((DATA).U)(11,5) ),  // offset h00 / h500
        9.U(5.W),                                 //  rs2 = s1
        Mux(write, 8.U(5.W), 0.U(5.W)),           // rs1 = s0 / rs1 = 0 base
        aamsize,                                  // word / double-word
        Mux(write, 0.U(5.W), ((DATA).U)(4,0)),    //  offset h00 / h500
        "b0100011".U(7.W) ,                       //  st
      )

      // csrr s0 dscratch1   
      abstractGeneratedMem(5) := Cat("h7b3".U(12.W), 0.U(5.W), "b010".U(3.W), 8.U(5.W), "b1110011".U(7.W))
      // csrr s1 dscratch2   
      abstractGeneratedMem(6) := Cat("h7b4".U(12.W), 0.U(5.W), "b010".U(3.W), 9.U(5.W), "b1110011".U(7.W))
      abstractGeneratedMem(7) := "b000000000000100000000000001110011".U

    }



    when( commandEn | isProgramBufferMemAccessAuto | isAbstractDataMemAccessAuto ) { //next cycle the whereTo and abstractGeneratedMem will change to correct state

      when( dmstatus.anynonexistent | dmstatus.anyunavail | dmstatus.anyrunning | ~dmstatus.allhalted) {
        cmderr := 4.U
      } .elsewhen ( busy ) {
        cmderr := 1.U
      } .elsewhen( cmderr =/= 0.U ) {}
      .otherwise {

        when( is_access_register ) {
          when( aarsize === 4.U ) {
            cmderr := 2.U
          } .otherwise {
            busy := true.B
            abstract_hartId := hartsel
            flags(hartsel).isGoing  := true.B
          }
        } 

        when( is_quick_access ) {
          cmderr := 2.U
          // flags(hartsel).isGoing  := true.B

          // busy := true.B
          // abstract_hartId := hartsel
        } 

        when( is_access_memory ) {
          cmderr := 2.U

          // when( is_misalign ) {
          //   cmderr := 2.U
          // } .otherwise {
          //   busy := true.B
          //   abstract_hartId := hartsel

          //   flags(hartsel).isGoing  := true.B

          // }
        }
      }
    } 

  }


  class DebugModuleImp extends DMBase with DMStatus with DMAbstract {



    val dmi_req = Wire(Decoupled(new RegMapperInput( params = RegMapperParams(indexBits = 8, maskBits = 4)) ))
    dmi_req.valid := io.dmi.req.valid
    io.dmi.req.ready := dmi_req.ready

    dmi_req.bits.read  := io.dmi.req.bits.op === 1.U
    dmi_req.bits.index := io.dmi.req.bits.addr
    dmi_req.bits.data  := io.dmi.req.bits.data
    dmi_req.bits.mask  := ~(0.U(8.W))//Mux(io.dmi.req.bits.op === 2.U, ~(0.U(8.W)), 0.U)

    val respMap = Seq(
      (0x04 << 2) -> RegFieldGroup("data", Some("Data used to communicate with Debug Module"),
        (0 to 11).map{ i =>
        // abstractDataMem
          RegField(
            32,
            RegReadFn ( (ready) => {abstractDataMem_ren1(i)  := ready ; (true.B, abstractDataMem_qout(i))}),
            RegWriteFn( (valid, data) => { abstractDataMem_wen1(i) := valid; abstractDataMem_dnxt1(i) := data; true.B } ),
            RegFieldDesc("abstractDataMem",         "abstractDataMem",         reset=Some(0)))

          // WNotifyVal(32, abstractDataMem_qout(i), abstractDataMem_dnxt1(i), abstractDataMem_en1(i))

        }),

      (0x10 << 2) -> RegFieldGroup("dmcontrol", Some("debug module control register"), Seq(
        RegField(1,  dmactive, RegFieldDesc("dmactive",         "dmactive",         reset=Some(0))),
        RegField(1,  ndmreset, RegFieldDesc("ndmreset",         "ndmreset",         reset=Some(0))),
        RegField.w(1, RegWriteFn((valid, data) => { clrresethaltEn := (valid & (data === 1.U)) ; true.B} ),RegFieldDesc("clrresethaltreq", "clrresethaltreq")),
        RegField.w(1, RegWriteFn((valid, data) => { setresethaltEn := (valid & (data === 1.U)) ; true.B} ),RegFieldDesc("setresethaltreq", "setresethaltreq")),
        RegField(2),
        RegField.r(10, 0.U, RegFieldDesc("hartselhi",         "hartselhi(ignore)")),
        RegField.r(10, 0.U, RegFieldDesc("hartsello",         "hartsello(ignore)")),
        RegField.r(1, 0.U, RegFieldDesc("hasel",         "hasel",         reset=Some(0))),
        RegField(1),
        RegField.w(1, RegWriteFn((valid, data) => { { ackhavereset_W1 := valid & (data === 1.U) }; true.B }), RegFieldDesc("ackhavereset", "0: No effect; 1: Clear havereset for any selected harts")),
        RegField(1, hartreset(hartsel), RegFieldDesc("hartreset",         "hartreset",         reset=Some(0))),
        RegField.w(1, RegWriteFn((valid, data) => { resumeReq_W1 := (haltreq(hartsel) === 0.U & valid & data === 1.U); true.B }), RegFieldDesc("resumereq", "Writing 1 causes the currently selected harts to resume once, if they are halted when the write occurs. It also clears the resume ack bit for those harts.resumereq is ignored if haltreq is set.")),
        RegField.w(1, RegWriteFn((valid, data) => { when(valid) {haltreq(hartsel) := data}; true.B }), RegFieldDesc("haltreq",         "haltreq"))
      )),

      (0x11 << 2) -> RegFieldGroup("dmstatus", Some("debug module status register"), Seq(
        RegField.r(4, 2.U,                      RegFieldDesc("version",         "version",                 reset=Some(2))),
        RegField.r(1, 0.U,                      RegFieldDesc("confstrptrvalid", "0: confstrptr0–confstrptr3 hold information which is not relevant to the configuration string", reset=Some(0))),
        RegField.r(1, 1.U,                      RegFieldDesc("hasresethaltreq", "Debug Module supports halt-on-reset functionality controllable by the setresethaltreq and clrresethaltreq bits", reset=Some(1))),
        RegField.r(1, 0.U,                      RegFieldDesc("authbusy",        "authbusy(ignore)",        reset=Some(0))),
        RegField.r(1, 1.U,                      RegFieldDesc("authenticated",   "authenticated(ignore)",   reset=Some(1))),
        RegField.r(1, dmstatus.anyhalted,       RegFieldDesc("anyhalted",       "anyhalted",               reset=Some(0))),
        RegField.r(1, dmstatus.allhalted,       RegFieldDesc("allhalted",       "allhalted",               reset=Some(0))),
        RegField.r(1, dmstatus.anyrunning,      RegFieldDesc("anyrunning",      "anyrunning",              reset=Some(1))),
        RegField.r(1, dmstatus.allrunning,      RegFieldDesc("allrunning",      "allrunning",              reset=Some(1))),
        RegField.r(1, dmstatus.anyunavail,      RegFieldDesc("anyunavail",      "anyunavail",              reset=Some(0))),
        RegField.r(1, dmstatus.allunavail,      RegFieldDesc("allunavail",      "allunavail",              reset=Some(0))),
        RegField.r(1, dmstatus.anynonexistent,  RegFieldDesc("anynonexistent",  "anynonexistent",          reset=Some(0))),
        RegField.r(1, dmstatus.allnonexistent,  RegFieldDesc("allnonexistent",  "allnonexistent",          reset=Some(0))),
        RegField.r(1, dmstatus.anyresumeack,    RegFieldDesc("anyresumeack",    "anyresumeack",            reset=Some(1))),
        RegField.r(1, dmstatus.allresumeack,    RegFieldDesc("allresumeack",    "allresumeack",            reset=Some(1))),
        RegField.r(1, dmstatus.anyhavereset,    RegFieldDesc("anyhavereset",    "anyhavereset",            reset=Some(0))),
        RegField.r(1, dmstatus.allhavereset,    RegFieldDesc("allhavereset",    "allhavereset",            reset=Some(0))),
        RegField(2),
        RegField.r(1, 0.U,       RegFieldDesc("impebreak",       "There is no implicit ebreak after Program Buffer",       reset=Some(0)))
      )),

      (0x12 << 2) -> RegFieldGroup("hartInfo", Some("Hart Info"), Seq(
        RegField.r(12, (DATA).U, RegFieldDesc("dataaddr",         "dataaddr",         reset=Some(16))),
        RegField.r(4,  12.U,    RegFieldDesc("datasize",         "datasize",         reset=Some(12))),
        RegField.r(1,  1.U,     RegFieldDesc("dataaccess",       "dataaccess",       reset=Some(1))),
        RegField(3),
        RegField.r(4,  4.U,    RegFieldDesc("nscratch",         "nscratch",         reset=Some(4))),
      )),

      (0x16 << 2) -> RegFieldGroup("abstractcs", Some("Abstract Control and Status"), Seq(
        RegField.r(4, 12.U, RegFieldDesc("datacount", "datacount", reset=Some(12))),
        RegField(4),
        RegField(3, cmderr, RegWriteFn((valid, data) => { when(valid & data === 1.U) { cmderr := Mux(busy, 1.U, 0.U) }; true.B }), RegFieldDesc("cmderr", "cmderr", reset=Some(0))),
        RegField(1),
        RegField.r(1,  busy, RegFieldDesc("busy", "busy", reset=Some(0))),
        RegField(11),
        RegField.r(5, 16.U, RegFieldDesc("progbufsize", "progbufsize")),
      )),

      (0x17 << 2) -> RegFieldGroup("command", Some("Abstract Command"), Seq(
        WNotifyVal(32, 0.U, wVal = commandVal, wNotify = commandEn, RegFieldDesc("cmdTpye_control", "cmdTpye + control")),
      )),

  // cmderr := Mux(busy, 1.U, 0.U)

      (0x18 << 2) -> RegFieldGroup("abstractauto", Some("Abstract Command Auto execute, Writteing this register while an abstract command is executing causes cmderr to be set to 1(busy) if it is 0"), Seq(
        RegField(12, autoexecdata,    RegWriteFn((valid, data) => { when(valid) { autoexecdata := data };    when(valid & busy & cmderr === 0.U) { cmderr := 1.U }; true.B }),    RegFieldDesc("autoexecdata", "When a bit in this field is 1, read or write accesses to the corresponding data word cause the command in command to be ececuted again", reset=Some(0))),
        RegField(4), 
        RegField(16, autoexecprogbuf, RegWriteFn((valid, data) => { when(valid) { autoexecprogbuf := data }; when(valid & busy & cmderr === 0.U) { cmderr := 1.U }; true.B }), RegFieldDesc("autoexecprogbuf", "When a bit in this field is 1, read or write accesses to the corresponding progbuf word cause the command in  command to be executed again", reset=Some(0))),

      //       val autoexecprogbuf = RegInit(0.U(16.W))
      // val autoexecdata    = RegInit(0.U(12.W))
      )),

      (0x1D << 2) -> RegFieldGroup("nextdm", Some("Next Debug Module"), Seq(
        RegField.r(32, 0.U)
      )),

      (0x20 << 2) -> RegFieldGroup("progbuf", Some("Program buffer used to communicate with Debug Module"),
        (0 to 15).map{ i =>
          RegField(
            32,
            RegReadFn( (ready) => { programBufferMem_ren1(i) := ready; (true.B, programBufferMem_qout(i))}),
            RegWriteFn( (valid, data) => { programBufferMem_wen1(i) := valid; programBufferMem_dnxt1(i) := data; true.B } ),
            RegFieldDesc("progbuf",         "progbuf",         reset=Some(0)))
          // WNotifyVal(32, programBufferMem_qout(i), programBufferMem_dnxt1(i), programBufferMem_wen1(i))
        }),

      // (0x38 << 2) -> RegFieldGroup("sbcs", Some("System Bus Access Control and Status"), Seq(
      //   RegField.r(1, 1.U, RegFieldDesc("sbaccess8",   "sbaccess8" )),
      //   RegField.r(1, 1.U, RegFieldDesc("sbaccess16",  "sbaccess16")),
      //   RegField.r(1, 1.U, RegFieldDesc("sbaccess32",  "sbaccess32")),
      //   RegField.r(1, 1.U, RegFieldDesc("sbaccess64",  "sbaccess64")),
      //   RegField.r(1, 0.U, RegFieldDesc("sbaccess128", "sbaccess128")),
      //   RegField.r(7, 64.U, RegFieldDesc("sbasize",         "sbasize")),
      //   RegField(3, sberror, RegWriteFn((valid, data) => { when (valid & data === 1.U) { sberror := 0.U }; true.B }), RegFieldDesc("sberror", "sberror", reset=Some(0))),
      //   RegField(1, sbreadondata, RegFieldDesc("sbreadondata", "sbreadondata", reset=Some(0))),
      //   RegField(1, sbautoincrement, RegFieldDesc("sbautoincrement", "sbautoincrement", reset=Some(0))),
      //   RegField(3, sbaccess, RegFieldDesc("sbaccess", "sbaccess", reset=Some(2))),
      //   RegField(1, sbreadonaddr, RegFieldDesc("abreadonaddr", "abreadonaddr", reset=Some(0))),
      //   RegField.r(1, sbbusy, RegFieldDesc("sbbusy", "sbbusy", reset=Some(0))),
      //   RegField(1, sbbusyerror, RegWriteFn((valid, data) => { when (valid & data === 1.U) { sbbusyerror := 0.U }; true.B }), RegFieldDesc("sbbusyerror", "sbbusyerror", reset=Some(0))),
      //   RegField(6),
      //   RegField.r(3, 1.U, RegFieldDesc("sbversion", "sbversion")),
      // )),

      // (0x39 << 2) -> RegFieldGroup("sbaddress", Some("System Bus Address"), Seq(
      //   RegField(32, sbaddress(0), RegWriteFn( (valid, data) => { sbaddressWrEn := valid; sbaddressWrData := data; true.B } ), RegFieldDesc("sbaddress0", "sbaddress[31:0]", reset=Some(0))),
      //   RegField(32, sbaddress(1), RegFieldDesc("sbaddress1", "sbaddress[63:32]", reset=Some(0))),
      // )),

      // (0x3C << 2) -> RegFieldGroup("sbdata", Some("System Bus Data"), Seq(
      //   RegField(32, RegReadFn( ivalid => { sbdataRdEn := ivalid; (true.B, sbdata(0))}), RegWriteFn( (valid, data) => { sbdataWrEn := valid; sbdataWrData := data; true.B } ),               RegFieldDesc("sbdata0",         "sbdata[31:0]",         reset=Some(0))),
      //   RegField(32, sbdata(1),                RegFieldDesc("sbdata1",         "sbdata[63:32]",        reset=Some(0))),
      // )),

      (0x40 << 2) -> RegFieldGroup("haltsum0", Some("Halt Summary"), Seq( 
        RegField.r(32, is_halted.asUInt, RegFieldDesc("haltsum", "halt summmary"))
      ))
    )

    val dmi_resp = RegMapper(bytes = 4, concurrency = 0, undefZero = true, in = dmi_req, mapping = respMap: _* )
    
    io.dmi.resp.valid := dmi_resp.valid
    dmi_resp.ready := io.dmi.resp.ready

    io.dmi.resp.bits.addr := DontCare
    io.dmi.resp.bits.data := dmi_resp.bits.data
    io.dmi.resp.bits.op   := DontCare


    peripNode.regmap(
      (HALTED ) -> Seq(WNotifyVal(32, 0.U, hartHaltedId, hartHaltedWrEn, RegFieldDesc("debug_hart_halted", "Debug ROM Causes hart to write its hartID here when it is in Debug Mode."))), //HALTED
      (GOING ) -> Seq(WNotifyVal(32, 0.U, hartGoingId,  hartGoingWrEn, RegFieldDesc("debug_hart_going", "Debug ROM causes hart to write 0 here when it begins executing Debug Mode instructions."))), //GOING
      (RESUMING ) -> Seq(WNotifyVal(32, 0.U, hartResumingId,  hartResumingWrEn, RegFieldDesc("debug_hart_resuming", "Debug ROM causes hart to write its hartID here when it leaves Debug Mode."))), //RESUMING
      (EXCEPTION ) -> Seq(WNotifyVal(32, 0.U, hartExceptionId,  hartExceptionWrEn, RegFieldDesc("debug_hart_exception", "Debug ROM causes hart to write 0 here if it gets an exception in Debug Mode."))), //EXCEPTION
      (WHERETO ) -> Seq(RegField.r(32, whereTo, RegFieldDesc("debug_whereto", "Instruction filled in by Debug Module to control hart in Debug Mode", volatile = true))),
      
      (ABSTRACT ) -> RegFieldGroup("debug_abstract", Some("Instructions generated by Debug Module"), abstractGeneratedMem.zipWithIndex.map{ case (x, _) => RegField.r(32, x)}),
      (PROGBUF ) -> RegFieldGroup("debug_progbuf", Some("Program buffer used to communicate with Debug Module"),
        (0 to 15).map{ i => 
          // RegField(32, programBufferMem_qout(i), RegFieldDesc("programBuffer", "programBuffer", reset=Some(0)))
          WNotifyVal(32, programBufferMem_qout(i), programBufferMem_dnxt2(i), programBufferMem_wen2(i))
        }),
      (DATA ) -> RegFieldGroup("debug_data", Some("Data used to communicate with Debug Module"),
        (0 to 11).map{ i =>
          // RegField(32, abstractDataMem_qout(i), RegFieldDesc("abstractDataMem", "abstractDataMem", reset=Some(0)))
          WNotifyVal(32, abstractDataMem_qout(i), abstractDataMem_dnxt2(i), abstractDataMem_wen2(i))
        }),
      
      (FLAGS ) -> RegFieldGroup("debug_flags", Some("Memory region used to control hart going/resuming in Debug Mode"), flags.zipWithIndex.map{case(x, _) => RegField.r(8, x.asUInt)}),
      (ROMBASE ) -> RegFieldGroup("debug_rom", Some("Debug ROM"), DebugRomContents().zipWithIndex.map{case (x, _) => RegField.r(8, (x & 0xFF).U(8.W))}),
    )

    //Abstract Debug
    when( io.dmi.req.fire ) {
      when( io.dmi.req.bits.op === 1.U ) { printf("\nReq: READ ") }
      when( io.dmi.req.bits.op === 2.U ) { printf("\nReq: WRITE 0x%x to ", io.dmi.req.bits.data) }
      when(      io.dmi.req.bits.addr === ((0x04).U) ) { printf("0x04, \"data\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x05).U) ) { printf("0x05, \"data\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x06).U) ) { printf("0x06, \"data\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x07).U) ) { printf("0x07, \"data\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x08).U) ) { printf("0x08, \"data\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x09).U) ) { printf("0x09, \"data\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x0A).U) ) { printf("0x0A, \"data\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x0B).U) ) { printf("0x0B, \"data\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x0C).U) ) { printf("0x0C, \"data\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x0D).U) ) { printf("0x0D, \"data\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x0E).U) ) { printf("0x0E, \"data\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x0F).U) ) { printf("0x0F, \"data\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x10).U) ) { printf("0x10, \"dmcontrol\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x11).U) ) { printf("0x11, \"dmstatus\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x12).U) ) { printf("0x12, \"hartInfo\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x16).U) ) { printf("0x16, \"abstractcs\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x17).U) ) { printf("0x17, \"command\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x18).U) ) { printf("0x18, \"abstractauto\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x1D).U) ) { printf("0x1D, \"nextdm\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x20).U) ) { printf("0x20, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x21).U) ) { printf("0x21, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x22).U) ) { printf("0x22, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x23).U) ) { printf("0x23, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x24).U) ) { printf("0x24, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x25).U) ) { printf("0x25, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x26).U) ) { printf("0x26, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x27).U) ) { printf("0x27, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x28).U) ) { printf("0x28, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x29).U) ) { printf("0x29, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x2A).U) ) { printf("0x2A, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x2B).U) ) { printf("0x2B, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x2C).U) ) { printf("0x2C, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x2D).U) ) { printf("0x2D, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x2E).U) ) { printf("0x2E, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x2F).U) ) { printf("0x2F, \"progbuf\"\n") }
      .elsewhen( io.dmi.req.bits.addr === ((0x40).U) ) { printf("0x40, \"haltsum0\"\n") }
      .otherwise{ printf(" undefine address: %x \n", io.dmi.req.bits.addr) }
    }

    when( io.dmi.resp.fire ) {
      printf( "Resp: 0x%x\n", io.dmi.resp.bits.data )
    }

    // when( hartHaltedWrEn )   { printf( "CPU Write 0x%x to HALTED\n", hartHaltedId ) }
    // when( hartGoingWrEn )    { printf( "CPU Write 0x%x to GOING\n", hartGoingId ) }
    // when( hartResumingWrEn ) { printf( "CPU Write 0x%x to RESUMING\n", hartResumingId ) }
    // when( hartExceptionWrEn ) { printf( "CPU Write 0x%x to EXCEPTION\n", hartExceptionId ) }



  }

  trait sba { this: DMBase =>
    // val ( sba_bus, edgeOut ) = sbaClientNode.out.head

    //   val sberror = RegInit(0.U(3.W))
    //   val sbreadondata = RegInit(false.B)
    //   val sbautoincrement = RegInit(false.B)
    //   val sbaccess = RegInit(2.U(3.W))
    //   val sbreadonaddr = RegInit(false.B)
    //   val sbbusy = RegInit(false.B)
    //   val sbbusyerror = RegInit(false.B)
    //   val sbaddress = RegInit(VecInit(Seq.fill(2)(0.U(32.W))))
    //   val sbdata = RegInit(VecInit(Seq.fill(2)(0.U(32.W))))

    //   val sbaddressWrEn   = Wire(Bool())
    //   val sbaddressWrData = Wire(UInt(32.W))

    //   val sbdataWrEn   = Wire(Bool())
    //   val sbdataWrData = Wire(UInt(32.W)) 
    //   val sbdataRdEn   = Wire(Bool())




    //   val sba = {
    //     val mdl = Module(new SBA(edgeOut))
    //     sba_bus.a.valid := mdl.io.getPut.valid
    //     sba_bus.a.bits := mdl.io.getPut.bits
    //     mdl.io.getPut.ready := sba_bus.a.ready

    //     mdl.io.access.valid := sba_bus.d.valid
    //     mdl.io.access.bits := sba_bus.d.bits
    //     sba_bus.d.ready := mdl.io.access.ready
    //     mdl
    //   }

    //     val req = Flipped(Decoupled(new Info_sba_req))
    //     val rsp = Decoupled(new Info_sba_rsp)

    //   val sba_req_valid = RegInit(false.B)
    //   val sba_op = RegInit(false.B)
    //   sba.io.req.valid := sba_req_valid

    //   sba.io.req.bits.paddr := Cat(sbaddress(1),sbaddress(0))
    //   sba.io.req.bits.wdata := Cat(sbdata(1), sbdata(0))
    //   sba.io.req.bits.is_byte   := (sbaccess === 0.U)
    //   sba.io.req.bits.is_half   := (sbaccess === 1.U)
    //   sba.io.req.bits.is_word   := (sbaccess === 2.U)
    //   sba.io.req.bits.is_dubl := (sbaccess === 3.U)
    //   sba.io.req.bits.is_rd_wrn := sba_op


    //           // val sbaddressWrEn   = Wire(Bool())
    //           // val sbaddressWrData = Wire(UInt(32.W))

    //           // val sbdataWrEn   = Wire(Bool())
    //           // val sbdataWrData = Wire(UInt(32.W)) 
    //           // val sbdataRdEn   = Wire(Bool())

    //   when( sbaddressWrEn ) {
    //     when( sbbusy ) {
    //       sbbusyerror := true.B
    //     } .otherwise {
    //       sbaddress(0) := sbaddressWrData
    //       when( sbreadonaddr & sberror === 0.U & ~sbbusyerror) {
    //         sbbusy := true.B
    //         sba_op := true.B
    //         sba_req_valid := true.B
    //       }
    //     }
    //   }

    //   when( sbdataRdEn ) {
    //     when( sbbusy ) {
    //       sbbusyerror := true.B
    //     } .elsewhen( sbreadondata & sberror === 0.U & ~sbbusyerror) {
    //       sbbusy := true.B
    //       sba_op := true.B
    //       sba_req_valid := true.B
    //     }
    //   }

    //   when( sbdataWrEn ) {
    //     when( sbbusy ) {
    //       sbbusyerror := true.B
    //     } .otherwise {
    //       sbdata(0) := sbdataWrData
    //       when( sberror === 0.U & ~sbbusyerror) {
    //         sbbusy := true.B
    //         sba_op := false.B
    //         sba_req_valid := true.B
    //       }      
    //     }
    //   }

    //   when( sba.io.req.fire ) {
    //     sba_req_valid := false.B
    //   }

    //   sba.io.rsp.ready := true.B

    //   when( sba.io.rsp.fire ) {
    //     sbbusy := false.B
    //     when( sbautoincrement ) {
    //       sbaddress(1) := (Cat(sbaddress(1), sbaddress(0)) + (1.U << sbaccess)) >> 32
    //       sbaddress(0) := sbaddress(0) + (1.U << sbaccess)
    //     }

    //     when( sba_op === true.B ) {
    //       sbdata(0) := sba.io.rsp.bits.rdata(31,0) 
    //       sbdata(1) := sba.io.rsp.bits.rdata(63,32)
    //     }

    //   }
  }
  


  lazy val module = new DebugModuleImp {}
// WNotifyVal {
//   def apply(n: Int, rVal: UInt, wVal: UInt, wNotify: Bool, desc: RegFieldDesc)

// }
}


