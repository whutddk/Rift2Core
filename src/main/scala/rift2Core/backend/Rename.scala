
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

package rift2Core.backend

import chisel3._
import chisel3.util._
import base._
import rift2Core.define._
import rift2Chip._
import chipsalliance.rocketchip.config.Parameters


abstract class RenameBase()(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val rnReq = Vec(rnChn, Flipped(new DecoupledIO(new IF4_Bundle)))
    val rnRsp = Vec(rnChn, (new DecoupledIO(new Dpt_info)))

    val xLookup = Vec( rnChn, new Lookup_Bundle )
    val xRename = Vec( rnChn, new Rename_Bundle )

    val fLookup = Vec( rnChn, new Lookup_Bundle )
    val fRename = Vec( rnChn, new Rename_Bundle )

    val cLookup = Vec( rnChn, Flipped(new SeqReg_Lookup_Bundle(4) ) )
    val cRename = Vec( rnChn, Flipped(new SeqReg_Rename_Bundle(4) ) )

    val vLookup = Vec( rnChn, new Lookup_Bundle )
    val vRename = Vec( rnChn, new Rename_Bundle )



    val rod_i = Vec(cmChn,new DecoupledIO(new Info_reorder_i))

  })

  val rnRspReport = Module( new RePort( new Dpt_info, port = rnChn) )
  val rnRspFifo = Module(new MultiPortFifo(new Dpt_info, aw = (if(!isMinArea) 4 else 1 ), rnChn, rnChn))
  rnRspFifo.io.enq <> rnRspReport.io.deq
  rnRspFifo.io.deq <> io.rnRsp

  val reOrder_fifo_i = {
    val mdl = Module(new MultiPortFifo(new Info_reorder_i, aw = (if(!isMinArea) 4 else 1 ), rnChn, cmChn))
    mdl.io.deq <> io.rod_i
    mdl
  }

  val reg_phy = Wire(Vec(rnChn, new Reg_PHY ) )

  def Pkg_Rename_Info( instr: Info_instruction, rename: Reg_PHY, csrr: UInt, csrw: UInt ): Dpt_info = {
    val res = Wire(new Dpt_info)

    res.alu_isa    := instr.alu_isa
    res.bru_isa    := instr.bru_isa
    res.lsu_isa    := instr.lsu_isa
    res.csr_isa    := instr.csr_isa
    res.mul_isa    := instr.mul_isa
    res.vectorIsa  := instr.vectorIsa
    res.privil_isa := 0.U.asTypeOf( new Privil_isa )
    res.fpu_isa    := instr.fpu_isa
    res.param      := instr.param
    res.phy        := rename

                    when( instr.fpu_isa.is_fpu ) {
                      when(~instr.fpu_isa.hasTwoRs) { res.phy.rs2 := 0.U }
                      when(~instr.fpu_isa.hasThreeRs) { res.phy.rs3 := 0.U }      
                    }
    res.csrr :=
      Mux(
        instr.csr_isa.is_csr & instr.param.imm === "b300".U,
        Cat( instr.param.imm(11,0), csrr( (log2Ceil(4)-1), 0 ) ),
        0.U
      )
    res.csrw :=
      Mux(
        instr.csr_isa.is_csr & instr.param.imm === "b300".U,
        Cat( instr.param.imm(11,0), csrw( (log2Ceil(4)-1), 0 ) ),
        0.U
      )
    return res
  }


  def Pkg_rod_i(instr:Info_instruction, rename: Reg_PHY, csrw: UInt): Info_reorder_i = {
    val res = Wire(new Info_reorder_i)

      res.pc             := instr.param.pc
      res.rd0_raw        := instr.param.raw.rd0
      res.rd0_phy        := rename.rd0
      res.rd1_raw        := Mux( instr.vectorIsa.is2Malloc, instr.param.raw.rd0 + 1.U, 0.U)
      res.rd1_phy        := Mux( instr.vectorIsa.is2Malloc, rename.rd1, 0.U)
      res.is_branch      := instr.bru_isa.is_branch
      res.is_jalr        := instr.bru_isa.jalr
      res.is_lu          := instr.lsu_isa.is_lu
      res.is_su          := instr.lsu_isa.is_su
      res.is_amo         := instr.lsu_isa.is_amo
      res.is_fence       := instr.lsu_isa.fence
      res.is_fence_i     := instr.lsu_isa.fence_i
      res.is_sfence_vma  := instr.lsu_isa.sfence_vma
      res.is_wfi         := instr.alu_isa.wfi
      res.is_csr         := instr.csr_isa.is_csr
      res.is_fcsr        := instr.fpu_isa.is_fpu
      res.is_fpu         := instr.fpu_isa.is_fpu | instr.lsu_isa.is_fpu
      res.privil         := instr.privil_isa
      res.is_illeage     := instr.is_illeage
      res.is_rvc         := instr.param.is_rvc

      res.isXcmm := instr.is_iwb
      res.isFcmm := instr.is_fwb
      res.isVcmm := instr.isVwb

      res.csrw   := 
        Mux(
          instr.csr_isa.is_csr & instr.param.imm === "b300".U,
          Cat( instr.param.imm(11,0), csrw( (log2Ceil(4)-1), 0 ) ),
          0.U
        )


    return res
  }
}

trait RenameMalloc { this: RenameBase =>
  for ( i <- 0 until rnChn ) yield {

    rnRspReport.io.enq(i).valid := io.rnReq(i).fire & ~io.rnReq(i).bits.is_privil_dpt
    rnRspReport.io.enq(i).bits  := Pkg_Rename_Info(io.rnReq(i).bits, reg_phy(i), io.cLookup(i).rsp, io.cRename(i).rsp )


    io.xRename(i).req.valid    := io.rnReq(i).fire & io.rnReq(i).bits.is_iwb
    io.xRename(i).req.bits.rd0 := io.rnReq(i).bits.param.raw.rd0

    io.fRename(i).req.valid    := io.rnReq(i).fire & io.rnReq(i).bits.is_fwb
    io.fRename(i).req.bits.rd0 := io.rnReq(i).bits.param.raw.rd0

    if(hasVector){
      io.vRename(i).req.valid    := io.rnReq(i).fire & io.rnReq(i).bits.isVwb
      io.vRename(i).req.bits.rd0 := io.rnReq(i).bits.param.raw.rd0
      io.vRename(i).req.bits.rd1 := Mux( io.rnReq(i).bits.vectorIsa.is2Malloc, io.rnReq(i).bits.param.raw.rd0 + 1.U, 0.U )
            
    } else {
      io.vRename(i).req.valid := false.B
      io.vRename(i).req.bits.rd0 := 0.U
      io.vRename(i).req.bits.rd1 := 0.U
    }


    for ( j <- 0 until rnChn ) { assert( PopCount( Seq(io.vRename(j).req.fire, io.fRename(j).req.fire, io.xRename(j).req.fire)) <= 1.U, "Assert Failed, rename should be one-hot" ) }


    reg_phy(i).rd0 := 
      Mux1H(
        Seq(io.xRename(i).req.fire -> io.xRename(i).rsp.rd0) ++
        (if(fpuNum > 0) { Seq(io.fRename(i).req.fire -> io.fRename(i).rsp.rd0) } else { Seq() } ) ++
        (if(hasVector)  { Seq(io.vRename(i).req.fire -> io.vRename(i).rsp.rd0) } else { Seq() } )
      )

    reg_phy(i).rd1 := 
      (if(hasVector){ Mux(io.vRename(i).req.fire, io.vRename(i).rsp.rd1, 0.U) } else { 0.U })

  }
}




trait WriteBackLookup {  this: RenameBase =>
  for ( i <- 0 until rnChn ) {
    io.xLookup(i).req.rs1 := io.rnReq(i).bits.param.raw.rs1
    io.xLookup(i).req.rs2 := io.rnReq(i).bits.param.raw.rs2
    io.xLookup(i).req.rs3 := 0.U
    io.xLookup(i).req.rs4 := 0.U
    io.xLookup(i).req.rs5 := 0.U

    if( fpuNum > 0 ) {
      io.fLookup(i).req.rs1 := io.rnReq(i).bits.param.raw.rs1
      io.fLookup(i).req.rs2 := io.rnReq(i).bits.param.raw.rs2
      io.fLookup(i).req.rs3 := io.rnReq(i).bits.param.raw.rs3      
      io.fLookup(i).req.rs4 := 0.U
      io.fLookup(i).req.rs5 := 0.U
    }

    if(hasVector){
      io.vLookup(i).req.rs1 := io.rnReq(i).bits.param.raw.rs1
      io.vLookup(i).req.rs2 := io.rnReq(i).bits.param.raw.rs2
      io.vLookup(i).req.rs3 := Mux( io.rnReq(i).bits.vectorIsa.isLookUpVS3, io.rnReq(i).bits.param.raw.rd0, 0.U)
      io.vLookup(i).req.rs4 := 
        Mux1H(Seq(
          io.rnReq(i).bits.vectorIsa.isLookUpVS2P -> (io.rnReq(i).bits.param.raw.rs2 + 1.U),
          io.rnReq(i).bits.vectorIsa.isLookUpVS3P -> (io.rnReq(i).bits.param.raw.rd0 + 1.U),
        ))
      io.vLookup(i).req.rs5 := 0.U //always request v0

      when( io.rnReq(i).fire ) {
        when( io.rnReq(i).bits.vectorIsa.isLookUpVS2P ) { assert( io.rnReq(i).bits.param.raw.rs2 =/= 31.U ) }
        when( io.rnReq(i).bits.vectorIsa.isLookUpVS3P ) { assert( io.rnReq(i).bits.param.raw.rd0 =/= 31.U ) }
      }    
    }




    reg_phy(i).rs1 :=
      MuxCase( io.xLookup(i).rsp.rs1, Seq() ++
        ( if( fpuNum > 0 ) { Seq(io.rnReq(i).bits.fpu_isa.is_fop -> io.fLookup(i).rsp.rs1) } else Seq() ) ++ 
        ( if( hasVector ) { Seq(
            io.rnReq(i).bits.vectorIsa.isLookUpRS1 -> io.xLookup(i).rsp.rs1,
            io.rnReq(i).bits.vectorIsa.isLookUpFS1 -> io.fLookup(i).rsp.rs1,
            io.rnReq(i).bits.vectorIsa.isLookUpVS1 -> io.vLookup(i).rsp.rs1,          
          )} else { Seq() }
        )
      )
    reg_phy(i).rs2 :=
      MuxCase( io.xLookup(i).rsp.rs2, Seq() ++ 
        ( if( fpuNum > 0 ) { Seq((io.rnReq(i).bits.fpu_isa.is_fop | io.rnReq(i).bits.lsu_isa.is_fst) -> io.fLookup(i).rsp.rs2) } else {Seq()} ) ++
        ( if( hasVector )  { Seq( io.rnReq(i).bits.vectorIsa.isLookUpVS2 -> io.vLookup(i).rsp.rs2) } else { Seq() } )
      )

    reg_phy(i).rs3 :=
      MuxCase(0.U, Seq() ++ 
        (if( fpuNum > 0 ) { Seq(io.rnReq(i).bits.fpu_isa.is_fop -> io.fLookup(i).rsp.rs3)}        else {Seq()} ) ++
        (if( hasVector )  { Seq(io.rnReq(i).bits.vectorIsa.isLookUpVS3 -> io.vLookup(i).rsp.rs3)} else {Seq()} )
      )

    reg_phy(i).rs4 :=
      MuxCase(0.U, Seq() ++
        (if( hasVector ) { Seq(
          io.rnReq(i).bits.vectorIsa.isLookUpVS2P -> io.vLookup(i).rsp.rs4,
          io.rnReq(i).bits.vectorIsa.isLookUpVS3P -> io.vLookup(i).rsp.rs4,
        )} else {Seq()} )
      )
    reg_phy(i).rs5 :=
        (if( hasVector ) { io.vLookup(i).rsp.rs5 }
        else { 0.U })


  }
}



trait LoadRob{ this: RenameBase =>
  reOrder_fifo_i.io.flush := false.B
  for ( i <- 0 until rnChn ){
    reOrder_fifo_i.io.enq(i).valid := io.rnReq(i).fire
    reOrder_fifo_i.io.enq(i).bits  := Pkg_rod_i(io.rnReq(i).bits, reg_phy(i), io.cRename(i).rsp)    
  }

}


trait CSRLoopup{ this: RenameBase =>
  for ( i <- 0 until rnChn ) {
    io.cLookup(i).req := 
      Mux1H(Seq(
        (io.rnReq(i).bits.csr_isa.is_csr & io.rnReq(i).bits.param.imm === "b300".U ) -> "b300".U,
      ))
  }
}

trait CSRMalloc{ this: RenameBase =>
  for ( i <- 0 until rnChn ) {
    io.cRename(i).req.bits := 
      Mux1H(Seq(
        (io.rnReq(i).bits.csr_isa.is_csr & io.rnReq(i).bits.param.imm === "b300".U ) -> "b300".U,
      ))

    io.cRename(i).req.valid :=
      (io.rnReq(i).bits.csr_isa.is_csr & io.rnReq(i).bits.param.imm === "b300".U )
  }




}


trait RenameFeatureCheck { this: RenameBase =>
  if ( fpuNum > 0 ) {
  } else {
    for ( i <- 0 until rnChn ) {
      when( io.rnReq(i).fire ) {
        assert(io.rnReq(i).bits.fpu_isa.is_fpu === false.B)
        assert(io.rnReq(i).bits.is_fwb === false.B)
      }
    }
  }
  if( mulNum > 0 ) {

  } else {
    for ( i <- 0 until rnChn ) {
      when( io.rnReq(i).fire ) {
        assert(io.rnReq(i).bits.mul_isa.is_mulDiv === false.B, "Assert Failed at Rename, MulDiv is not supported in this Version!")
      }
    }    
  }

  if( hasVector ){

  } else {
    for ( i <- 0 until rnChn ) {
      when( io.rnReq(i).fire ) {
        assert(io.rnReq(i).bits.vectorIsa.isVector === false.B, "Assert Failed at Rename, Vector is not supported in this Version!")
      }
    }  
  }
}



class Rename()(implicit p: Parameters) extends RenameBase
with RenameMalloc
with WriteBackLookup
with CSRLoopup
with CSRMalloc
with LoadRob
with RenameFeatureCheck {

  rnRspFifo.io.flush := false.B


  for (i <- 0 until rnChn ) yield {
    io.rnReq(i).ready := (
      for ( j <- 0 to i by 1 ) yield {
        io.xRename(j).req.ready & io.fRename(j).req.ready & io.cRename(i).req.ready & reOrder_fifo_i.io.enq(i).ready
        rnRspFifo.io.enq(j).ready
      }
    ).reduce(_&_)
  }
}


