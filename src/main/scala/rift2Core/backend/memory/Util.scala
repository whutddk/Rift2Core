


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

package rift2Core.backend.memory

import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._

import rift2Core.L1Cache._
import rift2Core.privilege._
import rift._
import base._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class addrTrans extends Module {
  val io = IO(new Bundle{
    val iss_exe = Input(new Lsu_iss_info)
    val mmu_lsu = Flipped(DecoupledIO(new Info_mmu_rsp))
    val block = Input(Bool())

    val deq = new DecoupledIO(new Lsu_iss_info)
  })

  /** the transation will be blocked if the request is illegal */
  io.deq.valid := io.mmu_lsu.valid & ~io.mmu_lsu.bits.is_fault & ~io.iss_exe.is_misAlign & ~io.block
  
  {
    io.deq.bits := io.iss_exe
    io.deq.bits.param.dat.op1 := io.mmu_lsu.bits.paddr
    io.mmu_lsu.ready := io.deq.ready & ~io.mmu_lsu.bits.is_fault & ~io.iss_exe.is_misAlign & ~io.block
  }


}

object addrTrans {
  def apply( iss_exe: Lsu_iss_info, mmu_lsu: DecoupledIO[Info_mmu_rsp], block: Bool ): DecoupledIO[Lsu_iss_info] = {
    val mdl = Module(new addrTrans)
    mdl.io.iss_exe <> iss_exe
    mdl.io.mmu_lsu <> mmu_lsu
    mdl.io.block := block

    return mdl.io.deq
  }
}

class OpMux extends Module {
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Lsu_iss_info))
    val st_deq = DecoupledIO(new Lsu_iss_info)
    val ld_deq = DecoupledIO(new Lsu_iss_info)
    val am_deq = DecoupledIO(new Lsu_iss_info) 
  })

  io.enq.ready := false.B
  when( io.enq.bits.fun.is_lu ) {
    io.ld_deq <> io.enq
  } .otherwise{
    io.ld_deq.valid := false.B
    io.ld_deq.bits  := DontCare
  }

  when( io.enq.bits.fun.is_su ) {
    io.st_deq <> io.enq
  } .otherwise{
    io.st_deq.valid := false.B
    io.st_deq.bits  := DontCare
  }

  when( io.enq.bits.fun.is_amo ) {
    io.am_deq <> io.enq
  } .otherwise{
    io.am_deq.valid := false.B
    io.am_deq.bits  := DontCare
  }

}

class regionMux(implicit p: Parameters) extends DcacheModule{
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Info_cache_s0s1))
    val deq = Vec(3, new DecoupledIO(new Info_cache_s0s1))
  })

  val psel = io.enq.bits.paddr(31,28)
  val sel = Mux( psel(3), 0.U, Mux( psel(2), 1.U, 2.U) )


  io.enq.ready := false.B

  when( sel === 2.U ) {
    io.deq(2) <> io.enq
  } .otherwise {
    io.deq(2).valid := false.B
    io.deq(2).bits  := DontCare
  }

  when( sel === 1.U ) {
    io.deq(1) <> io.enq
  } .otherwise {
    io.deq(1).valid := false.B
    io.deq(1).bits  := DontCare
  }

  when( sel === 0.U ) {
    io.deq(0) <> io.enq
  } .otherwise {
    io.deq(0).valid := false.B
    io.deq(0).bits  := DontCare
  }
}

class cacheMux(implicit p: Parameters) extends DcacheModule{
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Info_cache_s0s1))
    val deq = Vec(nm, new DecoupledIO(new Info_cache_s0s1))
  })

  val chn = io.enq.bits.paddr(12+nm_w-1,12)

  io.enq.ready := false.B
  
  for ( i <- 0 until nm ) yield {
    when( i.U === chn ) {
      io.deq(i) <> io.enq
    } .otherwise {
      io.deq(i).valid := false.B
      io.deq(i).bits  := DontCare
    }
  }
}





object Strb2Mask{
  def apply(strb: UInt): UInt = {
    val mask = Wire(UInt(64.W))

    // for ( i <- 0 until 8 ) yield {
    //   mask(8*i+7,8*i) := Fill(8, strb(i))
    // } 
    mask := Cat(
      Fill(8, strb(7)), Fill(8, strb(6)),
      Fill(8, strb(5)), Fill(8, strb(4)),
      Fill(8, strb(3)), Fill(8, strb(2)),
      Fill(8, strb(1)), Fill(8, strb(0))
    )
    mask
  } 
}

object align_mem{
  def apply(ori: Lsu_iss_info) = {
    val wdata = {
      val res = Wire(UInt(64.W))
      val paddr = ori.param.dat.op1
      val shift = Wire(UInt(6.W))
      shift := Cat( paddr(2,0), 0.U(3.W) )
      res   := ori.param.dat.op2 << shift  
      res
    }

    val wstrb = {
      val paddr = ori.param.dat.op1
      val op = ori.fun

      Mux1H(Seq(
        op.is_byte -> "b00000001".U, op.is_half -> "b00000011".U,
        op.is_word -> "b00001111".U, op.is_dubl -> "b11111111".U
      )) << Cat(paddr(2,0), 0.U(3.W) )
    }

    val paddr = ori.param.dat.op1 & ~(("b111".U)(64.W))

    (paddr, wdata, wstrb)
  }
}

object pkg_Info_cache_s0s1{
  def apply( ori: Info_miss_rsp )(implicit p: Parameters) = {
    val res = Wire(new Info_cache_s0s1)

    res.paddr := ori.paddr
    res.wstrb := "hFF".U
    res.wdata(0) := ori.wdata(63,0)
    res.wdata(1) := ori.wdata(127,64)
    res.wdata(2) := ori.wdata(191,128)
    res.wdata(3) := ori.wdata(255,192)

    {
      res.fun := 0.U.asTypeOf(new Cache_op)
      res.fun.grant := true.B      
    }
    res.rd := DontCare
    res.chk_idx := DontCare
    res
  }
  
  def apply( ori: Info_probe_req )(implicit p: Parameters) = {
    val res = Wire(new Info_cache_s0s1)
    res.paddr := ori.paddr
    res.wstrb := DontCare
    res.wdata := DontCare

    {
      res.fun := 0.U.asTypeOf(new Cache_op)
      res.fun.probe := true.B      
    }
    res.rd := DontCare
    res.chk_idx := DontCare
    res
  }

  def apply( ori: Lsu_iss_info )(implicit p: Parameters) = {

    val res = Wire(new Info_cache_s0s1)

    val (paddr, wdata, wstrb ) = align_mem( ori )
    res.paddr := paddr
    res.wdata := VecInit( Seq.fill(4)(wdata) )
    res.wstrb := wstrb


    {
      res.fun := 0.U.asTypeOf(new Cache_op)
      // res.fun := ori.fun
      res.fun.lb  := ori.fun.lb
      res.fun.lh  := ori.fun.lh
      res.fun.lw  := ori.fun.lw
      res.fun.ld  := ori.fun.ld
      res.fun.lbu := ori.fun.lbu
      res.fun.lhu := ori.fun.lhu
      res.fun.lwu := ori.fun.lwu
      res.fun.sb  := ori.fun.sb
      res.fun.sh  := ori.fun.sh
      res.fun.sw  := ori.fun.sw
      res.fun.sd  := ori.fun.sd
      res.fun.fence      := ori.fun.fence
      res.fun.fence_i    := ori.fun.fence_i
      res.fun.sfence_vma := ori.fun.sfence_vma
      res.fun.lr_w       := ori.fun.lr_w
      res.fun.sc_w       := ori.fun.sc_w
      res.fun.amoswap_w  := ori.fun.amoswap_w
      res.fun.amoadd_w   := ori.fun.amoadd_w
      res.fun.amoxor_w   := ori.fun.amoxor_w
      res.fun.amoand_w   := ori.fun.amoand_w
      res.fun.amoor_w    := ori.fun.amoor_w
      res.fun.amomin_w   := ori.fun.amomin_w
      res.fun.amomax_w   := ori.fun.amomax_w
      res.fun.amominu_w  := ori.fun.amominu_w
      res.fun.amomaxu_w  := ori.fun.amomaxu_w
      res.fun.lr_d       := ori.fun.lr_d
      res.fun.sc_d       := ori.fun.sc_d
      res.fun.amoswap_d  := ori.fun.amoswap_d
      res.fun.amoadd_d   := ori.fun.amoadd_d
      res.fun.amoxor_d   := ori.fun.amoxor_d
      res.fun.amoand_d   := ori.fun.amoand_d
      res.fun.amoor_d    := ori.fun.amoor_d
      res.fun.amomin_d   := ori.fun.amomin_d
      res.fun.amomax_d   := ori.fun.amomax_d
      res.fun.amominu_d  := ori.fun.amominu_d
      res.fun.amomaxu_d  := ori.fun.amomaxu_d
      res.fun.flw        := ori.fun.flw
      res.fun.fsw        := ori.fun.fsw
      res.fun.fld        := ori.fun.fld
      res.fun.fsd        := ori.fun.fsd
    }
    res.rd.rd0 := ori.param.rd0
    res.rd.is_iwb := ori.param.is_iwb
    res.rd.is_fwb := ori.param.is_fwb
    res.chk_idx := DontCare
    res
  
  }


}

object overlap_wr{
  def apply( ori: UInt, ori_wstrb: UInt, wdata: UInt, wstrb: UInt): (UInt, UInt) = {
    val wmask = Strb2Mask(wstrb)

    val new_data = (ori & ~wmask) | (wdata & wmask)
    val new_strb = ori_wstrb | wstrb

    return (new_data, new_strb)
  }
}

// class overlap_chk extends Module {
//   val io = IO(new Bundle{
//     val rd_info = Flipped(ValidIO(new Bundle{
//       val paddr = UInt(64.W)
//       val rdata = UInt(64.W)
//     }))
//     val wr_info = new Info_overlap

//     val real_rd = Output(UInt(64.W))
//   })

//   io.wr_info.req.valid <> io.rd_info.valid
//   io.wr_info.req.bits.paddr <> io.rd_info.bits.paddr

//   io.real_rd :=
//     Mux(
//       io.wr_info.rsp.valid,
//       overlap_wr( io.rd_info.bits.rdata, DontCare, io.wr_info.rsp.bits.wdata, io.wr_info.rsp.bits.wstrb)._1,
//       io.rd_info.bits.rdata
//     )

// }


// object overlap_chk{
//   def apply( rd: Info_overlap, wr: Info_overlap): UInt = {
//     val mdl = Module(new overlap_chk)
//     mdl.io.rd_info <> rd
//     mdl.io.wr_info <> wr
//     return mdl.io.real_rd
//   }
// }

