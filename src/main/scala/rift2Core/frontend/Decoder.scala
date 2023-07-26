





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
package rift2Core.frontend

import chisel3._
import chisel3.util._
import rift2Core.define._

import rift2Chip._
import org.chipsalliance.cde.config._


/**
  * This class defines the input and output ports for a RISC-V 16-bit decoder.
  * @param x The 32-bit instruction to decode, taken as input.
  * @param pc The current program counter, taken as input.
  * @param info The decoded instruction, returned as an output of type Info_instruction.
  */
class DecodeIO(implicit p: Parameters) extends RiftBundle{
  val x  = Input(UInt(32.W))
  val pc = Input(UInt(64.W))
  val info = Output( new Info_instruction )    
}

abstract class DecodeBase (implicit p: Parameters) extends RiftModule {
  val io: DecodeIO = IO(new DecodeIO)

  val x  = WireDefault(io.x)
  val pc = WireDefault(io.pc)
  val info = Wire(new Info_instruction)

  io.info := info
  info.param.pc := pc

}

class Decode16(implicit p: Parameters) extends DecodeBase{
  info.param.is_rvc := true.B

  /** create different kinds of Immediate here */
  def addi4spnImm = Cat(0.U(54.W), x(10,7), x(12,11), x(5), x(6), 0.U(2.W))
  def lwImm = Cat(0.U(57.W), x(5), x(12,10), x(6), 0.U(2.W))
  def ldImm = Cat(0.U(56.W), x(6,5), x(12,10), 0.U(3.W))
  def lwspImm = Cat(0.U(56.W), x(3,2), x(12), x(6,4), 0.U(2.W))
  def ldspImm = Cat(0.U(55.W), x(4,2), x(12), x(6,5), 0.U(3.W))
  def swspImm = Cat(0.U(56.W), x(8,7), x(12,9), 0.U(2.W))
  def sdspImm = Cat(0.U(55.W), x(9,7), x(12,10), 0.U(3.W))
  def luiImm = Cat(Fill(47, x(12)), x(6,2), 0.U(12.W))
  def addi16spImm = Cat(Fill(55, x(12)), x(4,3), x(5), x(2), x(6), 0.U(4.W))
  def addiImm = Cat(Fill(59, x(12)), x(6,2))
  def jImm = Cat(Fill(53, x(12)), x(8), x(10,9), x(6), x(7), x(2), x(11), x(5,3), 0.U(1.W))
  def bImm = Cat(Fill(56, x(12)), x(6,5), x(2), x(11,10), x(4,3), 0.U(1.W))
  def shamtImm = Cat(x(12), x(6,2))


  def c_addi4spn = ( x === BitPat("b????????????????000???????????00") ) & (x(12,5) =/= 0.U)
  def c_lw       = ( x === BitPat("b????????????????010???????????00") )
  def c_ld       = ( x === BitPat("b????????????????011???????????00") )
  def c_sw       = ( x === BitPat("b????????????????110???????????00") )
  def c_sd       = ( x === BitPat("b????????????????111???????????00") )
  def c_nop      = ( x === BitPat("b????????????????0000000000000001") )
  def c_addi     = ( x === BitPat("b????????????????000???????????01") ) & (x(11,7) =/= 0.U)
  def c_addiw    = ( x === BitPat("b????????????????001???????????01") ) & (x(11,7) =/= 0.U)
  def c_li       = ( x === BitPat("b????????????????010???????????01") ) & (x(11,7) =/= 0.U)
  def c_addi16sp = ( x === BitPat("b????????????????011?00010?????01") ) & (x(11,7) === 2.U) & (Cat(x(12), x(6,2)) =/= 0.U)
  def c_lui      = ( x === BitPat("b????????????????011???????????01") ) & (x(11,7) =/= 2.U) & (x(12,2) =/= 0.U)
  def c_srli     = ( x === BitPat("b????????????????100?00????????01") ) & (Cat(x(12), x(6,2)) =/= 0.U)
  def c_srai     = ( x === BitPat("b????????????????100?01????????01") ) & (Cat(x(12), x(6,2)) =/= 0.U)

  def c_andi     = ( x === BitPat("b????????????????100?10????????01") )
  def c_sub      = ( x === BitPat("b????????????????100011???00???01") )
  def c_xor      = ( x === BitPat("b????????????????100011???01???01") )
  def c_or       = ( x === BitPat("b????????????????100011???10???01") )
  def c_and      = ( x === BitPat("b????????????????100011???11???01") )
  def c_subw     = ( x === BitPat("b????????????????100111???00???01") )
  def c_addw     = ( x === BitPat("b????????????????100111???01???01") )
  def c_j        = ( x === BitPat("b????????????????101???????????01") )
  def c_beqz     = ( x === BitPat("b????????????????110???????????01") )
  def c_bnez     = ( x === BitPat("b????????????????111???????????01") )
  def c_slli     = ( x === BitPat("b????????????????000???????????10") ) & (x(11,7) =/= 0.U)
  
  def c_lwsp     = ( x === BitPat("b????????????????010???????????10") ) & (x(11,7) =/= 0.U)
  def c_ldsp     = ( x === BitPat("b????????????????011???????????10") ) & (x(11,7) =/= 0.U)
  def c_jr       = ( x === BitPat("b????????????????1000?????0000010") ) & (x(11,7) =/= 0.U)
  def c_mv       = ( x === BitPat("b????????????????1000??????????10") ) & (x(11,7) =/= 0.U) & (x(6,2) =/= 0.U)
  def c_ebreak   = ( x === BitPat("b????????????????1001000000000010") )
  def c_jalr     = ( x === BitPat("b????????????????1001?????0000010") ) & (x(11,7) =/= 0.U)
  def c_add      = ( x === BitPat("b????????????????1001??????????10") ) & (x(11,7) =/= 0.U) & (x(6,2) =/= 0.U)

  def c_swsp     = ( x === BitPat("b????????????????110???????????10") )
  def c_sdsp     = ( x === BitPat("b????????????????111???????????10") )


  def c_fld      = if( fpuNum > 0 ) ( x === BitPat("b????????????????001???????????00") ) else false.B
  def c_fsd      = if( fpuNum > 0 ) ( x === BitPat("b????????????????101???????????00") ) else false.B
  def c_fldsp    = if( fpuNum > 0 ) ( x === BitPat("b????????????????001???????????10") ) else false.B
  def c_fsdsp    = if( fpuNum > 0 ) ( x === BitPat("b????????????????101???????????10") ) else false.B




  info.param.raw.rd0        :=
    Mux1H( Seq(
      c_addi4spn -> Cat(1.U(2.W), x(4,2)),
      c_fld      -> Cat(1.U(2.W), x(4,2)),
      c_lw       -> Cat(1.U(2.W), x(4,2)),
      c_ld       -> Cat(1.U(2.W), x(4,2)),
      c_fsd      -> 0.U,
      c_sw       -> 0.U,
      c_sd       -> 0.U,
      c_nop      -> 0.U,
      c_addi     -> x(11,7),
      c_addiw    -> x(11,7),
      c_li       -> x(11,7),
      c_addi16sp -> 2.U,
      c_lui      -> x(11,7),
      c_srli     -> Cat(1.U(2.W), x(9,7)),
      c_srai     -> Cat(1.U(2.W), x(9,7)),
      c_andi     -> Cat(1.U(2.W), x(9,7)),
      c_sub      -> Cat(1.U(2.W), x(9,7)),
      c_xor      -> Cat(1.U(2.W), x(9,7)),
      c_or       -> Cat(1.U(2.W), x(9,7)),
      c_and      -> Cat(1.U(2.W), x(9,7)),
      c_subw     -> Cat(1.U(2.W), x(9,7)),
      c_addw     -> Cat(1.U(2.W), x(9,7)),
      c_j        -> 0.U,
      c_beqz     -> 0.U,
      c_bnez     -> 0.U,
      c_slli     -> x(11,7),
      c_fldsp    -> x(11,7),
      c_lwsp     -> x(11,7),
      c_ldsp     -> x(11,7),
      c_jr       -> 0.U,
      c_mv       -> x(11,7),
      c_ebreak   -> 0.U,
      c_jalr     -> 1.U,
      c_add      -> x(11,7),
      c_fsdsp    -> 0.U,
      c_swsp     -> 0.U,
      c_sdsp     -> 0.U
      )
    )


  
  
  info.param.raw.vm0 := 0.U
  info.param.raw.rs1        := 
    Mux1H( Seq(

      c_addi4spn -> 2.U,
      c_fld      -> Cat(1.U(2.W), x(9,7)),
      c_lw       -> Cat(1.U(2.W), x(9,7)),
      c_ld       -> Cat(1.U(2.W), x(9,7)),
      c_fsd      -> Cat(1.U(2.W), x(9,7)),
      c_sw       -> Cat(1.U(2.W), x(9,7)),
      c_sd       -> Cat(1.U(2.W), x(9,7)),
      c_nop      -> 0.U,
      c_addi     -> x(11,7),
      c_addiw    -> x(11,7),
      c_li       -> 0.U,
      c_addi16sp -> 2.U,
      c_lui      -> 0.U,
      c_srli     -> Cat(1.U(2.W), x(9,7)),
      c_srai     -> Cat(1.U(2.W), x(9,7)),
      c_andi     -> Cat(1.U(2.W), x(9,7)),
      c_sub      -> Cat(1.U(2.W), x(9,7)),
      c_xor      -> Cat(1.U(2.W), x(9,7)),
      c_or       -> Cat(1.U(2.W), x(9,7)),
      c_and      -> Cat(1.U(2.W), x(9,7)),
      c_subw     -> Cat(1.U(2.W), x(9,7)),
      c_addw     -> Cat(1.U(2.W), x(9,7)),
      c_j        -> 0.U,
      c_beqz     -> Cat(1.U(2.W), x(9,7)),
      c_bnez     -> Cat(1.U(2.W), x(9,7)),
      c_slli     -> x(11,7),
      c_fldsp    -> 2.U,
      c_lwsp     -> 2.U,
      c_ldsp     -> 2.U,
      c_jr       -> x(11,7),
      c_mv       -> 0.U,
      c_ebreak   -> 0.U,
      c_jalr     -> x(11,7),
      c_add      -> x(11,7),
      c_fsdsp    -> 2.U,
      c_swsp     -> 2.U,
      c_sdsp     -> 2.U

      )
    )


  info.param.raw.rs2       := 
    Mux1H( Seq(
      c_addi4spn -> 0.U,
      c_fld      -> 0.U,
      c_lw       -> 0.U,
      c_ld       -> 0.U,
      c_fsd      -> Cat(1.U(2.W), x(4,2)),
      c_sw       -> Cat(1.U(2.W), x(4,2)),
      c_sd       -> Cat(1.U(2.W), x(4,2)),
      c_nop      -> 0.U,
      c_addi     -> 0.U,
      c_addiw    -> 0.U,
      c_li       -> 0.U,
      c_addi16sp -> 0.U,
      c_lui      -> 0.U,
      c_srli     -> 0.U,
      c_srai     -> 0.U,
      c_andi     -> 0.U,
      c_sub      -> Cat(1.U(2.W), x(4,2)),
      c_xor      -> Cat(1.U(2.W), x(4,2)),
      c_or       -> Cat(1.U(2.W), x(4,2)),
      c_and      -> Cat(1.U(2.W), x(4,2)),
      c_subw     -> Cat(1.U(2.W), x(4,2)),
      c_addw     -> Cat(1.U(2.W), x(4,2)),
      c_j        -> 0.U,
      c_beqz     -> 0.U,
      c_bnez     -> 0.U,
      c_slli     -> 0.U,
      c_fldsp    -> 0.U,
      c_lwsp     -> 0.U,
      c_ldsp     -> 0.U,
      c_jr       -> 0.U,
      c_mv       -> x(6,2),
      c_ebreak   -> 0.U,
      c_jalr     -> 0.U,
      c_add      -> x(6,2),
      c_fsdsp    -> x(6,2),
      c_swsp     -> x(6,2),
      c_sdsp     -> x(6,2)
      )
    )
  
  info.param.raw.rs3       := 0.U

  info.param.imm        :=
    Mux1H( Seq(
        c_addi4spn -> addi4spnImm,
        c_fld -> ldImm,
        c_lw -> lwImm,
        c_ld -> ldImm,
        c_fsd -> ldImm,
        c_sw -> lwImm,
        c_sd -> ldImm,
        c_addi -> addiImm,
        c_addiw -> addiImm,
        c_li ->  addiImm,
        c_addi16sp -> addi16spImm,
        c_lui -> luiImm,
        c_andi ->addiImm,
        c_j -> jImm,
        c_beqz -> bImm,
        c_bnez -> bImm,
        c_fldsp -> ldspImm,
        c_lwsp -> lwspImm,
        c_ldsp -> ldspImm,
        c_fsdsp -> sdspImm,
        c_swsp -> swspImm,
        c_sdsp -> sdspImm,
        c_slli -> shamtImm,
        c_srli -> shamtImm,
        c_srai -> shamtImm

      )
    )

  info.param.rm := Mux( info.fpuIsa.is_fpu, x(14,12), 0.U )



  info.aluIsa.lui         := c_lui
  info.aluIsa.auipc       := false.B
  info.aluIsa.addi        := c_addi4spn | c_nop | c_addi | c_li | c_addi16sp
  info.aluIsa.addiw       := c_addiw
  info.aluIsa.slti        := false.B
  info.aluIsa.sltiu       := false.B
  info.aluIsa.xori        := false.B
  info.aluIsa.ori         := false.B
  info.aluIsa.andi        := c_andi
  info.aluIsa.slli        := c_slli
  info.aluIsa.slliw       := false.B
  info.aluIsa.srli        := c_srli
  info.aluIsa.srliw       := false.B
  info.aluIsa.srai        := c_srai
  info.aluIsa.sraiw       := false.B
  info.aluIsa.add         := c_mv | c_add
  info.aluIsa.addw        := c_addw
  info.aluIsa.sub         := c_sub
  info.aluIsa.subw        := c_subw
  info.aluIsa.sll         := false.B
  info.aluIsa.sllw        := false.B
  info.aluIsa.slt         := false.B
  info.aluIsa.sltu        := false.B
  info.aluIsa.xor         := c_xor
  info.aluIsa.srl         := false.B
  info.aluIsa.srlw        := false.B
  info.aluIsa.sra         := false.B
  info.aluIsa.sraw        := false.B
  info.aluIsa.or          := c_or
  info.aluIsa.and         := c_and
  info.aluIsa.wfi         := false.B
  
  info.bruIsa.jal         := c_j
  info.bruIsa.jalr        := c_jr | c_jalr
  info.bruIsa.beq         := c_beqz
  info.bruIsa.bne         := c_bnez
  info.bruIsa.blt         := false.B
  info.bruIsa.bge         := false.B
  info.bruIsa.bltu        := false.B
  info.bruIsa.bgeu        := false.B

  info.lsuIsa.lb          := false.B
  info.lsuIsa.lh          := false.B
  info.lsuIsa.lw          := c_lw | c_lwsp
  info.lsuIsa.ld          := c_ld | c_ldsp
  info.lsuIsa.lbu         := false.B
  info.lsuIsa.lhu         := false.B
  info.lsuIsa.lwu         := false.B
  info.lsuIsa.sb          := false.B
  info.lsuIsa.sh          := false.B
  info.lsuIsa.sw          := c_sw | c_swsp
  info.lsuIsa.sd          := c_sd | c_sdsp
  info.lsuIsa.fence       := false.B
  info.lsuIsa.fence_i     := false.B
  info.lsuIsa.sfence_vma  := false.B

  info.csrIsa.rw          := false.B
  info.csrIsa.rs          := false.B
  info.csrIsa.rc          := false.B
  info.csrIsa.rwi         := false.B
  info.csrIsa.rsi         := false.B
  info.csrIsa.rci         := false.B

  info.mulIsa.mul         := false.B
  info.mulIsa.mulh        := false.B
  info.mulIsa.mulhsu      := false.B
  info.mulIsa.mulhu       := false.B
  info.mulIsa.div         := false.B
  info.mulIsa.divu        := false.B
  info.mulIsa.rem         := false.B
  info.mulIsa.remu        := false.B
  info.mulIsa.mulw        := false.B
  info.mulIsa.divw        := false.B
  info.mulIsa.divuw       := false.B
  info.mulIsa.remw        := false.B
  info.mulIsa.remuw       := false.B

  info.privil_isa.ecall       := false.B
  info.privil_isa.ebreak      := c_ebreak
  info.privil_isa.mret        := false.B
  info.privil_isa.uret        := false.B
  info.privil_isa.sret        := false.B
  info.privil_isa.dret        := false.B



  info.privil_isa.hfence_vvma := false.B
  info.privil_isa.hfence_gvma := false.B

  info.privil_isa.hlv_b       := false.B
  info.privil_isa.hlv_bu      := false.B
  info.privil_isa.hlv_h       := false.B
  info.privil_isa.hlv_hu      := false.B
  info.privil_isa.hlvx_hu     := false.B
  info.privil_isa.hlv_w       := false.B
  info.privil_isa.hlvx_wu     := false.B
  info.privil_isa.hsv_b       := false.B
  info.privil_isa.hsv_h       := false.B
  info.privil_isa.hsv_w       := false.B
  info.privil_isa.hlv_wu      := false.B
  info.privil_isa.hlv_d       := false.B
  info.privil_isa.hsv_d       := false.B

  info.lsuIsa.lr_w        := false.B
  info.lsuIsa.sc_w        := false.B
  info.lsuIsa.amoswap_w   := false.B
  info.lsuIsa.amoadd_w    := false.B
  info.lsuIsa.amoxor_w    := false.B
  info.lsuIsa.amoand_w    := false.B
  info.lsuIsa.amoor_w     := false.B
  info.lsuIsa.amomin_w    := false.B
  info.lsuIsa.amomax_w    := false.B
  info.lsuIsa.amominu_w   := false.B
  info.lsuIsa.amomaxu_w   := false.B
  info.lsuIsa.lr_d        := false.B
  info.lsuIsa.sc_d        := false.B
  info.lsuIsa.amoswap_d   := false.B
  info.lsuIsa.amoadd_d    := false.B
  info.lsuIsa.amoxor_d    := false.B
  info.lsuIsa.amoand_d    := false.B
  info.lsuIsa.amoor_d     := false.B
  info.lsuIsa.amomin_d    := false.B
  info.lsuIsa.amomax_d    := false.B
  info.lsuIsa.amominu_d   := false.B
  info.lsuIsa.amomaxu_d   := false.B

  info.lsuIsa.flw         := false.B
  info.lsuIsa.fsw         := false.B
  info.fpuIsa.fmadd_s     := false.B
  info.fpuIsa.fmsub_s     := false.B
  info.fpuIsa.fnmsub_s    := false.B
  info.fpuIsa.fnmadd_s    := false.B
  info.fpuIsa.fadd_s      := false.B
  info.fpuIsa.fsub_s      := false.B
  info.fpuIsa.fmul_s      := false.B
  info.fpuIsa.fdiv_s      := false.B
  info.fpuIsa.fsqrt_s     := false.B
  info.fpuIsa.fsgnj_s     := false.B
  info.fpuIsa.fsgnjn_s    := false.B
  info.fpuIsa.fsgnjx_s    := false.B
  info.fpuIsa.fmin_s      := false.B
  info.fpuIsa.fmax_s      := false.B
  info.fpuIsa.fcvt_w_s    := false.B
  info.fpuIsa.fcvt_wu_s   := false.B
  info.fpuIsa.fmv_x_w     := false.B
  info.fpuIsa.feq_s       := false.B
  info.fpuIsa.flt_s       := false.B
  info.fpuIsa.fle_s       := false.B
  info.fpuIsa.fclass_s    := false.B
  info.fpuIsa.fcvt_s_w    := false.B
  info.fpuIsa.fcvt_s_wu   := false.B
  info.fpuIsa.fmv_w_x     := false.B
  info.fpuIsa.fcvt_l_s    := false.B
  info.fpuIsa.fcvt_lu_s   := false.B
  info.fpuIsa.fcvt_s_l    := false.B
  info.fpuIsa.fcvt_s_lu   := false.B

  info.lsuIsa.fld         := c_fld | c_fldsp
  info.lsuIsa.fsd         := c_fsd | c_fsdsp
  info.fpuIsa.fmadd_d     := false.B
  info.fpuIsa.fmsub_d     := false.B
  info.fpuIsa.fnmsub_d    := false.B
  info.fpuIsa.fnmadd_d    := false.B
  info.fpuIsa.fadd_d      := false.B
  info.fpuIsa.fsub_d      := false.B
  info.fpuIsa.fmul_d      := false.B
  info.fpuIsa.fdiv_d      := false.B
  info.fpuIsa.fsqrt_d     := false.B
  info.fpuIsa.fsgnj_d     := false.B
  info.fpuIsa.fsgnjn_d    := false.B
  info.fpuIsa.fsgnjx_d    := false.B
  info.fpuIsa.fmin_d      := false.B
  info.fpuIsa.fmax_d      := false.B
  info.fpuIsa.fcvt_s_d    := false.B
  info.fpuIsa.fcvt_d_s    := false.B
  info.fpuIsa.feq_d       := false.B
  info.fpuIsa.flt_d       := false.B
  info.fpuIsa.fle_d       := false.B
  info.fpuIsa.fclass_d    := false.B
  info.fpuIsa.fcvt_w_d    := false.B
  info.fpuIsa.fcvt_wu_d   := false.B
  info.fpuIsa.fcvt_d_w    := false.B
  info.fpuIsa.fcvt_d_wu   := false.B
  info.fpuIsa.fcvt_l_d    := false.B
  info.fpuIsa.fcvt_lu_d   := false.B
  info.fpuIsa.fmv_x_d     := false.B
  info.fpuIsa.fcvt_d_l    := false.B
  info.fpuIsa.fcvt_d_lu   := false.B
  info.fpuIsa.fmv_d_x     := false.B
  // info.fpuIsa.fcsr_rw     := false.B
  // info.fpuIsa.fcsr_rs     := false.B
  // info.fpuIsa.fcsr_rc     := false.B
  // info.fpuIsa.fcsr_rwi    := false.B
  // info.fpuIsa.fcsr_rsi    := false.B
  // info.fpuIsa.fcsr_rci    := false.B

  info.privil_isa.is_access_fault := ( x === BitPat("b1001110001000001") )
  info.privil_isa.is_paging_fault := ( x === BitPat("b1001110001000101") )

} 

/** This object provides a method to instantiate a RISC-V 16-bit instruction decoder*/
object Decode16 {
  /**
    * Decodes the 16-bit instruction using the given input signals and parameters.
    * @param x The 16-bit instruction to decode, taken as input of type UInt.
    * @param pc The current program counter, taken as input of type UInt.
    * @param hasFpu A boolean flag indicating whether the instruction includes a floating point unit.
    * @return Returns the decoded instruction as an instance of the Info_instruction class.
    */
  def apply(x:UInt, pc: UInt, hasFpu: Boolean)(implicit p: Parameters): Info_instruction = {
    // val info = Wire(new Info_instruction)
    val dec16 = Module(new Decode16 with NVDecode)
    dec16.io.x := x
    dec16.io.pc := pc
    return dec16.io.info
  }
}

trait Decode32G { this: DecodeBase =>

    def iType_imm = Cat( Fill(52, x(31)), x(31,20))
    def sType_imm = Cat( Fill(52, x(31)), x(31,25), x(11,7) )
    def bType_imm = Cat( Fill(52, x(31)), x(7), x(30,25), x(11,8), 0.U(1.W))
    def uType_imm = Cat( Fill(32, x(31)), x(31,12), 0.U(12.W))
    def jType_imm = Cat( Fill(44, x(31)), x(19,12), x(20), x(30,21), 0.U(1.W))
    def aType_imm = Cat( Fill(62, 0.U), x(26,25))
    def mType_imm = Cat( Fill(61, 0.U), x(14,12))
    def shamt_imm = Cat( Fill(58, 0.U), x(25,20) )


    def is_iType = info.bruIsa.jalr | info.lsuIsa.lb | info.lsuIsa.lh |info.lsuIsa.lw | info.lsuIsa.lbu | info.lsuIsa.lhu | info.lsuIsa.lwu | info.lsuIsa.ld | info.aluIsa.addi | info.aluIsa.addiw | info.aluIsa.slti | info.aluIsa.sltiu | info.aluIsa.xori | info.aluIsa.ori | info.aluIsa.andi | info.lsuIsa.fence | info.lsuIsa.fence_i | info.csrIsa.is_csr | info.lsuIsa.flw | info.lsuIsa.fld  | info.vecIsa.isVector //| info.fpuIsa.is_fun_fcsr
    def is_sType = info.lsuIsa.sb | info.lsuIsa.sh | info.lsuIsa.sw | info.lsuIsa.sd | info.lsuIsa.fsw | info.lsuIsa.fsd 
    def is_bType = info.bruIsa.beq | info.bruIsa.bne | info.bruIsa.blt | info.bruIsa.bge | info.bruIsa.bltu | info.bruIsa.bgeu;
    def is_uType = info.aluIsa.lui | info.aluIsa.auipc;
    def is_jType = info.bruIsa.jal;
    def is_aType = info.lsuIsa.is_lrsc | info.lsuIsa.is_amo
    def is_mType = info.fpuIsa.is_fpu //& ~(info.fpuIsa.is_fun_fcsr)
    def is_shamt = info.aluIsa.slli | info.aluIsa.srli | info.aluIsa.srai | info.aluIsa.slliw | info.aluIsa.srliw | info.aluIsa.sraiw



    info.param.imm := Mux1H( Seq(
        is_iType -> iType_imm,
        is_sType -> sType_imm,
        is_bType -> bType_imm,
        is_uType -> uType_imm,
        is_jType -> jType_imm,
        is_aType -> aType_imm,
        is_mType -> mType_imm,
        is_shamt -> shamt_imm
      )
    )





    info.param.raw.rd0 := MuxCase( x(11,7), Seq(
      info.aluIsa.wfi         -> 0.U,
      info.bruIsa.beq         -> 0.U,
      info.bruIsa.bne         -> 0.U,
      info.bruIsa.blt         -> 0.U,
      info.bruIsa.bge         -> 0.U,
      info.bruIsa.bltu        -> 0.U,
      info.bruIsa.bgeu        -> 0.U,
      info.lsuIsa.sb          -> 0.U,
      info.lsuIsa.sh          -> 0.U,
      info.lsuIsa.sw          -> 0.U,
      info.lsuIsa.sd          -> 0.U,
      info.lsuIsa.fsw         -> 0.U,
      info.lsuIsa.fsd         -> 0.U,
      info.lsuIsa.sfence_vma  -> 0.U,

      info.lsuIsa.isVStore    -> 0.U,
      
      info.privil_isa.ecall       -> 0.U,
      info.privil_isa.ebreak      -> 0.U,
      info.privil_isa.mret        -> 0.U,
      info.privil_isa.uret        -> 0.U,
      info.privil_isa.sret        -> 0.U,
      info.privil_isa.dret        -> 0.U,
      info.privil_isa.hfence_vvma -> 0.U,
      info.privil_isa.hfence_gvma -> 0.U,
      info.privil_isa.hsv_b       -> 0.U,
      info.privil_isa.hsv_h       -> 0.U,
      info.privil_isa.hsv_w       -> 0.U,
      info.privil_isa.hsv_d       -> 0.U,
  

    ))



    info.param.raw.vm0 := 0.U

    info.param.raw.rs1 := MuxCase( x(19,15), Seq(
      info.aluIsa.lui       -> 0.U,
      info.aluIsa.auipc     -> 0.U,
      info.aluIsa.wfi       -> 0.U,
      info.bruIsa.jal       -> 0.U,
      info.privil_isa.ecall  -> 0.U,
      info.privil_isa.ebreak -> 0.U,
      info.privil_isa.uret   -> 0.U,
      info.privil_isa.sret   -> 0.U,
      info.privil_isa.mret   -> 0.U,
      info.privil_isa.dret   -> 0.U,
    ))
      

    info.param.raw.rs2 := MuxCase( x(24,20), Seq(
      info.aluIsa.lui   -> 0.U,
      info.aluIsa.auipc -> 0.U,
      info.aluIsa.addi  -> 0.U,
      info.aluIsa.slti  -> 0.U,
      info.aluIsa.sltiu -> 0.U,
      info.aluIsa.xori  -> 0.U,
      info.aluIsa.ori   -> 0.U,
      info.aluIsa.andi  -> 0.U,
      info.aluIsa.slli  -> 0.U,
      info.aluIsa.srli  -> 0.U,
      info.aluIsa.srai  -> 0.U,
      info.aluIsa.slli  -> 0.U,
      info.aluIsa.srli  -> 0.U,
      info.aluIsa.srai  -> 0.U,
      info.aluIsa.addiw -> 0.U,
      info.aluIsa.slliw -> 0.U,
      info.aluIsa.srliw -> 0.U,
      info.aluIsa.sraiw -> 0.U,
      info.aluIsa.wfi   -> 0.U,
      info.bruIsa.jal   -> 0.U,
      info.bruIsa.jalr  -> 0.U,

      info.csrIsa.isXCSR  -> 0.U,
      info.csrIsa.vsetvli  -> 0.U,
      info.csrIsa.vsetivli -> 0.U,

      info.lsuIsa.lb    -> 0.U,
      info.lsuIsa.lh    -> 0.U,
      info.lsuIsa.lw    -> 0.U,
      info.lsuIsa.lbu   -> 0.U,
      info.lsuIsa.lhu   -> 0.U,
      info.lsuIsa.lwu   -> 0.U,
      info.lsuIsa.ld    -> 0.U,
      info.lsuIsa.fence -> 0.U,
      info.lsuIsa.fence_i -> 0.U,
      info.lsuIsa.lr_w  -> 0.U,
      info.lsuIsa.lr_d  -> 0.U,
      info.lsuIsa.flw       -> 0.U,
      info.lsuIsa.fld       -> 0.U,
      info.lsuIsa.isUnitStride -> 0.U,

      info.privil_isa.ecall  -> 0.U,
      info.privil_isa.ebreak -> 0.U,
      info.privil_isa.uret   -> 0.U,
      info.privil_isa.sret   -> 0.U,
      info.privil_isa.mret   -> 0.U,
      info.privil_isa.dret   -> 0.U,

      info.fpuIsa.fsqrt_s   -> 0.U,
      info.fpuIsa.fcvt_w_s  -> 0.U,
      info.fpuIsa.fcvt_wu_s -> 0.U,
      info.fpuIsa.fmv_x_w   -> 0.U,
      info.fpuIsa.fclass_s  -> 0.U,
      info.fpuIsa.fcvt_s_w  -> 0.U,
      info.fpuIsa.fcvt_s_wu -> 0.U,
      info.fpuIsa.fmv_w_x   -> 0.U,
      info.fpuIsa.fcvt_l_s  -> 0.U,
      info.fpuIsa.fcvt_lu_s -> 0.U,
      info.fpuIsa.fcvt_s_l  -> 0.U,
      info.fpuIsa.fcvt_s_lu -> 0.U,
      info.fpuIsa.fsqrt_d   -> 0.U,
      info.fpuIsa.fcvt_s_d  -> 0.U,
      info.fpuIsa.fcvt_d_s  -> 0.U,
      info.fpuIsa.fclass_d  -> 0.U,
      info.fpuIsa.fcvt_w_d  -> 0.U,
      info.fpuIsa.fcvt_wu_d -> 0.U,
      info.fpuIsa.fcvt_d_w  -> 0.U,
      info.fpuIsa.fcvt_d_wu -> 0.U,
      info.fpuIsa.fcvt_l_d  -> 0.U,
      info.fpuIsa.fcvt_lu_d -> 0.U,
      info.fpuIsa.fmv_x_d   -> 0.U,
      info.fpuIsa.fcvt_d_l  -> 0.U,
      info.fpuIsa.fcvt_d_lu -> 0.U,
      info.fpuIsa.fmv_d_x   -> 0.U,


      info.privil_isa.hlv_b   -> 0.U,
      info.privil_isa.hlv_bu  -> 0.U,
      info.privil_isa.hlv_h   -> 0.U,
      info.privil_isa.hlv_hu  -> 0.U,
      info.privil_isa.hlvx_hu -> 0.U,
      info.privil_isa.hlv_w   -> 0.U,
      info.privil_isa.hlvx_wu -> 0.U,
      info.privil_isa.hlv_wu  -> 0.U,
      info.privil_isa.hlv_d   -> 0.U,
    ))



    info.param.raw.rs3 := Mux1H( Seq(
      info.fpuIsa.fmadd_s  -> x(31,27),
      info.fpuIsa.fmsub_s  -> x(31,27),
      info.fpuIsa.fnmsub_s -> x(31,27),
      info.fpuIsa.fnmadd_s -> x(31,27),
      info.fpuIsa.fmadd_d  -> x(31,27),
      info.fpuIsa.fmsub_d  -> x(31,27),
      info.fpuIsa.fnmsub_d -> x(31,27),
      info.fpuIsa.fnmadd_d -> x(31,27),
      
      info.lsuIsa.isVStore -> x(11,7),
    ))


    info.param.rm := x(14,12)

    info.aluIsa.lui         := ( x === BitPat("b?????????????????????????0110111") )
    info.aluIsa.auipc       := ( x === BitPat("b?????????????????????????0010111") )
    info.aluIsa.addi        := ( x === BitPat("b?????????????????000?????0010011") )
    info.aluIsa.addiw       := ( x === BitPat("b?????????????????000?????0011011") )
    info.aluIsa.slti        := ( x === BitPat("b?????????????????010?????0010011") )
    info.aluIsa.sltiu       := ( x === BitPat("b?????????????????011?????0010011") )
    info.aluIsa.xori        := ( x === BitPat("b?????????????????100?????0010011") )
    info.aluIsa.ori         := ( x === BitPat("b?????????????????110?????0010011") )
    info.aluIsa.andi        := ( x === BitPat("b?????????????????111?????0010011") )
    info.aluIsa.slli        := ( x === BitPat("b000000???????????001?????0010011") )
    info.aluIsa.slliw       := ( x === BitPat("b0000000??????????001?????0011011") )
    info.aluIsa.srli        := ( x === BitPat("b000000???????????101?????0010011") )
    info.aluIsa.srliw       := ( x === BitPat("b0000000??????????101?????0011011") )
    info.aluIsa.srai        := ( x === BitPat("b010000???????????101?????0010011") )
    info.aluIsa.sraiw       := ( x === BitPat("b0100000??????????101?????0011011") )
    info.aluIsa.add         := ( x === BitPat("b0000000??????????000?????0110011") )
    info.aluIsa.addw        := ( x === BitPat("b0000000??????????000?????0111011") )
    info.aluIsa.sub         := ( x === BitPat("b0100000??????????000?????0110011") )
    info.aluIsa.subw        := ( x === BitPat("b0100000??????????000?????0111011") )
    info.aluIsa.sll         := ( x === BitPat("b0000000??????????001?????0110011") )
    info.aluIsa.sllw        := ( x === BitPat("b0000000??????????001?????0111011") )
    info.aluIsa.slt         := ( x === BitPat("b0000000??????????010?????0110011") )
    info.aluIsa.sltu        := ( x === BitPat("b0000000??????????011?????0110011") )
    info.aluIsa.xor         := ( x === BitPat("b0000000??????????100?????0110011") )
    info.aluIsa.srl         := ( x === BitPat("b0000000??????????101?????0110011") )
    info.aluIsa.srlw        := ( x === BitPat("b0000000??????????101?????0111011") )
    info.aluIsa.sra         := ( x === BitPat("b0100000??????????101?????0110011") )
    info.aluIsa.sraw        := ( x === BitPat("b0100000??????????101?????0111011") )
    info.aluIsa.or          := ( x === BitPat("b0000000??????????110?????0110011") )
    info.aluIsa.and         := ( x === BitPat("b0000000??????????111?????0110011") )
    info.aluIsa.wfi         := ( x === BitPat("b00010000010100000000000001110011") )

    info.bruIsa.jal         := ( x === BitPat("b?????????????????????????1101111") )
    info.bruIsa.jalr        := ( x === BitPat("b?????????????????000?????1100111") )
    info.bruIsa.beq         := ( x === BitPat("b?????????????????000?????1100011") )
    info.bruIsa.bne         := ( x === BitPat("b?????????????????001?????1100011") )
    info.bruIsa.blt         := ( x === BitPat("b?????????????????100?????1100011") )
    info.bruIsa.bge         := ( x === BitPat("b?????????????????101?????1100011") )
    info.bruIsa.bltu        := ( x === BitPat("b?????????????????110?????1100011") )
    info.bruIsa.bgeu        := ( x === BitPat("b?????????????????111?????1100011") )

    info.lsuIsa.lb          := ( x === BitPat("b?????????????????000?????0000011") )
    info.lsuIsa.lh          := ( x === BitPat("b?????????????????001?????0000011") )
    info.lsuIsa.lw          := ( x === BitPat("b?????????????????010?????0000011") )
    info.lsuIsa.ld          := ( x === BitPat("b?????????????????011?????0000011") )
    info.lsuIsa.lbu         := ( x === BitPat("b?????????????????100?????0000011") )
    info.lsuIsa.lhu         := ( x === BitPat("b?????????????????101?????0000011") )
    info.lsuIsa.lwu         := ( x === BitPat("b?????????????????110?????0000011") )
    info.lsuIsa.sb          := ( x === BitPat("b?????????????????000?????0100011") )
    info.lsuIsa.sh          := ( x === BitPat("b?????????????????001?????0100011") )
    info.lsuIsa.sw          := ( x === BitPat("b?????????????????010?????0100011") )
    info.lsuIsa.sd          := ( x === BitPat("b?????????????????011?????0100011") )
    info.lsuIsa.fence       := ( x === BitPat("b?????????????????000?????0001111") )
    info.lsuIsa.fence_i     := ( x === BitPat("b?????????????????001?????0001111") )
    info.lsuIsa.sfence_vma  := ( x === BitPat("b0001001??????????000000001110011") )

    info.csrIsa.rw          := ( x === BitPat("b?????????????????001?????1110011") )// & ( x(31,20) =/= 1.U & x(31,20) =/= 2.U & x(31,20) =/= 3.U ) 
    info.csrIsa.rs          := ( x === BitPat("b?????????????????010?????1110011") )// & ( x(31,20) =/= 1.U & x(31,20) =/= 2.U & x(31,20) =/= 3.U ) 
    info.csrIsa.rc          := ( x === BitPat("b?????????????????011?????1110011") )// & ( x(31,20) =/= 1.U & x(31,20) =/= 2.U & x(31,20) =/= 3.U ) 
    info.csrIsa.rwi         := ( x === BitPat("b?????????????????101?????1110011") )// & ( x(31,20) =/= 1.U & x(31,20) =/= 2.U & x(31,20) =/= 3.U ) 
    info.csrIsa.rsi         := ( x === BitPat("b?????????????????110?????1110011") )// & ( x(31,20) =/= 1.U & x(31,20) =/= 2.U & x(31,20) =/= 3.U ) 
    info.csrIsa.rci         := ( x === BitPat("b?????????????????111?????1110011") )// & ( x(31,20) =/= 1.U & x(31,20) =/= 2.U & x(31,20) =/= 3.U ) 

    // if( fpuNum > 0 ) {
    //   info.fpuIsa.fcsr_rw   := ( x === BitPat("b?????????????????001?????1110011") ) & ( x(31,20) === 1.U | x(31,20) === 2.U | x(31,20) === 3.U ) 
    //   info.fpuIsa.fcsr_rs   := ( x === BitPat("b?????????????????010?????1110011") ) & ( x(31,20) === 1.U | x(31,20) === 2.U | x(31,20) === 3.U ) 
    //   info.fpuIsa.fcsr_rc   := ( x === BitPat("b?????????????????011?????1110011") ) & ( x(31,20) === 1.U | x(31,20) === 2.U | x(31,20) === 3.U ) 
    //   info.fpuIsa.fcsr_rwi  := ( x === BitPat("b?????????????????101?????1110011") ) & ( x(31,20) === 1.U | x(31,20) === 2.U | x(31,20) === 3.U ) 
    //   info.fpuIsa.fcsr_rsi  := ( x === BitPat("b?????????????????110?????1110011") ) & ( x(31,20) === 1.U | x(31,20) === 2.U | x(31,20) === 3.U ) 
    //   info.fpuIsa.fcsr_rci  := ( x === BitPat("b?????????????????111?????1110011") ) & ( x(31,20) === 1.U | x(31,20) === 2.U | x(31,20) === 3.U )       
    // } else {
    //   info.fpuIsa.fcsr_rw   := false.B
    //   info.fpuIsa.fcsr_rs   := false.B
    //   info.fpuIsa.fcsr_rc   := false.B
    //   info.fpuIsa.fcsr_rwi  := false.B
    //   info.fpuIsa.fcsr_rsi  := false.B
    //   info.fpuIsa.fcsr_rci  := false.B
    // }


    info.mulIsa.mul         := ( x === BitPat("b0000001??????????000?????0110011") )
    info.mulIsa.mulh        := ( x === BitPat("b0000001??????????001?????0110011") )
    info.mulIsa.mulhsu      := ( x === BitPat("b0000001??????????010?????0110011") )
    info.mulIsa.mulhu       := ( x === BitPat("b0000001??????????011?????0110011") )
    info.mulIsa.div         := ( x === BitPat("b0000001??????????100?????0110011") )
    info.mulIsa.divu        := ( x === BitPat("b0000001??????????101?????0110011") )
    info.mulIsa.rem         := ( x === BitPat("b0000001??????????110?????0110011") )
    info.mulIsa.remu        := ( x === BitPat("b0000001??????????111?????0110011") )
    info.mulIsa.mulw        := ( x === BitPat("b0000001??????????000?????0111011") )
    info.mulIsa.divw        := ( x === BitPat("b0000001??????????100?????0111011") )
    info.mulIsa.divuw       := ( x === BitPat("b0000001??????????101?????0111011") )
    info.mulIsa.remw        := ( x === BitPat("b0000001??????????110?????0111011") )
    info.mulIsa.remuw       := ( x === BitPat("b0000001??????????111?????0111011") )

    info.privil_isa.ecall       := ( x === BitPat("b00000000000000000000000001110011") )
    info.privil_isa.ebreak      := ( x === BitPat("b00000000000100000000000001110011") )
    info.privil_isa.mret        := ( x === BitPat("b00110000001000000000000001110011") )
    info.privil_isa.uret        := ( x === BitPat("b00000000001000000000000001110011") )
    info.privil_isa.sret        := ( x === BitPat("b00010000001000000000000001110011") )
    info.privil_isa.dret        := ( x === BitPat("b01111011001000000000000001110011") )



    info.privil_isa.hfence_vvma := ( x === BitPat("b0010001??????????000000001110011") )
    info.privil_isa.hfence_gvma := ( x === BitPat("b0110001??????????000000001110011") )

    info.privil_isa.hlv_b       := ( x === BitPat("b0110000 00000 ????? 100 ????? 1110011") )
    info.privil_isa.hlv_bu      := ( x === BitPat("b0110000 00001 ????? 100 ????? 1110011") )
    info.privil_isa.hlv_h       := ( x === BitPat("b0110010 00000 ????? 100 ????? 1110011") )
    info.privil_isa.hlv_hu      := ( x === BitPat("b0110010 00001 ????? 100 ????? 1110011") )
    info.privil_isa.hlvx_hu     := ( x === BitPat("b0110010 00011 ????? 100 ????? 1110011") )
    info.privil_isa.hlv_w       := ( x === BitPat("b0110100 00000 ????? 100 ????? 1110011") )
    info.privil_isa.hlvx_wu     := ( x === BitPat("b0110100 00011 ????? 100 ????? 1110011") )
    info.privil_isa.hsv_b       := ( x === BitPat("b0110001 ????? ????? 100 00000 1110011") )
    info.privil_isa.hsv_h       := ( x === BitPat("b0110011 ????? ????? 100 00000 1110011") )
    info.privil_isa.hsv_w       := ( x === BitPat("b0110101 ????? ????? 100 00000 1110011") )
    info.privil_isa.hlv_wu      := ( x === BitPat("b0110100 00001 ????? 100 ????? 1110011") )
    info.privil_isa.hlv_d       := ( x === BitPat("b0110110 00000 ????? 100 ????? 1110011") )
    info.privil_isa.hsv_d       := ( x === BitPat("b0110111 ????? ????? 100 00000 1110011") )

    info.lsuIsa.lr_w        := ( x === BitPat("b00010??00000?????010?????0101111") )
    info.lsuIsa.sc_w        := ( x === BitPat("b00011????????????010?????0101111") )
    info.lsuIsa.amoswap_w   := ( x === BitPat("b00001????????????010?????0101111") )
    info.lsuIsa.amoadd_w    := ( x === BitPat("b00000????????????010?????0101111") )
    info.lsuIsa.amoxor_w    := ( x === BitPat("b00100????????????010?????0101111") )
    info.lsuIsa.amoand_w    := ( x === BitPat("b01100????????????010?????0101111") )
    info.lsuIsa.amoor_w     := ( x === BitPat("b01000????????????010?????0101111") )
    info.lsuIsa.amomin_w    := ( x === BitPat("b10000????????????010?????0101111") )
    info.lsuIsa.amomax_w    := ( x === BitPat("b10100????????????010?????0101111") )
    info.lsuIsa.amominu_w   := ( x === BitPat("b11000????????????010?????0101111") )
    info.lsuIsa.amomaxu_w   := ( x === BitPat("b11100????????????010?????0101111") )
    info.lsuIsa.lr_d        := ( x === BitPat("b00010??00000?????011?????0101111") )
    info.lsuIsa.sc_d        := ( x === BitPat("b00011????????????011?????0101111") )
    info.lsuIsa.amoswap_d   := ( x === BitPat("b00001????????????011?????0101111") )
    info.lsuIsa.amoadd_d    := ( x === BitPat("b00000????????????011?????0101111") )
    info.lsuIsa.amoxor_d    := ( x === BitPat("b00100????????????011?????0101111") )
    info.lsuIsa.amoand_d    := ( x === BitPat("b01100????????????011?????0101111") )
    info.lsuIsa.amoor_d     := ( x === BitPat("b01000????????????011?????0101111") )
    info.lsuIsa.amomin_d    := ( x === BitPat("b10000????????????011?????0101111") )
    info.lsuIsa.amomax_d    := ( x === BitPat("b10100????????????011?????0101111") )
    info.lsuIsa.amominu_d   := ( x === BitPat("b11000????????????011?????0101111") )
    info.lsuIsa.amomaxu_d   := ( x === BitPat("b11100????????????011?????0101111") )



    info.privil_isa.is_access_fault     := false.B
    info.privil_isa.is_paging_fault     := false.B

}

trait Decode32FD{ this: DecodeBase =>
  if( fpuNum > 0 ) {
    info.lsuIsa.flw         := ( x === BitPat("b?????????????????010?????0000111") )
    info.lsuIsa.fsw         := ( x === BitPat("b?????????????????010?????0100111") )
    info.fpuIsa.fmadd_s     := ( x === BitPat("b?????00??????????????????1000011") )
    info.fpuIsa.fmsub_s     := ( x === BitPat("b?????00??????????????????1000111") )
    info.fpuIsa.fnmsub_s    := ( x === BitPat("b?????00??????????????????1001011") )
    info.fpuIsa.fnmadd_s    := ( x === BitPat("b?????00??????????????????1001111") )
    info.fpuIsa.fadd_s      := ( x === BitPat("b0000000??????????????????1010011") )
    info.fpuIsa.fsub_s      := ( x === BitPat("b0000100??????????????????1010011") )
    info.fpuIsa.fmul_s      := ( x === BitPat("b0001000??????????????????1010011") )
    info.fpuIsa.fdiv_s      := ( x === BitPat("b0001100??????????????????1010011") )
    info.fpuIsa.fsqrt_s     := ( x === BitPat("b010110000000?????????????1010011") )
    info.fpuIsa.fsgnj_s     := ( x === BitPat("b0010000??????????000?????1010011") )
    info.fpuIsa.fsgnjn_s    := ( x === BitPat("b0010000??????????001?????1010011") )
    info.fpuIsa.fsgnjx_s    := ( x === BitPat("b0010000??????????010?????1010011") )
    info.fpuIsa.fmin_s      := ( x === BitPat("b0010100??????????000?????1010011") )
    info.fpuIsa.fmax_s      := ( x === BitPat("b0010100??????????001?????1010011") )
    info.fpuIsa.fcvt_w_s    := ( x === BitPat("b110000000000?????????????1010011") )
    info.fpuIsa.fcvt_wu_s   := ( x === BitPat("b110000000001?????????????1010011") )
    info.fpuIsa.fmv_x_w     := ( x === BitPat("b111000000000?????000?????1010011") )
    info.fpuIsa.feq_s       := ( x === BitPat("b1010000??????????010?????1010011") )
    info.fpuIsa.flt_s       := ( x === BitPat("b1010000??????????001?????1010011") )
    info.fpuIsa.fle_s       := ( x === BitPat("b1010000??????????000?????1010011") )
    info.fpuIsa.fclass_s    := ( x === BitPat("b111000000000?????001?????1010011") )
    info.fpuIsa.fcvt_s_w    := ( x === BitPat("b110100000000?????????????1010011") )
    info.fpuIsa.fcvt_s_wu   := ( x === BitPat("b110100000001?????????????1010011") )
    info.fpuIsa.fmv_w_x     := ( x === BitPat("b111100000000?????000?????1010011") )
    info.fpuIsa.fcvt_l_s    := ( x === BitPat("b110000000010?????????????1010011") )
    info.fpuIsa.fcvt_lu_s   := ( x === BitPat("b110000000011?????????????1010011") )
    info.fpuIsa.fcvt_s_l    := ( x === BitPat("b110100000010?????????????1010011") )
    info.fpuIsa.fcvt_s_lu   := ( x === BitPat("b110100000011?????????????1010011") )


    info.lsuIsa.fld         := ( x === BitPat("b?????????????????011?????0000111") )
    info.lsuIsa.fsd         := ( x === BitPat("b?????????????????011?????0100111") )
    info.fpuIsa.fmadd_d     := ( x === BitPat("b?????01??????????????????1000011") )
    info.fpuIsa.fmsub_d     := ( x === BitPat("b?????01??????????????????1000111") )
    info.fpuIsa.fnmsub_d    := ( x === BitPat("b?????01??????????????????1001011") )
    info.fpuIsa.fnmadd_d    := ( x === BitPat("b?????01??????????????????1001111") )
    info.fpuIsa.fadd_d      := ( x === BitPat("b0000001??????????????????1010011") )
    info.fpuIsa.fsub_d      := ( x === BitPat("b0000101??????????????????1010011") )
    info.fpuIsa.fmul_d      := ( x === BitPat("b0001001??????????????????1010011") )
    info.fpuIsa.fdiv_d      := ( x === BitPat("b0001101??????????????????1010011") )
    info.fpuIsa.fsqrt_d     := ( x === BitPat("b010110100000?????????????1010011") )
    info.fpuIsa.fsgnj_d     := ( x === BitPat("b0010001??????????000?????1010011") )
    info.fpuIsa.fsgnjn_d    := ( x === BitPat("b0010001??????????001?????1010011") )
    info.fpuIsa.fsgnjx_d    := ( x === BitPat("b0010001??????????010?????1010011") )
    info.fpuIsa.fmin_d      := ( x === BitPat("b0010101??????????000?????1010011") )
    info.fpuIsa.fmax_d      := ( x === BitPat("b0010101??????????001?????1010011") )
    info.fpuIsa.fcvt_s_d    := ( x === BitPat("b010000000001?????????????1010011") )
    info.fpuIsa.fcvt_d_s    := ( x === BitPat("b010000100000?????????????1010011") )
    info.fpuIsa.feq_d       := ( x === BitPat("b1010001??????????010?????1010011") )
    info.fpuIsa.flt_d       := ( x === BitPat("b1010001??????????001?????1010011") )
    info.fpuIsa.fle_d       := ( x === BitPat("b1010001??????????000?????1010011") )
    info.fpuIsa.fclass_d    := ( x === BitPat("b111000100000?????001?????1010011") )
    info.fpuIsa.fcvt_w_d    := ( x === BitPat("b110000100000?????????????1010011") )
    info.fpuIsa.fcvt_wu_d   := ( x === BitPat("b110000100001?????????????1010011") )
    info.fpuIsa.fcvt_d_w    := ( x === BitPat("b110100100000?????????????1010011") )
    info.fpuIsa.fcvt_d_wu   := ( x === BitPat("b110100100001?????????????1010011") )
    info.fpuIsa.fcvt_l_d    := ( x === BitPat("b110000100010?????????????1010011") )
    info.fpuIsa.fcvt_lu_d   := ( x === BitPat("b110000100011?????????????1010011") )
    info.fpuIsa.fmv_x_d     := ( x === BitPat("b111000100000?????000?????1010011") )
    info.fpuIsa.fcvt_d_l    := ( x === BitPat("b110100100010?????????????1010011") )
    info.fpuIsa.fcvt_d_lu   := ( x === BitPat("b110100100011?????????????1010011") )
    info.fpuIsa.fmv_d_x     := ( x === BitPat("b111100100000?????000?????1010011") )      
  } else {
    info.lsuIsa.flw := false.B
    info.lsuIsa.fsw := false.B
    info.lsuIsa.fld := false.B
    info.lsuIsa.fsd := false.B
    info.fpuIsa     := 0.U.asTypeOf(new Fpu_isa)
  }
}



class Decode32(implicit p: Parameters) extends DecodeBase
with Decode32G
with Decode32FD{
  info.param.is_rvc := false.B
} 

/**
 * This object implements a 32-bit RISC-V instruction decoder.
 * @note It takes UInt inputs x and pc, along with a boolean hasFpu to indicate whether
 *       or not the processor has a floating point unit. An implicit Parameters object is
 *       also expected to be in scope.
 */
object Decode32 {
  /**
    * Decodes the given 32-bit RISC-V instruction.
    *
    * @param x The 32-bit instruction to be decoded.
    * @param pc The program counter for the instruction.
    * @param hasFpu Indicates whether or not the processor has a floating point unit.
    * @param p The implicit Parameters object for the Rift2Core.
    * @return An Info_instruction object representing the decoded instruction.
    */
  def apply(x:UInt, pc: UInt, hasFpu: Boolean, hasVector: Boolean)(implicit p: Parameters): Info_instruction = {
    // val info = Wire(new Info_instruction)
    val dec32 = Module( (if(hasVector) {new Decode32 with VDecode32} else {new Decode32 with NVDecode}) )
    dec32.io.x := x
    dec32.io.pc := pc
    return dec32.io.info
  }
}






