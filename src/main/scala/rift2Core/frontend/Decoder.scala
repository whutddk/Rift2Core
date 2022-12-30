





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
package rift2Core.frontend

import chisel3._
import chisel3.util._
import rift2Core.define._

import rift2Chip._
import chipsalliance.rocketchip.config._



class Decode16(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val x  = Input(UInt(16.W))
    val pc = Input(UInt(64.W))
    val info = Output( new Info_instruction )
  })

  val x  = WireDefault(io.x)
  val pc = WireDefault(io.pc)
  val info = Wire(new Info_instruction)
  io.info := info

  info.param.pc := pc
  info.param.is_rvc := true.B

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
  info.param.raw.rs4       := 0.U
  info.param.raw.rs5       := 0.U
  info.param.raw.rd1       := 0.U

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

  info.param.rm := Mux( info.fpu_isa.is_fpu, x(14,12), 0.U )



  info.alu_isa.lui         := c_lui
  info.alu_isa.auipc       := false.B
  info.alu_isa.addi        := c_addi4spn | c_nop | c_addi | c_li | c_addi16sp
  info.alu_isa.addiw       := c_addiw
  info.alu_isa.slti        := false.B
  info.alu_isa.sltiu       := false.B
  info.alu_isa.xori        := false.B
  info.alu_isa.ori         := false.B
  info.alu_isa.andi        := c_andi
  info.alu_isa.slli        := c_slli
  info.alu_isa.slliw       := false.B
  info.alu_isa.srli        := c_srli
  info.alu_isa.srliw       := false.B
  info.alu_isa.srai        := c_srai
  info.alu_isa.sraiw       := false.B
  info.alu_isa.add         := c_mv | c_add
  info.alu_isa.addw        := c_addw
  info.alu_isa.sub         := c_sub
  info.alu_isa.subw        := c_subw
  info.alu_isa.sll         := false.B
  info.alu_isa.sllw        := false.B
  info.alu_isa.slt         := false.B
  info.alu_isa.sltu        := false.B
  info.alu_isa.xor         := c_xor
  info.alu_isa.srl         := false.B
  info.alu_isa.srlw        := false.B
  info.alu_isa.sra         := false.B
  info.alu_isa.sraw        := false.B
  info.alu_isa.or          := c_or
  info.alu_isa.and         := c_and
  info.alu_isa.wfi         := false.B
  
  info.bru_isa.jal         := c_j
  info.bru_isa.jalr        := c_jr | c_jalr
  info.bru_isa.beq         := c_beqz
  info.bru_isa.bne         := c_bnez
  info.bru_isa.blt         := false.B
  info.bru_isa.bge         := false.B
  info.bru_isa.bltu        := false.B
  info.bru_isa.bgeu        := false.B

  info.lsu_isa.lb          := false.B
  info.lsu_isa.lh          := false.B
  info.lsu_isa.lw          := c_lw | c_lwsp
  info.lsu_isa.ld          := c_ld | c_ldsp
  info.lsu_isa.lbu         := false.B
  info.lsu_isa.lhu         := false.B
  info.lsu_isa.lwu         := false.B
  info.lsu_isa.sb          := false.B
  info.lsu_isa.sh          := false.B
  info.lsu_isa.sw          := c_sw | c_swsp
  info.lsu_isa.sd          := c_sd | c_sdsp
  info.lsu_isa.fence       := false.B
  info.lsu_isa.fence_i     := false.B
  info.lsu_isa.sfence_vma  := false.B

  info.csr_isa.rw          := false.B
  info.csr_isa.rs          := false.B
  info.csr_isa.rc          := false.B
  info.csr_isa.rwi         := false.B
  info.csr_isa.rsi         := false.B
  info.csr_isa.rci         := false.B

  info.mul_isa.mul         := false.B
  info.mul_isa.mulh        := false.B
  info.mul_isa.mulhsu      := false.B
  info.mul_isa.mulhu       := false.B
  info.mul_isa.div         := false.B
  info.mul_isa.divu        := false.B
  info.mul_isa.rem         := false.B
  info.mul_isa.remu        := false.B
  info.mul_isa.mulw        := false.B
  info.mul_isa.divw        := false.B
  info.mul_isa.divuw       := false.B
  info.mul_isa.remw        := false.B
  info.mul_isa.remuw       := false.B

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

  info.lsu_isa.lr_w        := false.B
  info.lsu_isa.sc_w        := false.B
  info.lsu_isa.amoswap_w   := false.B
  info.lsu_isa.amoadd_w    := false.B
  info.lsu_isa.amoxor_w    := false.B
  info.lsu_isa.amoand_w    := false.B
  info.lsu_isa.amoor_w     := false.B
  info.lsu_isa.amomin_w    := false.B
  info.lsu_isa.amomax_w    := false.B
  info.lsu_isa.amominu_w   := false.B
  info.lsu_isa.amomaxu_w   := false.B
  info.lsu_isa.lr_d        := false.B
  info.lsu_isa.sc_d        := false.B
  info.lsu_isa.amoswap_d   := false.B
  info.lsu_isa.amoadd_d    := false.B
  info.lsu_isa.amoxor_d    := false.B
  info.lsu_isa.amoand_d    := false.B
  info.lsu_isa.amoor_d     := false.B
  info.lsu_isa.amomin_d    := false.B
  info.lsu_isa.amomax_d    := false.B
  info.lsu_isa.amominu_d   := false.B
  info.lsu_isa.amomaxu_d   := false.B

  info.lsu_isa.flw         := false.B
  info.lsu_isa.fsw         := false.B
  info.fpu_isa.fmadd_s     := false.B
  info.fpu_isa.fmsub_s     := false.B
  info.fpu_isa.fnmsub_s    := false.B
  info.fpu_isa.fnmadd_s    := false.B
  info.fpu_isa.fadd_s      := false.B
  info.fpu_isa.fsub_s      := false.B
  info.fpu_isa.fmul_s      := false.B
  info.fpu_isa.fdiv_s      := false.B
  info.fpu_isa.fsqrt_s     := false.B
  info.fpu_isa.fsgnj_s     := false.B
  info.fpu_isa.fsgnjn_s    := false.B
  info.fpu_isa.fsgnjx_s    := false.B
  info.fpu_isa.fmin_s      := false.B
  info.fpu_isa.fmax_s      := false.B
  info.fpu_isa.fcvt_w_s    := false.B
  info.fpu_isa.fcvt_wu_s   := false.B
  info.fpu_isa.fmv_x_w     := false.B
  info.fpu_isa.feq_s       := false.B
  info.fpu_isa.flt_s       := false.B
  info.fpu_isa.fle_s       := false.B
  info.fpu_isa.fclass_s    := false.B
  info.fpu_isa.fcvt_s_w    := false.B
  info.fpu_isa.fcvt_s_wu   := false.B
  info.fpu_isa.fmv_w_x     := false.B
  info.fpu_isa.fcvt_l_s    := false.B
  info.fpu_isa.fcvt_lu_s   := false.B
  info.fpu_isa.fcvt_s_l    := false.B
  info.fpu_isa.fcvt_s_lu   := false.B

  info.lsu_isa.fld         := c_fld | c_fldsp
  info.lsu_isa.fsd         := c_fsd | c_fsdsp
  info.fpu_isa.fmadd_d     := false.B
  info.fpu_isa.fmsub_d     := false.B
  info.fpu_isa.fnmsub_d    := false.B
  info.fpu_isa.fnmadd_d    := false.B
  info.fpu_isa.fadd_d      := false.B
  info.fpu_isa.fsub_d      := false.B
  info.fpu_isa.fmul_d      := false.B
  info.fpu_isa.fdiv_d      := false.B
  info.fpu_isa.fsqrt_d     := false.B
  info.fpu_isa.fsgnj_d     := false.B
  info.fpu_isa.fsgnjn_d    := false.B
  info.fpu_isa.fsgnjx_d    := false.B
  info.fpu_isa.fmin_d      := false.B
  info.fpu_isa.fmax_d      := false.B
  info.fpu_isa.fcvt_s_d    := false.B
  info.fpu_isa.fcvt_d_s    := false.B
  info.fpu_isa.feq_d       := false.B
  info.fpu_isa.flt_d       := false.B
  info.fpu_isa.fle_d       := false.B
  info.fpu_isa.fclass_d    := false.B
  info.fpu_isa.fcvt_w_d    := false.B
  info.fpu_isa.fcvt_wu_d   := false.B
  info.fpu_isa.fcvt_d_w    := false.B
  info.fpu_isa.fcvt_d_wu   := false.B
  info.fpu_isa.fcvt_l_d    := false.B
  info.fpu_isa.fcvt_lu_d   := false.B
  info.fpu_isa.fmv_x_d     := false.B
  info.fpu_isa.fcvt_d_l    := false.B
  info.fpu_isa.fcvt_d_lu   := false.B
  info.fpu_isa.fmv_d_x     := false.B
  info.fpu_isa.fcsr_rw     := false.B
  info.fpu_isa.fcsr_rs     := false.B
  info.fpu_isa.fcsr_rc     := false.B
  info.fpu_isa.fcsr_rwi    := false.B
  info.fpu_isa.fcsr_rsi    := false.B
  info.fpu_isa.fcsr_rci    := false.B

  info.privil_isa.is_access_fault := ( x === BitPat("b1001110001000001") )
  info.privil_isa.is_paging_fault := ( x === BitPat("b1001110001000101") )

  info.vectorIsa := 0.U.asTypeOf( new VectorIsa )
  info.param.vm  := false.B
  info.param.nf  := 0.U


} 

object Decode16 {
  def apply(x:UInt, pc: UInt, hasFpu: Boolean)(implicit p: Parameters): Info_instruction = {
    // val info = Wire(new Info_instruction)
    val dec16 = Module(new Decode16)
    dec16.io.x := x
    dec16.io.pc := pc
    return dec16.io.info
  }
}


abstract class Decode32Base (implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val x  = Input(UInt(32.W))
    val pc = Input(UInt(64.W))
    val info = Output( new Info_instruction )
  })

  val x  = WireDefault(io.x)
  val pc = WireDefault(io.pc)
  val info = Wire(new Info_instruction)

  io.info := info
  info.param.pc := pc
  info.param.is_rvc := false.B
}

trait Decode32G { this: Decode32Base =>

    def iType_imm = Cat( Fill(52, x(31)), x(31,20))
    def sType_imm = Cat( Fill(52, x(31)), x(31,25), x(11,7) )
    def bType_imm = Cat( Fill(52, x(31)), x(7), x(30,25), x(11,8), 0.U(1.W))
    def uType_imm = Cat( Fill(32, x(31)), x(31,12), 0.U(12.W))
    def jType_imm = Cat( Fill(44, x(31)), x(19,12), x(20), x(30,21), 0.U(1.W))
    def aType_imm = Cat( Fill(62, 0.U), x(26,25))
    def mType_imm = Cat( Fill(61, 0.U), x(14,12))
    def shamt_imm = Cat( Fill(58, 0.U), x(25,20) )


    def is_iType = info.bru_isa.jalr | info.lsu_isa.lb | info.lsu_isa.lh |info.lsu_isa.lw | info.lsu_isa.lbu | info.lsu_isa.lhu | info.lsu_isa.lwu | info.lsu_isa.ld | info.alu_isa.addi | info.alu_isa.addiw | info.alu_isa.slti | info.alu_isa.sltiu | info.alu_isa.xori | info.alu_isa.ori | info.alu_isa.andi | info.lsu_isa.fence | info.lsu_isa.fence_i | info.csr_isa.rw | info.csr_isa.rs | info.csr_isa.rc | info.csr_isa.rwi | info.csr_isa.rsi | info.csr_isa.rci | info.lsu_isa.flw | info.lsu_isa.fld | info.fpu_isa.is_fun_fcsr | info.vectorIsa.isVector
    def is_sType = info.lsu_isa.sb | info.lsu_isa.sh | info.lsu_isa.sw | info.lsu_isa.sd | info.lsu_isa.fsw | info.lsu_isa.fsd 
    def is_bType = info.bru_isa.beq | info.bru_isa.bne | info.bru_isa.blt | info.bru_isa.bge | info.bru_isa.bltu | info.bru_isa.bgeu;
    def is_uType = info.alu_isa.lui | info.alu_isa.auipc;
    def is_jType = info.bru_isa.jal;
    def is_aType = info.lsu_isa.is_lrsc | info.lsu_isa.is_amo
    def is_mType = info.fpu_isa.is_fpu & ~(info.fpu_isa.is_fun_fcsr)
    def is_shamt = info.alu_isa.slli | info.alu_isa.srli | info.alu_isa.srai | info.alu_isa.slliw | info.alu_isa.srliw | info.alu_isa.sraiw



    info.param.imm := MuxCase( 0.U, Array(
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





    info.param.raw.rd0 := MuxCase( x(11,7), Array(
      info.alu_isa.wfi         -> 0.U,
      info.bru_isa.beq         -> 0.U,
      info.bru_isa.bne         -> 0.U,
      info.bru_isa.blt         -> 0.U,
      info.bru_isa.bge         -> 0.U,
      info.bru_isa.bltu        -> 0.U,
      info.bru_isa.bgeu        -> 0.U,
      info.lsu_isa.sb          -> 0.U,
      info.lsu_isa.sh          -> 0.U,
      info.lsu_isa.sw          -> 0.U,
      info.lsu_isa.sd          -> 0.U,
      info.lsu_isa.fsw         -> 0.U,
      info.lsu_isa.fsd         -> 0.U,
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





    info.param.raw.rs1 := MuxCase( x(19,15), Array(
      info.alu_isa.lui       -> 0.U,
      info.alu_isa.auipc     -> 0.U,
      info.alu_isa.wfi       -> 0.U,
      info.bru_isa.jal       -> 0.U,
      info.privil_isa.ecall  -> 0.U,
      info.privil_isa.ebreak -> 0.U,
      info.privil_isa.uret   -> 0.U,
      info.privil_isa.sret   -> 0.U,
      info.privil_isa.mret   -> 0.U,
      info.privil_isa.dret   -> 0.U,
    ))
      

    info.param.raw.rs2 := MuxCase( x(24,20), Array(
      info.alu_isa.lui   -> 0.U,
      info.alu_isa.auipc -> 0.U,
      info.alu_isa.addi  -> 0.U,
      info.alu_isa.slti  -> 0.U,
      info.alu_isa.sltiu -> 0.U,
      info.alu_isa.xori  -> 0.U,
      info.alu_isa.ori   -> 0.U,
      info.alu_isa.andi  -> 0.U,
      info.alu_isa.slli  -> 0.U,
      info.alu_isa.srli  -> 0.U,
      info.alu_isa.srai  -> 0.U,
      info.alu_isa.slli  -> 0.U,
      info.alu_isa.srli  -> 0.U,
      info.alu_isa.srai  -> 0.U,
      info.alu_isa.addiw -> 0.U,
      info.alu_isa.slliw -> 0.U,
      info.alu_isa.srliw -> 0.U,
      info.alu_isa.sraiw -> 0.U,
      info.alu_isa.wfi   -> 0.U,
      info.bru_isa.jal   -> 0.U,
      info.bru_isa.jalr  -> 0.U,
      info.lsu_isa.lb    -> 0.U,
      info.lsu_isa.lh    -> 0.U,
      info.lsu_isa.lw    -> 0.U,
      info.lsu_isa.lbu   -> 0.U,
      info.lsu_isa.lhu   -> 0.U,
      info.lsu_isa.lwu   -> 0.U,
      info.lsu_isa.ld    -> 0.U,
      info.lsu_isa.fence -> 0.U,
      info.lsu_isa.fence_i -> 0.U,
      info.lsu_isa.lr_w  -> 0.U,
      info.lsu_isa.lr_d  -> 0.U,
      info.lsu_isa.flw       -> 0.U,

      info.privil_isa.ecall  -> 0.U,
      info.privil_isa.ebreak -> 0.U,
      info.privil_isa.uret   -> 0.U,
      info.privil_isa.sret   -> 0.U,
      info.privil_isa.mret   -> 0.U,
      info.privil_isa.dret   -> 0.U,

      info.csr_isa.is_csr -> 0.U,
      info.fpu_isa.fcsr_rw   -> 0.U,
      info.fpu_isa.fcsr_rs   -> 0.U,
      info.fpu_isa.fcsr_rc   -> 0.U,
      info.fpu_isa.fcsr_rwi   -> 0.U,
      info.fpu_isa.fcsr_rsi   -> 0.U,
      info.fpu_isa.fcsr_rci   -> 0.U,
      info.fpu_isa.fsqrt_s   -> 0.U,
      info.fpu_isa.fcvt_w_s  -> 0.U,
      info.fpu_isa.fcvt_wu_s -> 0.U,
      info.fpu_isa.fmv_x_w   -> 0.U,
      info.fpu_isa.fclass_s  -> 0.U,
      info.fpu_isa.fcvt_s_w  -> 0.U,
      info.fpu_isa.fcvt_s_wu -> 0.U,
      info.fpu_isa.fmv_w_x   -> 0.U,
      info.fpu_isa.fcvt_l_s  -> 0.U,
      info.fpu_isa.fcvt_lu_s -> 0.U,
      info.fpu_isa.fcvt_s_l  -> 0.U,
      info.fpu_isa.fcvt_s_lu -> 0.U,
      info.fpu_isa.fsqrt_d   -> 0.U,
      info.fpu_isa.fcvt_s_d  -> 0.U,
      info.fpu_isa.fcvt_d_s  -> 0.U,
      info.fpu_isa.fclass_d  -> 0.U,
      info.fpu_isa.fcvt_w_d  -> 0.U,
      info.fpu_isa.fcvt_wu_d -> 0.U,
      info.fpu_isa.fcvt_d_w  -> 0.U,
      info.fpu_isa.fcvt_d_wu -> 0.U,
      info.fpu_isa.fcvt_l_d  -> 0.U,
      info.fpu_isa.fcvt_lu_d -> 0.U,
      info.fpu_isa.fmv_x_d   -> 0.U,
      info.fpu_isa.fcvt_d_l  -> 0.U,
      info.fpu_isa.fcvt_d_lu -> 0.U,
      info.fpu_isa.fmv_d_x   -> 0.U,


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



    info.param.raw.rs3 := MuxCase( 0.U, Array(
      info.fpu_isa.fmadd_s  -> x(31,27),
      info.fpu_isa.fmsub_s  -> x(31,27),
      info.fpu_isa.fnmsub_s -> x(31,27),
      info.fpu_isa.fnmadd_s -> x(31,27),
      info.fpu_isa.fmadd_d  -> x(31,27),
      info.fpu_isa.fmsub_d  -> x(31,27),
      info.fpu_isa.fnmsub_d -> x(31,27),
      info.fpu_isa.fnmadd_d -> x(31,27),
    ))

    info.param.raw.rs4       := 0.U
    info.param.raw.rs5       := 0.U
    info.param.raw.rd1       := 0.U

    info.param.rm := x(14,12)

    info.alu_isa.lui         := ( x === BitPat("b?????????????????????????0110111") )
    info.alu_isa.auipc       := ( x === BitPat("b?????????????????????????0010111") )
    info.alu_isa.addi        := ( x === BitPat("b?????????????????000?????0010011") )
    info.alu_isa.addiw       := ( x === BitPat("b?????????????????000?????0011011") )
    info.alu_isa.slti        := ( x === BitPat("b?????????????????010?????0010011") )
    info.alu_isa.sltiu       := ( x === BitPat("b?????????????????011?????0010011") )
    info.alu_isa.xori        := ( x === BitPat("b?????????????????100?????0010011") )
    info.alu_isa.ori         := ( x === BitPat("b?????????????????110?????0010011") )
    info.alu_isa.andi        := ( x === BitPat("b?????????????????111?????0010011") )
    info.alu_isa.slli        := ( x === BitPat("b000000???????????001?????0010011") )
    info.alu_isa.slliw       := ( x === BitPat("b0000000??????????001?????0011011") )
    info.alu_isa.srli        := ( x === BitPat("b000000???????????101?????0010011") )
    info.alu_isa.srliw       := ( x === BitPat("b0000000??????????101?????0011011") )
    info.alu_isa.srai        := ( x === BitPat("b010000???????????101?????0010011") )
    info.alu_isa.sraiw       := ( x === BitPat("b0100000??????????101?????0011011") )
    info.alu_isa.add         := ( x === BitPat("b0000000??????????000?????0110011") )
    info.alu_isa.addw        := ( x === BitPat("b0000000??????????000?????0111011") )
    info.alu_isa.sub         := ( x === BitPat("b0100000??????????000?????0110011") )
    info.alu_isa.subw        := ( x === BitPat("b0100000??????????000?????0111011") )
    info.alu_isa.sll         := ( x === BitPat("b0000000??????????001?????0110011") )
    info.alu_isa.sllw        := ( x === BitPat("b0000000??????????001?????0111011") )
    info.alu_isa.slt         := ( x === BitPat("b0000000??????????010?????0110011") )
    info.alu_isa.sltu        := ( x === BitPat("b0000000??????????011?????0110011") )
    info.alu_isa.xor         := ( x === BitPat("b0000000??????????100?????0110011") )
    info.alu_isa.srl         := ( x === BitPat("b0000000??????????101?????0110011") )
    info.alu_isa.srlw        := ( x === BitPat("b0000000??????????101?????0111011") )
    info.alu_isa.sra         := ( x === BitPat("b0100000??????????101?????0110011") )
    info.alu_isa.sraw        := ( x === BitPat("b0100000??????????101?????0111011") )
    info.alu_isa.or          := ( x === BitPat("b0000000??????????110?????0110011") )
    info.alu_isa.and         := ( x === BitPat("b0000000??????????111?????0110011") )
    info.alu_isa.wfi         := ( x === BitPat("b00010000010100000000000001110011") )

    info.bru_isa.jal         := ( x === BitPat("b?????????????????????????1101111") )
    info.bru_isa.jalr        := ( x === BitPat("b?????????????????000?????1100111") )
    info.bru_isa.beq         := ( x === BitPat("b?????????????????000?????1100011") )
    info.bru_isa.bne         := ( x === BitPat("b?????????????????001?????1100011") )
    info.bru_isa.blt         := ( x === BitPat("b?????????????????100?????1100011") )
    info.bru_isa.bge         := ( x === BitPat("b?????????????????101?????1100011") )
    info.bru_isa.bltu        := ( x === BitPat("b?????????????????110?????1100011") )
    info.bru_isa.bgeu        := ( x === BitPat("b?????????????????111?????1100011") )

    info.lsu_isa.lb          := ( x === BitPat("b?????????????????000?????0000011") )
    info.lsu_isa.lh          := ( x === BitPat("b?????????????????001?????0000011") )
    info.lsu_isa.lw          := ( x === BitPat("b?????????????????010?????0000011") )
    info.lsu_isa.ld          := ( x === BitPat("b?????????????????011?????0000011") )
    info.lsu_isa.lbu         := ( x === BitPat("b?????????????????100?????0000011") )
    info.lsu_isa.lhu         := ( x === BitPat("b?????????????????101?????0000011") )
    info.lsu_isa.lwu         := ( x === BitPat("b?????????????????110?????0000011") )
    info.lsu_isa.sb          := ( x === BitPat("b?????????????????000?????0100011") )
    info.lsu_isa.sh          := ( x === BitPat("b?????????????????001?????0100011") )
    info.lsu_isa.sw          := ( x === BitPat("b?????????????????010?????0100011") )
    info.lsu_isa.sd          := ( x === BitPat("b?????????????????011?????0100011") )
    info.lsu_isa.fence       := ( x === BitPat("b?????????????????000?????0001111") )
    info.lsu_isa.fence_i     := ( x === BitPat("b?????????????????001?????0001111") )
    info.lsu_isa.sfence_vma  := ( x === BitPat("b0001001??????????000000001110011") )

    info.csr_isa.rw          := ( x === BitPat("b?????????????????001?????1110011") ) & ( x(31,20) =/= 1.U & x(31,20) =/= 2.U & x(31,20) =/= 3.U ) 
    info.csr_isa.rs          := ( x === BitPat("b?????????????????010?????1110011") ) & ( x(31,20) =/= 1.U & x(31,20) =/= 2.U & x(31,20) =/= 3.U ) 
    info.csr_isa.rc          := ( x === BitPat("b?????????????????011?????1110011") ) & ( x(31,20) =/= 1.U & x(31,20) =/= 2.U & x(31,20) =/= 3.U ) 
    info.csr_isa.rwi         := ( x === BitPat("b?????????????????101?????1110011") ) & ( x(31,20) =/= 1.U & x(31,20) =/= 2.U & x(31,20) =/= 3.U ) 
    info.csr_isa.rsi         := ( x === BitPat("b?????????????????110?????1110011") ) & ( x(31,20) =/= 1.U & x(31,20) =/= 2.U & x(31,20) =/= 3.U ) 
    info.csr_isa.rci         := ( x === BitPat("b?????????????????111?????1110011") ) & ( x(31,20) =/= 1.U & x(31,20) =/= 2.U & x(31,20) =/= 3.U ) 

    if( fpuNum > 0 ) {
      info.fpu_isa.fcsr_rw   := ( x === BitPat("b?????????????????001?????1110011") ) & ( x(31,20) === 1.U | x(31,20) === 2.U | x(31,20) === 3.U ) 
      info.fpu_isa.fcsr_rs   := ( x === BitPat("b?????????????????010?????1110011") ) & ( x(31,20) === 1.U | x(31,20) === 2.U | x(31,20) === 3.U ) 
      info.fpu_isa.fcsr_rc   := ( x === BitPat("b?????????????????011?????1110011") ) & ( x(31,20) === 1.U | x(31,20) === 2.U | x(31,20) === 3.U ) 
      info.fpu_isa.fcsr_rwi  := ( x === BitPat("b?????????????????101?????1110011") ) & ( x(31,20) === 1.U | x(31,20) === 2.U | x(31,20) === 3.U ) 
      info.fpu_isa.fcsr_rsi  := ( x === BitPat("b?????????????????110?????1110011") ) & ( x(31,20) === 1.U | x(31,20) === 2.U | x(31,20) === 3.U ) 
      info.fpu_isa.fcsr_rci  := ( x === BitPat("b?????????????????111?????1110011") ) & ( x(31,20) === 1.U | x(31,20) === 2.U | x(31,20) === 3.U )       
    } else {
      info.fpu_isa.fcsr_rw   := false.B
      info.fpu_isa.fcsr_rs   := false.B
      info.fpu_isa.fcsr_rc   := false.B
      info.fpu_isa.fcsr_rwi  := false.B
      info.fpu_isa.fcsr_rsi  := false.B
      info.fpu_isa.fcsr_rci  := false.B
    }


    info.mul_isa.mul         := ( x === BitPat("b0000001??????????000?????0110011") )
    info.mul_isa.mulh        := ( x === BitPat("b0000001??????????001?????0110011") )
    info.mul_isa.mulhsu      := ( x === BitPat("b0000001??????????010?????0110011") )
    info.mul_isa.mulhu       := ( x === BitPat("b0000001??????????011?????0110011") )
    info.mul_isa.div         := ( x === BitPat("b0000001??????????100?????0110011") )
    info.mul_isa.divu        := ( x === BitPat("b0000001??????????101?????0110011") )
    info.mul_isa.rem         := ( x === BitPat("b0000001??????????110?????0110011") )
    info.mul_isa.remu        := ( x === BitPat("b0000001??????????111?????0110011") )
    info.mul_isa.mulw        := ( x === BitPat("b0000001??????????000?????0111011") )
    info.mul_isa.divw        := ( x === BitPat("b0000001??????????100?????0111011") )
    info.mul_isa.divuw       := ( x === BitPat("b0000001??????????101?????0111011") )
    info.mul_isa.remw        := ( x === BitPat("b0000001??????????110?????0111011") )
    info.mul_isa.remuw       := ( x === BitPat("b0000001??????????111?????0111011") )

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

    info.lsu_isa.lr_w        := ( x === BitPat("b00010??00000?????010?????0101111") )
    info.lsu_isa.sc_w        := ( x === BitPat("b00011????????????010?????0101111") )
    info.lsu_isa.amoswap_w   := ( x === BitPat("b00001????????????010?????0101111") )
    info.lsu_isa.amoadd_w    := ( x === BitPat("b00000????????????010?????0101111") )
    info.lsu_isa.amoxor_w    := ( x === BitPat("b00100????????????010?????0101111") )
    info.lsu_isa.amoand_w    := ( x === BitPat("b01100????????????010?????0101111") )
    info.lsu_isa.amoor_w     := ( x === BitPat("b01000????????????010?????0101111") )
    info.lsu_isa.amomin_w    := ( x === BitPat("b10000????????????010?????0101111") )
    info.lsu_isa.amomax_w    := ( x === BitPat("b10100????????????010?????0101111") )
    info.lsu_isa.amominu_w   := ( x === BitPat("b11000????????????010?????0101111") )
    info.lsu_isa.amomaxu_w   := ( x === BitPat("b11100????????????010?????0101111") )
    info.lsu_isa.lr_d        := ( x === BitPat("b00010??00000?????011?????0101111") )
    info.lsu_isa.sc_d        := ( x === BitPat("b00011????????????011?????0101111") )
    info.lsu_isa.amoswap_d   := ( x === BitPat("b00001????????????011?????0101111") )
    info.lsu_isa.amoadd_d    := ( x === BitPat("b00000????????????011?????0101111") )
    info.lsu_isa.amoxor_d    := ( x === BitPat("b00100????????????011?????0101111") )
    info.lsu_isa.amoand_d    := ( x === BitPat("b01100????????????011?????0101111") )
    info.lsu_isa.amoor_d     := ( x === BitPat("b01000????????????011?????0101111") )
    info.lsu_isa.amomin_d    := ( x === BitPat("b10000????????????011?????0101111") )
    info.lsu_isa.amomax_d    := ( x === BitPat("b10100????????????011?????0101111") )
    info.lsu_isa.amominu_d   := ( x === BitPat("b11000????????????011?????0101111") )
    info.lsu_isa.amomaxu_d   := ( x === BitPat("b11100????????????011?????0101111") )



    info.privil_isa.is_access_fault     := false.B
    info.privil_isa.is_paging_fault     := false.B

}

trait Decode32FD{ this: Decode32Base =>
  if( fpuNum > 0 ) {
    info.lsu_isa.flw         := ( x === BitPat("b?????????????????010?????0000111") )
    info.lsu_isa.fsw         := ( x === BitPat("b?????????????????010?????0100111") )
    info.fpu_isa.fmadd_s     := ( x === BitPat("b?????00??????????????????1000011") )
    info.fpu_isa.fmsub_s     := ( x === BitPat("b?????00??????????????????1000111") )
    info.fpu_isa.fnmsub_s    := ( x === BitPat("b?????00??????????????????1001011") )
    info.fpu_isa.fnmadd_s    := ( x === BitPat("b?????00??????????????????1001111") )
    info.fpu_isa.fadd_s      := ( x === BitPat("b0000000??????????????????1010011") )
    info.fpu_isa.fsub_s      := ( x === BitPat("b0000100??????????????????1010011") )
    info.fpu_isa.fmul_s      := ( x === BitPat("b0001000??????????????????1010011") )
    info.fpu_isa.fdiv_s      := ( x === BitPat("b0001100??????????????????1010011") )
    info.fpu_isa.fsqrt_s     := ( x === BitPat("b010110000000?????????????1010011") )
    info.fpu_isa.fsgnj_s     := ( x === BitPat("b0010000??????????000?????1010011") )
    info.fpu_isa.fsgnjn_s    := ( x === BitPat("b0010000??????????001?????1010011") )
    info.fpu_isa.fsgnjx_s    := ( x === BitPat("b0010000??????????010?????1010011") )
    info.fpu_isa.fmin_s      := ( x === BitPat("b0010100??????????000?????1010011") )
    info.fpu_isa.fmax_s      := ( x === BitPat("b0010100??????????001?????1010011") )
    info.fpu_isa.fcvt_w_s    := ( x === BitPat("b110000000000?????????????1010011") )
    info.fpu_isa.fcvt_wu_s   := ( x === BitPat("b110000000001?????????????1010011") )
    info.fpu_isa.fmv_x_w     := ( x === BitPat("b111000000000?????000?????1010011") )
    info.fpu_isa.feq_s       := ( x === BitPat("b1010000??????????010?????1010011") )
    info.fpu_isa.flt_s       := ( x === BitPat("b1010000??????????001?????1010011") )
    info.fpu_isa.fle_s       := ( x === BitPat("b1010000??????????000?????1010011") )
    info.fpu_isa.fclass_s    := ( x === BitPat("b111000000000?????001?????1010011") )
    info.fpu_isa.fcvt_s_w    := ( x === BitPat("b110100000000?????????????1010011") )
    info.fpu_isa.fcvt_s_wu   := ( x === BitPat("b110100000001?????????????1010011") )
    info.fpu_isa.fmv_w_x     := ( x === BitPat("b111100000000?????000?????1010011") )
    info.fpu_isa.fcvt_l_s    := ( x === BitPat("b110000000010?????????????1010011") )
    info.fpu_isa.fcvt_lu_s   := ( x === BitPat("b110000000011?????????????1010011") )
    info.fpu_isa.fcvt_s_l    := ( x === BitPat("b110100000010?????????????1010011") )
    info.fpu_isa.fcvt_s_lu   := ( x === BitPat("b110100000011?????????????1010011") )


    info.lsu_isa.fld         := ( x === BitPat("b?????????????????011?????0000111") )
    info.lsu_isa.fsd         := ( x === BitPat("b?????????????????011?????0100111") )
    info.fpu_isa.fmadd_d     := ( x === BitPat("b?????01??????????????????1000011") )
    info.fpu_isa.fmsub_d     := ( x === BitPat("b?????01??????????????????1000111") )
    info.fpu_isa.fnmsub_d    := ( x === BitPat("b?????01??????????????????1001011") )
    info.fpu_isa.fnmadd_d    := ( x === BitPat("b?????01??????????????????1001111") )
    info.fpu_isa.fadd_d      := ( x === BitPat("b0000001??????????????????1010011") )
    info.fpu_isa.fsub_d      := ( x === BitPat("b0000101??????????????????1010011") )
    info.fpu_isa.fmul_d      := ( x === BitPat("b0001001??????????????????1010011") )
    info.fpu_isa.fdiv_d      := ( x === BitPat("b0001101??????????????????1010011") )
    info.fpu_isa.fsqrt_d     := ( x === BitPat("b010110100000?????????????1010011") )
    info.fpu_isa.fsgnj_d     := ( x === BitPat("b0010001??????????000?????1010011") )
    info.fpu_isa.fsgnjn_d    := ( x === BitPat("b0010001??????????001?????1010011") )
    info.fpu_isa.fsgnjx_d    := ( x === BitPat("b0010001??????????010?????1010011") )
    info.fpu_isa.fmin_d      := ( x === BitPat("b0010101??????????000?????1010011") )
    info.fpu_isa.fmax_d      := ( x === BitPat("b0010101??????????001?????1010011") )
    info.fpu_isa.fcvt_s_d    := ( x === BitPat("b010000000001?????????????1010011") )
    info.fpu_isa.fcvt_d_s    := ( x === BitPat("b010000100000?????????????1010011") )
    info.fpu_isa.feq_d       := ( x === BitPat("b1010001??????????010?????1010011") )
    info.fpu_isa.flt_d       := ( x === BitPat("b1010001??????????001?????1010011") )
    info.fpu_isa.fle_d       := ( x === BitPat("b1010001??????????000?????1010011") )
    info.fpu_isa.fclass_d    := ( x === BitPat("b111000100000?????001?????1010011") )
    info.fpu_isa.fcvt_w_d    := ( x === BitPat("b110000100000?????????????1010011") )
    info.fpu_isa.fcvt_wu_d   := ( x === BitPat("b110000100001?????????????1010011") )
    info.fpu_isa.fcvt_d_w    := ( x === BitPat("b110100100000?????????????1010011") )
    info.fpu_isa.fcvt_d_wu   := ( x === BitPat("b110100100001?????????????1010011") )
    info.fpu_isa.fcvt_l_d    := ( x === BitPat("b110000100010?????????????1010011") )
    info.fpu_isa.fcvt_lu_d   := ( x === BitPat("b110000100011?????????????1010011") )
    info.fpu_isa.fmv_x_d     := ( x === BitPat("b111000100000?????000?????1010011") )
    info.fpu_isa.fcvt_d_l    := ( x === BitPat("b110100100010?????????????1010011") )
    info.fpu_isa.fcvt_d_lu   := ( x === BitPat("b110100100011?????????????1010011") )
    info.fpu_isa.fmv_d_x     := ( x === BitPat("b111100100000?????000?????1010011") )      
  } else {
    info.lsu_isa.flw := false.B
    info.lsu_isa.fsw := false.B
    info.lsu_isa.fld := false.B
    info.lsu_isa.fsd := false.B
    info.fpu_isa     := 0.U.asTypeOf(new Fpu_isa)
  }
}



class Decode32(implicit p: Parameters) extends Decode32Base with Decode32G with Decode32FD with VDecode32{


} 



object Decode32 {
  def apply(x:UInt, pc: UInt, hasFpu: Boolean)(implicit p: Parameters): Info_instruction = {
    // val info = Wire(new Info_instruction)
    val dec32 = Module(new Decode32)
    dec32.io.x := x
    dec32.io.pc := pc
    return dec32.io.info
  }
}






