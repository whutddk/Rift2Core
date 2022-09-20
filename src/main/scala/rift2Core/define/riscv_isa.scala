

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

package rift2Core.define

import chisel3._
import chisel3.util._

import rift._
import chipsalliance.rocketchip.config.Parameters





class Alu_isa extends Bundle{
  val lui   = Bool()
  val auipc = Bool()
  val addi  = Bool()
  val addiw = Bool()
  val slti  = Bool()
  val sltiu = Bool()
  val xori  = Bool()
  val ori   = Bool()
  val andi  = Bool()
  val slli  = Bool()
  val slliw = Bool()
  val srli  = Bool()
  val srliw = Bool()
  val srai  = Bool()
  val sraiw = Bool()
  val add   = Bool()
  val addw  = Bool()
  val sub   = Bool()
  val subw  = Bool()
  val sll   = Bool()
  val sllw  = Bool()
  val slt   = Bool()
  val sltu  = Bool()
  val xor   = Bool()
  val srl   = Bool()
  val srlw  = Bool()
  val sra   = Bool()
  val sraw  = Bool()
  val or    = Bool()
  val and   = Bool()

  val wfi = Bool()

  def is_32w = addiw | addw | subw | slliw | sllw | srliw | srlw | sraiw | sraw;
  def is_usi = sltiu | sltu
  def is_imm =  lui | auipc | addi | addiw | slti | sltiu | xori | ori | andi | slli | slliw | srli | srliw | srai | sraiw

  def is_fun_add = addi | addiw | add | addw | lui | auipc | sub | subw | wfi
  def is_fun_slt = slti | sltiu | slt | sltu
  def is_fun_xor = xori | xor
  def is_fun_or  = ori | or
  def is_fun_and = andi | and
  def is_fun_sll = slli | slliw | sll | sllw
  def is_fun_srl = srli | srliw | srl | srlw
  def is_fun_sra = srai | sraiw | sra | sraw

  def is_alu = lui | auipc | addi | addiw | slti | sltiu | xori | ori | andi | slli | slliw | srli | srliw | srai | sraiw | add | addw | sub | subw | sll | sllw | slt | sltu | xor | srl | srlw | sra | sraw | or |and |wfi

} 

class Bru_isa extends Bundle {
  val jal  = Bool()
  val jalr = Bool()
  val beq  = Bool()
  val bne  = Bool()
  val blt  = Bool()
  val bge  = Bool()
  val bltu = Bool()
  val bgeu = Bool()

  def is_bru = jal | jalr | beq | bne | blt | bge | bltu | bgeu
  def is_branch = beq | bne | blt | bge | bltu | bgeu
}

class Lsu_isa extends Bundle {
  val lb  = Bool()
  val lh  = Bool()
  val lw  = Bool()
  val ld  = Bool()
  val lbu = Bool()
  val lhu = Bool()
  val lwu = Bool()
  val sb  = Bool()
  val sh  = Bool()
  val sw  = Bool()
  val sd  = Bool()
  val fence = Bool()
  val fence_i = Bool()
  val sfence_vma = Bool()

  val lr_w      = Bool()
  val sc_w      = Bool()
  val amoswap_w = Bool()
  val amoadd_w  = Bool()
  val amoxor_w  = Bool()
  val amoand_w  = Bool()
  val amoor_w   = Bool()
  val amomin_w  = Bool()
  val amomax_w  = Bool()
  val amominu_w = Bool()
  val amomaxu_w = Bool()
  val lr_d      = Bool()
  val sc_d      = Bool()
  val amoswap_d = Bool()
  val amoadd_d  = Bool()
  val amoxor_d  = Bool()
  val amoand_d  = Bool()
  val amoor_d   = Bool()
  val amomin_d  = Bool()
  val amomax_d  = Bool()
  val amominu_d = Bool()
  val amomaxu_d = Bool()

  val flw = Bool()
  val fsw = Bool()
  val fld = Bool()
  val fsd = Bool()

  def is_sc = sc_d | sc_w
  def is_lr = lr_d | lr_w

  def is_lu  = lb | lh | lw | ld | lbu | lhu | lwu |  flw | fld | is_lr
  def is_su  = sb | sh | sw | sd | fsw | fsd 
  def is_nls = lb | lh | lw | ld | lbu | lhu | lwu | sb | sh | sw | sd
  def is_lrsc = is_sc | is_lr
  def is_amo =
    amoswap_w | amoadd_w | amoxor_w | amoand_w | amoor_w | amomin_w | amomax_w | amominu_w | amomaxu_w | amoswap_d | amoadd_d | amoxor_d | amoand_d | amoor_d | amomin_d | amomax_d | amominu_d | amomaxu_d | is_sc
  def is_fls = flw | fsw | fld | fsd
  def is_fence = fence | fence_i | sfence_vma
  def is_lsu = is_nls | is_lrsc | is_amo | is_fls | is_fence

  def is_byte = lb | lbu | sb
  def is_half = lh | lhu | sh
  def is_word = lw | lwu | sw | amoswap_w | amoadd_w | amoxor_w | amoand_w | amoor_w | amomin_w | amomax_w | amominu_w | amomaxu_w | flw | fsw | lr_w | sc_w
  def is_dubl = ld | lr_d | fld | sd | sc_d | fsd | amoswap_d | amoadd_d | amoxor_d | amoand_d | amoor_d | amomin_d | amomax_d | amominu_d | amomaxu_d

  def is_usi = lbu | lhu | lwu


  def is_R = is_lu | is_lr | is_amo
  def is_W = is_su | is_sc | is_amo

  def is_fst = fsw | fsd
  def is_ist = ~is_fst
  def is_fwb = flw | fld
  def is_iwb = ~is_fwb
  def is_fpu = flw | fsw | fld | fsd
}

class Csr_isa extends Bundle {
  val rw  = Bool()
  val rs  = Bool()
  val rc  = Bool()
  val rwi = Bool()
  val rsi = Bool()
  val rci = Bool()

  def is_csr = rw | rs | rc | rwi | rsi | rci

  
}

class Mul_isa extends Bundle {
  val mul     = Bool()
  val mulh    = Bool()
  val mulhsu = Bool()
  val mulhu   = Bool()
  val div     = Bool()
  val divu    = Bool()
  val rem     = Bool()
  val remu    = Bool()
  val mulw    = Bool()
  val divw    = Bool()
  val divuw   = Bool()
  val remw    = Bool()
  val remuw   = Bool()

  def isDiv32w = divw | divuw | remw  | remuw
  def isDivusi = divu | remu  | divuw | remuw


  def isMul = mul | mulh | mulhsu | mulhu | mulw
  def isDiv = div | divu | divw | divuw | rem | remu | remw | remuw

  def is_mulDiv = isMul | isDiv



}


class Privil_isa extends Bundle {
  val ecall = Bool()
  val ebreak = Bool()
  val mret = Bool()

  val uret = Bool()
  val sret = Bool()
  val dret = Bool()





  val hfence_vvma = Bool()
  val hfence_gvma = Bool()

  val hlv_b = Bool()
  val hlv_bu = Bool()
  val hlv_h = Bool()
  val hlv_hu = Bool()
  val hlvx_hu = Bool()
  val hlv_w = Bool()
  val hlvx_wu = Bool()
  val hsv_b = Bool()
  val hsv_h = Bool()
  val hsv_w = Bool()

  val hlv_wu = Bool()
  val hlv_d = Bool()
  val hsv_d = Bool()

  val is_access_fault = Bool()
  val is_paging_fault = Bool()

  def is_privil = 
    ecall | ebreak | mret | uret | sret | dret | hfence_vvma | hfence_gvma | hlv_b | hlv_bu | hlv_h | hlv_hu | hlvx_hu | hlv_w | hlvx_wu | hsv_b | hsv_h | hsv_w | hlv_wu | hlv_d | hsv_d |
    is_access_fault | is_paging_fault


}


class Fpu_isa extends Bundle {

  val fmadd_s = Bool()
  val fmsub_s = Bool()
  val fnmsub_s = Bool()
  val fnmadd_s = Bool()
  val fadd_s = Bool()
  val fsub_s = Bool()
  val fmul_s = Bool()
  val fdiv_s = Bool()
  val fsqrt_s = Bool()
  val fsgnj_s = Bool()
  val fsgnjn_s = Bool()
  val fsgnjx_s = Bool()
  val fmin_s = Bool()
  val fmax_s = Bool()
  val fcvt_w_s = Bool()
  val fcvt_wu_s = Bool()
  val fmv_x_w = Bool()
  val feq_s = Bool()
  val flt_s = Bool()
  val fle_s = Bool()
  val fclass_s = Bool()
  val fcvt_s_w = Bool()
  val fcvt_s_wu = Bool()
  val fmv_w_x = Bool()

  val fcvt_l_s = Bool()
  val fcvt_lu_s = Bool()
  val fcvt_s_l = Bool()
  val fcvt_s_lu = Bool()


  val fmadd_d = Bool()
  val fmsub_d = Bool()
  val fnmsub_d = Bool()
  val fnmadd_d = Bool()
  val fadd_d = Bool()
  val fsub_d = Bool()
  val fmul_d = Bool()
  val fdiv_d = Bool()
  val fsqrt_d = Bool()
  val fsgnj_d = Bool()
  val fsgnjn_d = Bool()
  val fsgnjx_d = Bool()
  val fmin_d = Bool()
  val fmax_d = Bool()
  val fcvt_s_d = Bool()
  val fcvt_d_s = Bool()
  val feq_d = Bool()
  val flt_d = Bool()
  val fle_d = Bool()
  val fclass_d = Bool()
  val fcvt_w_d = Bool()
  val fcvt_wu_d = Bool()
  val fcvt_d_w = Bool()
  val fcvt_d_wu = Bool()

  val fcvt_l_d = Bool()
  val fcvt_lu_d = Bool()
  val fmv_x_d = Bool()
  val fcvt_d_l = Bool()
  val fcvt_d_lu = Bool()
  val fmv_d_x = Bool()

  val fcsr_rw = Bool()
  val fcsr_rs = Bool()
  val fcsr_rc = Bool()

  val fcsr_rwi = Bool()
  val fcsr_rsi = Bool()
  val fcsr_rci = Bool()

  def is_fun_frw = fcsr_rw | fcsr_rwi
  def is_fun_frs = fcsr_rs | fcsr_rsi
  def is_fun_frc = fcsr_rc | fcsr_rci

  def is_fun_fcsri = fcsr_rwi | fcsr_rsi | fcsr_rci

  def is_fun_fcsr =   
    is_fun_frw | is_fun_frs | is_fun_frc


  def is_fun_class = fclass_s | fclass_d
  def is_fun_fcmp =  feq_s | flt_s | fle_s | feq_d | flt_d | fle_d
  def is_fun_maxMin = fmin_s | fmax_s | fmin_d | fmax_d
  def is_fun_fcvtX = 
    fcvt_w_s | fcvt_wu_s | fcvt_l_s | fcvt_lu_s |
    fcvt_w_d | fcvt_wu_d | fcvt_l_d | fcvt_lu_d 
  def is_fun_xcvtF = 
    fcvt_s_w | fcvt_s_wu | fcvt_s_l | fcvt_s_lu |
    fcvt_d_w | fcvt_d_wu | fcvt_d_l | fcvt_d_lu 
  def is_fun_fcvtF = 
    fcvt_s_d | fcvt_d_s
  def is_fun_fmvX =
    fmv_x_w | fmv_x_d
  def is_fun_xmvF =
    fmv_w_x | fmv_d_x
  def is_fun_fsgn = 
    fsgnj_s | fsgnjn_s | fsgnjx_s |
    fsgnj_d | fsgnjn_d | fsgnjx_d
  def is_fun_fma = 
    fmadd_s  | fmsub_s  | fnmsub_s | fnmadd_s | fadd_s   | fsub_s   | fmul_s   |
    fmadd_d  | fmsub_d  | fnmsub_d | fnmadd_d | fadd_d   | fsub_d   | fmul_d
  def is_fun_divSqrt = 
    fdiv_s  | fdiv_d  | fsqrt_s | fsqrt_d
    
  

  def hasThreeRs = 
  fmadd_s | fmsub_s | fnmsub_s | fnmadd_s | 
  fmadd_d | fmsub_d | fnmsub_d | fnmadd_d

  def hasTwoRs = hasThreeRs |
  fadd_s | fsub_s | fmul_s | fdiv_s | fsqrt_s |
  fsgnj_s | fsgnjn_s | fsgnjx_s | fsgnj_d | fsgnjn_d | fsgnjx_d |
  fmin_s | fmax_s | fmin_d | fmax_d |
  feq_s | flt_s |fle_s | feq_d | flt_d | fle_d |
  fadd_d | fsub_d | fmul_d | fdiv_d | fsqrt_d |
  fcsr_rw | fcsr_rs | fcsr_rc | fcsr_rwi | fcsr_rsi | fcsr_rci

  def hasOneRs = hasTwoRs |
  fcvt_w_s | fcvt_wu_s |
  fmv_x_w  | fmv_x_d |
  fclass_s | fclass_d |
  fcvt_s_w | fcvt_s_wu |
  fmv_w_x  | fmv_d_x |
  fcvt_l_s | fcvt_lu_s |
  fcvt_s_l | fcvt_s_lu |
  fcvt_s_d | fcvt_d_s |
  fcvt_w_d | fcvt_wu_d |
  fcvt_d_w | fcvt_d_wu |
  fcvt_l_d | fcvt_lu_d |
  fcvt_d_l | fcvt_d_lu

  def is_fop = 
    fmadd_s   | fmsub_s   | fnmsub_s  | fnmadd_s  |
    fmadd_d   | fmsub_d   | fnmsub_d  | fnmadd_d  |    
    fadd_s    | fsub_s    | fmul_s    | fdiv_s    | fsqrt_s   |
    fadd_d    | fsub_d    | fmul_d    | fdiv_d    | fsqrt_d   |    
    fsgnj_s   | fsgnjn_s  | fsgnjx_s  |
    fsgnj_d   | fsgnjn_d  | fsgnjx_d  |
    fmin_s    | fmax_s    | fmin_d    | fmax_d    |
    fmv_x_w   | fmv_x_d   |
    feq_s     | flt_s     | fle_s     |
    feq_d     | flt_d     | fle_d     |
    fclass_s  | fclass_d  |
    fcvt_w_s  | fcvt_wu_s | fcvt_l_s  | fcvt_lu_s |
    fcvt_w_d  | fcvt_wu_d | fcvt_l_d  | fcvt_lu_d |
    fcvt_s_d  | fcvt_d_s

  def is_iwb = 
    feq_s | flt_s | fle_s | feq_d | flt_d | fle_d |
    fmv_x_w | fmv_x_d | fclass_s | fclass_d |
    fcvt_w_s | fcvt_wu_s | fcvt_l_s | fcvt_lu_s | fcvt_w_d | fcvt_wu_d | fcvt_l_d | fcvt_lu_d |
    fmv_x_w | fmv_x_d |
    fcsr_rw | fcsr_rs | fcsr_rc | fcsr_rwi | fcsr_rsi | fcsr_rci

  def is_fwb =
    fmadd_s | fmsub_s | fnmsub_s | fnmadd_s | fadd_s | fsub_s | fmul_s | fdiv_s | fsqrt_s |
    fmadd_d | fmsub_d | fnmsub_d | fnmadd_d | fadd_d | fsub_d | fmul_d | fdiv_d | fsqrt_d |
    fsgnj_s | fsgnjn_s | fsgnjx_s |
    fsgnj_d | fsgnjn_d | fsgnjx_d |
    fmin_s | fmax_s | fmin_d | fmax_d |
    fcvt_s_w | fcvt_s_wu | fcvt_d_w | fcvt_d_wu |
    fcvt_s_l | fcvt_s_lu | fcvt_d_l | fcvt_d_lu |
    fcvt_s_d | fcvt_d_s |
    fmv_w_x | fmv_d_x
    


  def is_usi =
    fcvt_s_wu | fcvt_s_lu| fcvt_d_wu | fcvt_d_lu | fcvt_wu_s | fcvt_lu_s | fcvt_wu_d | fcvt_lu_d


  def FtypeTagIn = {
    def FtypeTagIn_0 =
    fmadd_s   | fmsub_s   | fnmsub_s  | fnmadd_s  |  
    fadd_s    | fsub_s    | fmul_s    | fdiv_s    | fsqrt_s   | 
    fsgnj_s   | fsgnjn_s  | fsgnjx_s  |
    fmin_s    | fmax_s    |
    feq_s     | flt_s     | fle_s     |
    fclass_s  |
    fcvt_w_s  | fcvt_wu_s | fcvt_l_s  | fcvt_lu_s |
    fcvt_d_s

    def FtypeTagIn_1 =
    fmadd_d   | fmsub_d   | fnmsub_d  | fnmadd_d  |    
    fadd_d    | fsub_d    | fmul_d    | fdiv_d    | fsqrt_d   |    
    fsgnj_d   | fsgnjn_d  | fsgnjx_d  |
    fmin_d    | fmax_d    |
    fmv_x_w   | fmv_x_d   |
    feq_d     | flt_d     | fle_d     |
    fclass_d  |
    fcvt_w_d  | fcvt_wu_d | fcvt_l_d  | fcvt_lu_d |
    fcvt_s_d
    
    Mux1H(Seq(
      FtypeTagIn_0 -> 0.U, FtypeTagIn_1 -> 1.U,
    ))
  }


  def FtypeTagOut = {
    def FtypeTagOut_0 =
      fmadd_s | fmsub_s | fnmsub_s | fnmadd_s |
      fadd_s | fsub_s | fmul_s |
      fsgnj_s | fsgnjn_s | fsgnjx_s |
      fmin_s | fmax_s |
      fcvt_s_w | fcvt_s_wu | fcvt_s_l | fcvt_s_lu |
      fcvt_s_d

    def FtypeTagOut_1 =
      fmadd_d | fmsub_d | fnmsub_d | fnmadd_d |
      fadd_d | fsub_d | fmul_d | 
      fsgnj_d | fsgnjn_d | fsgnjx_d |
      fmin_d | fmax_d |
      fmv_d_x   | fmv_w_x   |
      fcvt_d_w | fcvt_d_wu | fcvt_d_l | fcvt_d_lu |
      fcvt_d_s
    
    Mux1H(Seq(
      FtypeTagOut_0 -> 0.U, FtypeTagOut_1 -> 1.U,
    ))
  }

  def XtypeTagIn = {
    def XtypeTagIn_0 =
      fmv_w_x   |
      fcvt_s_w | fcvt_s_wu | fcvt_d_w | fcvt_d_wu 

    def XtypeTagIn_1 =
      fmv_d_x   |
      fcvt_s_l | fcvt_s_lu | fcvt_d_l | fcvt_d_lu
    
    Mux1H(Seq(
      XtypeTagIn_0 -> 0.U, XtypeTagIn_1 -> 1.U,
    ))
  }

  def XtypeTagOut = {
    def XtypeTagOut_0 =
    fmv_x_w |  
    fcvt_w_s | fcvt_wu_s | fcvt_w_d | fcvt_wu_d

    def XtypeTagOut_1 =
    feq_s | flt_s | fle_s | feq_d | flt_d | fle_d |
    fmv_x_d | 
    fcvt_l_s | fcvt_lu_s | fcvt_l_d | fcvt_lu_d
    
    Mux1H(Seq(
      XtypeTagOut_0 -> 0.U, XtypeTagOut_1 -> 1.U,
    ))
  }




  def is_fpu =
  fmadd_s   | fmsub_s   | fnmsub_s  | fnmadd_s  | fadd_s    | fsub_s    | fmul_s    | fdiv_s    | fsqrt_s   |
  fmadd_d   | fmsub_d   | fnmsub_d  | fnmadd_d  | fadd_d    | fsub_d    | fmul_d    | fdiv_d    | fsqrt_d   |
  fsgnj_s   | fsgnjn_s  | fsgnjx_s  | fsgnj_d   | fsgnjn_d  | fsgnjx_d  |
  fmin_s    | fmax_s    | fmin_d    | fmax_d    |
  feq_s     | flt_s     | fle_s     | feq_d     | flt_d     | fle_d     |
  fclass_s  | fclass_d  |
  fmv_x_w   | fmv_w_x   | fmv_x_d   | fmv_d_x   |  
  fcvt_s_w  | fcvt_s_wu | fcvt_l_s  | fcvt_lu_s |
  fcvt_s_l  | fcvt_s_lu | fcvt_s_d  | fcvt_d_s  |
  fcvt_w_d  | fcvt_wu_d | fcvt_d_w  | fcvt_d_wu | fcvt_l_d  | fcvt_lu_d |
  fcvt_d_l  | fcvt_d_lu | fcvt_w_s  | fcvt_wu_s |
  fcsr_rw   | fcsr_rs   | fcsr_rc   | fcsr_rwi  | fcsr_rsi  | fcsr_rci


}


// class Register_source(dp:Int)(implicit p: Parameters) extends RiftBundle {
//   val rs1 = UInt((log2Ceil(dp)).W)
//   val rs2 = UInt((log2Ceil(dp)).W)
//   val rs3 = UInt((log2Ceil(dp)).W)
// }

// class Register_dstntn()(implicit p: Parameters) extends RiftBundle {
//   val rd0 = UInt((log2Ceil(regNum)).W)
// }

class RS_RAW(implicit p: Parameters) extends RiftBundle {
  val rs1 = UInt((log2Ceil(32)).W)
  val rs2 = UInt((log2Ceil(32)).W)
  val rs3 = UInt((log2Ceil(32)).W)
}

class RS_PHY(implicit p: Parameters) extends RiftBundle {
  val rs1 = UInt((log2Ceil(regNum)).W)
  val rs2 = UInt((log2Ceil(regNum)).W)
  val rs3 = UInt((log2Ceil(regNum)).W)
}

class RD_RAW(implicit p: Parameters) extends RiftBundle {
  val rd0 = UInt((log2Ceil(32)).W)
}

class RD_PHY(implicit p: Parameters) extends RiftBundle {
  val rd0 = UInt((log2Ceil(regNum)).W)
}

class Operation_source(dw: Int)(implicit p: Parameters) extends RiftBundle {
  val op1 = UInt(dw.W)
  val op2 = UInt(dw.W)
  val op3 = UInt(dw.W)
}

class Reg_RAW(implicit p: Parameters) extends RiftBundle {
  val rs1 = UInt((log2Ceil(32)).W)
  val rs2 = UInt((log2Ceil(32)).W)
  val rs3 = UInt((log2Ceil(32)).W)
  val rd0 = UInt((log2Ceil(32)).W)
}

class Reg_PHY(implicit p: Parameters) extends RiftBundle {
  val rs1 = UInt((log2Ceil(regNum)).W)
  val rs2 = UInt((log2Ceil(regNum)).W)
  val rs3 = UInt((log2Ceil(regNum)).W)
  val rd0 = UInt((log2Ceil(regNum)).W)
}



class Instruction_set(implicit p: Parameters) extends RiftBundle{
  val alu_isa = new Alu_isa
  val bru_isa = new Bru_isa
  val lsu_isa = new Lsu_isa
  val csr_isa = new Csr_isa
  val mul_isa = new Mul_isa
  val privil_isa = new Privil_isa
  val fpu_isa = new Fpu_isa

  def is_fwb =
    lsu_isa.is_fwb | fpu_isa.is_fwb

  def is_ooo_dpt = alu_isa.is_alu | mul_isa.is_mulDiv
  def is_ito_dpt = bru_isa.is_bru | csr_isa.is_csr | lsu_isa.is_lsu | fpu_isa.is_fpu
  def is_privil_dpt = privil_isa.is_privil
  def is_fpu_dpt = fpu_isa.is_fpu
  def is_iwb = ~is_fwb
  def is_illeage = ~(alu_isa.is_alu | bru_isa.is_bru | lsu_isa.is_lsu | csr_isa.is_csr | mul_isa.is_mulDiv | privil_isa.is_privil | fpu_isa.is_fpu) 

  def dptRegion = MuxCase( 0.U, Array(
    alu_isa.is_alu    -> 1.U,
    mul_isa.is_mulDiv -> 2.U,
    bru_isa.is_bru    -> 3.U,
    csr_isa.is_csr    -> 4.U,
    lsu_isa.is_lsu    -> 5.U,
    fpu_isa.is_fpu    -> 6.U,
  ))
}



class Instruction_param(implicit p: Parameters) extends RiftBundle {
  val is_rvc = Bool()
  val pc = UInt(vlen.W)
  
  val imm = UInt(64.W)
  val rm = UInt(3.W)
  val raw = new Reg_RAW

}

class Info_instruction(implicit p: Parameters) extends Instruction_set {
  val param = new Instruction_param

}


class Dpt_info(implicit p: Parameters) extends Info_instruction {
  val phy = new Reg_PHY
}



class Alu_function(implicit p: Parameters) extends RiftBundle {

  val add = Bool()
  val slt = Bool()
  val xor = Bool()
  val or  = Bool()
  val and = Bool()
  val sll = Bool()
  val srl = Bool()
  val sra = Bool()
}

class Alu_param(implicit p: Parameters) extends RD_PHY {
  val is_32w = Bool()
  val is_usi = Bool()

  val dat = new Operation_source(dw=64)

  // override def cloneType = ( new Alu_param ).asInstanceOf[this.type]
}

class Alu_iss_info(implicit p: Parameters) extends RiftBundle {
  val fun = new Alu_function
  val param = new Alu_param
}





class Bru_param(implicit p: Parameters) extends RD_PHY {
  val is_rvc = Bool()
  val pc = UInt(64.W)
  val imm = UInt(64.W)

  val dat = new Operation_source(dw=64)

  // override def cloneType = ( new Bru_param ).asInstanceOf[this.type]
}

class Bru_iss_info(implicit p: Parameters) extends RiftBundle {
  val fun = new Bru_isa
  val param = new Bru_param
}





class Lsu_param(implicit p: Parameters) extends RD_PHY {
  val dat = new Operation_source(dw=64)

  // override def cloneType = ( new Lsu_param ).asInstanceOf[this.type]
}

class Lsu_iss_info(implicit p: Parameters) extends RiftBundle {
  val fun = new Lsu_isa
  val param = new Lsu_param

  def is_misAlign =
    Mux1H( Seq(
      fun.is_half -> (param.dat.op1(0) =/= 0.U),
      fun.is_word -> (param.dat.op1(1,0) =/= 0.U),
      fun.is_dubl -> (param.dat.op1(2,0) =/= 0.U)	
    ))

  def paddr = param.dat.op1

  def wdata_align(dw: Int) = {
      val res = Wire(UInt(dw.W))
      res := param.dat.op2 << ( paddr((log2Ceil(dw/8)-1),0) << 3 )
      res
    }

  def wstrb_align(dw: Int) = {
    val wstrb = Wire(UInt((dw/8).W))
    wstrb := Mux1H(Seq(
        fun.is_byte -> "b00000001".U, fun.is_half -> "b00000011".U,
        fun.is_word -> "b00001111".U, fun.is_dubl -> "b11111111".U
      )) << paddr((log2Ceil(dw/8)-1),0)
    wstrb
  }
}








class Fpu_dpt_info(implicit p: Parameters) extends RiftBundle {
  val isa = new Fpu_isa
  val param = new Instruction_param
  val phy = new Reg_PHY
}





class Info_reorder_i(implicit p: Parameters) extends RiftBundle {
  val pc = UInt(vlen.W)
  val rd0_raw = UInt(5.W)
  val rd0_phy = UInt((log2Ceil(regNum)).W)

  val is_branch = Bool()
  val is_jalr = Bool()
  val is_lu = Bool()
  val is_su = Bool()
  val is_amo = Bool()
  val is_fence = Bool()
  val is_fence_i = Bool()
  val is_sfence_vma = Bool()
  val is_wfi = Bool()
  val is_csr = Bool()
  val is_fpu = Bool()
  val is_fcsr = Bool()
  val is_rvc = Bool()

  val is_xcmm = Bool()
  val is_fcmm = Bool()

  val privil = new Privil_isa
  val is_illeage = Bool()

}





class Csr_function(implicit p: Parameters) extends RiftBundle {
  val rw  = Bool()
  val rs  = Bool()
  val rc  = Bool()
}

class Csr_param(implicit p: Parameters) extends RD_PHY {
  val dat = new Operation_source(dw=64)

  // override def cloneType = ( new Csr_param ).asInstanceOf[this.type]
}

class Csr_iss_info(implicit p: Parameters) extends RiftBundle {
  val fun = new Csr_function
  val param = new Csr_param
}

class Mul_param(implicit p: Parameters) extends RD_PHY {
  val dat = new Operation_source(dw=64)

// override def cloneType = ( new Mul_param ).asInstanceOf[this.type]
}

class Mul_iss_info(implicit p: Parameters) extends RiftBundle {
  val fun = new Mul_isa
  val param = new Mul_param
}



case class WriteBack_info(dw:Int)(implicit p: Parameters) extends RD_PHY {
  val res = UInt(dw.W)

}





class Info_cmm_csr(implicit p: Parameters) extends RiftBundle {
  val is_trap = Bool()
  val is_xRet = Bool()
  val privil_mstatus = UInt(64.W)
  val privil_mepc = UInt(64.W)
  val privil_mcause = UInt(64.W)
  val privil_mtval = UInt(64.W)
}

class Info_clint_csr(implicit p: Parameters) extends RiftBundle {
  val is_externInterrupt = Bool()
  val is_rtimerInterrupt = Bool()
  val is_softwvInterrupt = Bool()
}
















class Info_cmm_lsu(implicit p: Parameters) extends RiftBundle {
  val is_amo_pending = Bool()
  val is_store_commit = Vec(cm_chn, Bool())
}

class Info_lsu_cmm(implicit p: Parameters) extends RiftBundle {
  val is_access_fault = Bool()
  val is_paging_fault = Bool()
  val is_misAlign = Bool()
  val trap_addr = UInt(64.W)
}

class Stq_req_Bundle(implicit p: Parameters) extends RiftBundle {
  val paddr = UInt(plen.W)
}

class Stq_resp_Bundle(implicit p: Parameters) extends RiftBundle {
  val wdata = UInt(64.W)
  val wstrb = UInt(8.W)
}



class Commit_Redirect_Bundle(implicit p: Parameters) extends RiftBundle{
  val pc = UInt(64.W)
}

class PreFetch_Req_Bundle(implicit p: Parameters) extends RiftBundle {
  val paddr = UInt(plen.W)
}


class Info_if_cmm(implicit p: Parameters) extends RiftBundle {
  val ill_vaddr = UInt(64.W)
}

class AClint_Bundle extends Bundle {
  val msi = Bool()
  val mti = Bool()

  val ssi = Bool()
  val sti = Bool()
}

class Plic_Bundle extends Bundle {
  val mei = Bool()
  val sei = Bool()
}
