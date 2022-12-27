

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

import rift2Chip._
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





class VectorIsa extends Bundle {
  class OPI extends Bundle{
    val ivv = Bool()
    val ivx = Bool()
    val ivi = Bool()

    // def sel = ivv | ivx | ivi
  }

  class OPM extends Bundle{
    val mvv = Bool()
    val mvx = Bool()

    // def sel = mvv | mvx 
    def mrd = 
    def mvd = 
  }

  class OPF extends Bundle{
    val fvv = Bool()
    val fvf = Bool()

    // def sel = fvv | fvf
    def fvd = 
    def frd = 
  }

  val vadd         = new OPI
  val vsub         = new OPI
  val vrsub        = new OPI
  val vminu        = new OPI
  val vmin         = new OPI
  val vmaxu        = new OPI
  val vmax         = new OPI
  val vand         = new OPI
  val vor          = new OPI
  val vxor         = new OPI
  val vrgather     = new OPI
  val vslideup     = new OPI
  val vrgatherei16 = new OPI
  val vslidedown   = new OPI
  val vredsum      = new OPM
  val vredand      = new OPM
  val vredor       = new OPM
  val vredxor      = new OPM
  val vredminu     = new OPM
  val vredmin      = new OPM
  val vredmaxu     = new OPM
  val vredmax      = new OPM
  val vaaddu       = new OPM
  val vaadd        = new OPM
  val vasubu       = new OPM
  val vasub        = new OPM
  val vslide1up    = new OPM
  val vslide1down  = new OPM
  val vfadd        = new OPF
  val vfredusum    = new OPF
  val vfsub        = new OPF
  val vfredosum    = new OPF
  val vfmin        = new OPF
  val vfredmin     = new OPF
  val vfmax        = new OPF
  val vfredmax     = new OPF
  val vfsgnj       = new OPF
  val vfsgnjn      = new OPF
  val vfsgnjx      = new OPF
  val vfslide1up   = new OPF
  val vfslide1down = new OPF

  val vadc      = new OPI
  val vmadc     = new OPI
  val vsbc      = new OPI
  val vmsbc     = new OPI
  val vmerge    = new OPI
  val vmv       = new OPI
  val vmseq     = new OPI
  val vmsne     = new OPI
  val vmsltu    = new OPI
  val vmslt     = new OPI
  val vmsleu    = new OPI
  val vmsle     = new OPI
  val vmsgtu    = new OPI
  val vmsgt     = new OPI
  val vmv_x_s   = Bool()
  val vpopc     = Bool()
  val vfirst    = Bool()
  val vmv_s_x   = Bool()
  val vzext_vf8 = Bool()
  val vsext_vf8 = Bool()
  val vzext_vf4 = Bool()
  val vsext_vf4 = Bool()
  val vzext_vf2 = Bool()
  val vsext_vf2 = Bool()
  val vmsbf = Bool()
  val vmsof = Bool()
  val vmsif = Bool()
  val viota = Bool()
  val vid   = Bool()
  val vcompress = new OPM
  val vmandnot  = new OPM
  val vmand     = new OPM
  val vmor      = new OPM
  val vmxor     = new OPM
  val vmornot   = new OPM
  val vmnand    = new OPM
  val vmnor     = new OPM
  val vmxnor    = new OPM
  val vfmv_f_s  = Bool()
  val vfmv_s_f  = Bool()
  val vfcvt_xu_f_v     = Bool()
  val vfcvt_x_f_v      = Bool()
  val vfcvt_f_xu_v     = Bool()
  val vfcvt_f_x_v      = Bool()
  val vfcvt_rtz_xu_f_v = Bool()
  val vfcvt_rtz_x_f_v  = Bool()

  val vfwcvt_xu_f_v     = Bool()
  val vfwcvt_x_f_v      = Bool()
  val vfwcvt_f_xu_v     = Bool()
  val vfwcvt_f_x_v      = Bool()
  val vfwcvt_f_f_v      = Bool()
  val vfwcvt_rtz_xu_f_v = Bool()
  val vfwcvt_rtz_x_f_v  = Bool()

  val vfncvt_xu_f_w     = Bool()
  val vfncvt_x_f_w      = Bool()
  val vfncvt_f_xu_w     = Bool()
  val vfncvt_f_x_w      = Bool()
  val vfncvt_f_f_w      = Bool()
  val vfncvt_rod_f_f_w  = Bool()
  val vfncvt_rtz_xu_f_w = Bool()
  val vfncvt_rtz_x_f_w  = Bool()

  val vfsqrt_v   = Bool()
  val vfrsqrt7_v = Bool()
  val vfrec7_v   = Bool()
  val vfclass_v  = Bool()

  val vfmerge   = new OPF
  val vfmv      = new OPF
  val vmfeq     = new OPF
  val vmfle     = new OPF
  val vmflt     = new OPF
  val vmfne     = new OPF
  val vmfgt     = new OPF
  val vmfge     = new OPF

  val vsaddu  = new OPI
  val vsadd   = new OPI
  val vssubu  = new OPI
  val vssub   = new OPI
  val vsll    = new OPI
  val vsmul   = new OPI
  val vmvnr   = new OPI

  val vsrl    = new OPI
  val vsra    = new OPI
  val vssrl   = new OPI
  val vssra   = new OPI
  val vnsrl   = new OPI
  val vnsra   = new OPI
  val vnclipu = new OPI
  val vnclip  = new OPI
  val vdivu   = new OPM
  val vdiv    = new OPM
  val vremu   = new OPM
  val vrem    = new OPM
  val vmulhu  = new OPM
  val vmul    = new OPM
  val vmulhsu = new OPM
  val vmulh   = new OPM
  val vmadd   = new OPM
  val vnmsub  = new OPM
  val vmacc   = new OPM
  val vnmsac  = new OPM
  val vfdiv   = new OPF
  val vfrdiv  = new OPF
  val vfmul   = new OPF
  val vfrsub  = new OPF
  val vfmadd  = new OPF
  val vfnmadd = new OPF
  val vfmsub  = new OPF
  val vfnmsub = new OPF
  val vfmacc  = new OPF
  val vfnmacc = new OPF
  val vfmsac  = new OPF
  val vfnmsac = new OPF

  val vwredsumu  = new OPI
  val vwredsum   = new OPI
  val vwaddu     = new OPM
  val vwadd      = new OPM
  val vwsubu     = new OPM
  val vwsub      = new OPM
  val vwaddu_w   = new OPM
  val vwadd_w    = new OPM
  val vwsubu_w   = new OPM
  val vwsub_w    = new OPM
  val vwmulu     = new OPM
  val vwmulsu    = new OPM
  val vwmul      = new OPM
  val vwmaccu    = new OPM
  val vwmacc     = new OPM
  val vwmaccus   = new OPM
  val vwmaccsu   = new OPM
  val vfwadd     = new OPF
  val vfwredusum = new OPF
  val vfwsub     = new OPF
  val vfwredosum = new OPF
  val vfwadd_w   = new OPF
  val vfwsub_w   = new OPF
  val vfwmul     = new OPF
  val vfwmacc    = new OPF
  val vfwnmacc   = new OPF
  val vfwmsac    = new OPF
  val vfwnmsac   = new OPF

  val vsetvli  = Bool()
  val vsetivli = Bool()
  val vsetvl   = Bool()

  val vle       = Bool()
  val vlm       = Bool()
  val vlse      = Bool()
  val vluxei    = Bool()
  val vloxei    = Bool()
  val vleNff    = Bool()

  val vlNreN    = Bool()

  val vse       = Bool()
  val vsm       = Bool()
  val vsse      = Bool()
  val vsuxei    = Bool()
  val vsoxei    = Bool()
  val vsNr      = Bool()




  def isVAlu =
    vadd | vsub | vrsub | vminu | vmin | vmaxu | vmax | vand | vor | vxor | 
    vwaddu | vwadd | vwsubu | vwsub | vwaddu_w | vwadd_w  | vwsubu_w | vwsub_w | vadc | vmadc | vsbc | vmsbc |
    vzext_vf8 | vsext_vf8 | vzext_vf4 | vsext_vf4 | vzext_vf2 | vsext_vf2 |
    vsll  | vsrl  | vsra  | vnsrl | vnsra |
    vmseq  | vmsne  | vmsltu | vmslt  | vmsleu | vmsle  | vmsgtu | vmsgt  |
    vmerge | vmv |
    vredsum | vredand | vredor | vredxor | vredminu | vredmin | vredmaxu | vredmax |
    vwredsumu | vwredsum |
    vmand | vmnand | vmandnot | vmxor | vmor | vmnor | vmornot | vmxnor |
    vpopc | vfirst | vmsbf | vmsif | vmsof | viota | vid |
    vmv_x_s | vmv_s_x |
    vslideup | vslidedown | vslide1up | vslide1down |
    vrgather | vrgatherei16 |
    vcompress |
    vmvnr

  def isVMul = 
    vdivu | vdiv  | vremu | vrem  |
    vmulhu | vmul | vmulhsu | vmulh |
    vmadd  | vnmsub | vmacc  | vnmsac |
    vwmulu  | vwmulsu | vwmul   |
    vwmaccu  | vwmacc | vwmaccus | vwmaccsu

  def isVQpu = 
    vsaddu | vsadd | vssubu | vssub |
    vaaddu | vaadd | vasubu | vasub |
    vsmul |
    vssrl | vssra |
    vnclipu | vnclip

  def isVFpu = 
    vfadd | vfsub | vfrsub |
    vfwadd | vfwsub | vfwadd_w | vfwsub_w |
    vfdiv | vfrdiv | vfmul | vfwmul |
    vfmacc | vfnmacc | vfmsac  | vfnmsac | vfmadd  | vfnmadd | vfmsub  | vfnmsub |
    vfwmacc | vfwnmacc | vfwmsac  | vfwnmsac |
    vfsqrt_v | vfrsqrt7_v | vfrec7_v |
    vfmin   | vfmax   | vfsgnj  | vfsgnjn | vfsgnjx |
    vmfeq | vmfle | vmflt | vmfne | vmfgt | vmfge |
    vfclass_v |
    vfmerge | vfmv |
    vfcvt_xu_f_v | vfcvt_x_f_v | vfcvt_f_xu_v | vfcvt_f_x_v | vfcvt_rtz_xu_f_v | vfcvt_rtz_x_f_v | vfwcvt_xu_f_v | vfwcvt_x_f_v | vfwcvt_f_xu_v | vfwcvt_f_x_v | vfwcvt_f_f_v | vfwcvt_rtz_xu_f_v | vfwcvt_rtz_x_f_v | vfncvt_xu_f_w | vfncvt_x_f_w | vfncvt_f_xu_w | vfncvt_f_x_w | vfncvt_f_f_w | vfncvt_rod_f_f_w | vfncvt_rtz_xu_f_w | vfncvt_rtz_x_f_w |
    vfredusum | vfredosum | vfredmin  | vfredmax  |
    vfwredusum | vfwredosum |
    vfmv_f_s | vfmv_s_f |
    vfslide1up | vfslide1down

  def isVload = 
    vle | vlm | vlse | vluxei | vloxei | vleNff | vlNreN

  def isVstore = 
    vse | vsm | vsse | vsuxei | vsoxei | vsNr

  def isVMem = vload | vstore

  def isVConfig = vsetvli | vsetivli | vsetvl








  def isLookUpRS1 = _.ivx | _.mvx | vsetvli | vsetvl | isVload | isVstore | vmv_s_x
  def isLookUpRS2 = _.fvf | vsetvl | vlse | vsse



  def isLookUpFS1 = vfmv_s_f


  def isLookUpVS1 = _.ivv | _.mvv | _.fvv
  def isLookUpVS2 =
    _.ivv | _.ivx | _.ivi | _.mvv | _.mvx | _.fvv | _.fvf |
    vzext_vf8 | vsext_vf8 | vzext_vf4 | vsext_vf4 | vzext_vf2 | vsext_vf2 |
    vluxei | vloxei | vsuxei | vsoxei |
    vfsqrt_v | vfrsqrt7_v | vfrec7_v | vfclass_v |
    vfcvt_xu_f_v | vfcvt_x_f_v | vfcvt_rtz_xu_f_v | vfcvt_rtz_x_f_v | vfcvt_f_xu_v | vfcvt_f_x_v |
    vfwcvt_xu_f_v | vfwcvt_x_f_v | vfwcvt_rtz_xu_f_v | vfwcvt_rtz_x_f_v | vfwcvt_f_xu_v | vfwcvt_f_x_v | vfwcvt_f_f_v |
    vfncvt_xu_f_w | vfncvt_x_f_w | vfncvt_rtz_xu_f_w | vfncvt_rtz_x_f_w | vfncvt_f_xu_w | vfncvt_f_x_w | vfncvt_f_f_w | vfncvt_rod_f_f_w |
    vfirst | vmsbf | vmsif | vmsof | viota |
    vmv_x_s | vfmv_f_s | vpopc
  def isLookUpVS3 =
    vmacc._ | vnmsac._ | vmadd._ | vnmsub._ | vwmaccu._ | vwmacc._ | vwmaccsu._ | vwmaccus._ |
    vfmacc._ | vfnmacc._ | vfmsac._ | vfnmsac._ | vfmadd._ | vfnmadd._ | vfmsub._ | vfnmsub._ | vfwmacc._ | vfwnmacc._ | vfwmsac._ | vfwnmsac._ |
    isVstore

  def isLookUpVS2P =
    vwaddu_w | vwsubu_w | vwadd_w | vwsub_w |
    vnclipu._ | vnclip._
    vfwadd_w._ | vfwsub_w._ |
    vfwmacc._ | vfwnmacc._ | vfwmsac._ | vfwnmsac._ | 
    vfncvt_xu_f_w | vfncvt_x_f_w | vfncvt_rtz_xu_f_w | vfncvt_rtz_x_f_w | vfncvt_f_xu_w | vfncvt_f_x_w | vfncvt_f_f_w | vfncvt_rod_f_f_w |


  def isLookUpVS3P =
    vwmaccu._ | vwmacc._ | vwmaccsu._ | vwmaccus._ |

  def is2Malloc = 
    vwaddu._ | vwsubu._ | vwadd._ | vwsub._ |
    vwaddu_w | vwsubu_w | vwadd_w | vwsub_w |
    vwmul._  | vwmulu._ | vwmulsu._ |
    vwmaccu._ | vwmacc._ | vwmaccsu._ | vwmaccus._ |
    vfwadd._ | vfwsub._ | vfwadd_w._ | vfwsub_w._ |
    vfmul._ |
    vfwmacc._ | vfwnmacc._ | vfwmsac._ | vfwnmsac._ | 
    vfwcvt_xu_f_v | vfwcvt_x_f_v | vfwcvt_rtz_xu_f_v | vfwcvt_rtz_x_f_v | vfwcvt_f_xu_v | vfwcvt_f_x_v | vfwcvt_f_f_v |
    vwredsumu._ | vwredsum._ | vfwredosum._


  def isXwb = _.mrd | vsetvli | vsetivli | vsetvl | vfirst | vpopc | vmv_x_s
  def isFwb = _.frd | vfmv_f_s
  def isVwb =
    _.ivv | _.ivx | _.ivi | _.mvd | _.fvf | _.fvd |
    vzext_vf8 | vsext_vf8 | vzext_vf4 | vsext_vf4 | vzext_vf2 | vsext_vf2 |
    isVload |
    vfsqrt_v | vfrsqrt7_v | vfrec7_v | vfclass_v | 
    vfcvt_xu_f_v | vfcvt_x_f_v | vfcvt_rtz_xu_f_v | vfcvt_rtz_x_f_v | vfcvt_f_xu_v | vfcvt_f_x_v |
    vfwcvt_xu_f_v | vfwcvt_x_f_v | vfwcvt_rtz_xu_f_v | vfwcvt_rtz_x_f_v | vfwcvt_f_xu_v | vfwcvt_f_x_v | vfwcvt_f_f_v |
    vfncvt_xu_f_w | vfncvt_x_f_w | vfncvt_rtz_xu_f_w | vfncvt_rtz_x_f_w | vfncvt_f_xu_w | vfncvt_f_x_w | vfncvt_f_f_w | vfncvt_rod_f_f_w |
    vmsbf | vmsif | vmsof | viota | vid |
    vmv_s_x | vfmv_s_f





  def isVector = isVAlu | isVMul | isVQpu | isVFpu | isVMem | isVConfig




}











class RS_RAW(implicit p: Parameters) extends RiftBundle {
  val rs1 = UInt((log2Ceil(32)).W)
  val rs2 = UInt((log2Ceil(32)).W)
  val rs3 = UInt((log2Ceil(32)).W)
}

class RS_PHY(implicit p: Parameters) extends RiftBundle {
  val rs1 = UInt((log2Ceil(maxRegNum)).W)
  val rs2 = UInt((log2Ceil(maxRegNum)).W)
  val rs3 = UInt((log2Ceil(maxRegNum)).W)
}

class RD_RAW(implicit p: Parameters) extends RiftBundle {
  val rd0 = UInt((log2Ceil(32)).W)
}

class RD_PHY(implicit p: Parameters) extends RiftBundle {
  val rd0 = UInt((log2Ceil(maxRegNum)).W)
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
  val rs4 = UInt((log2Ceil(32)).W)
  val rs5 = UInt((log2Ceil(32)).W)
  val rd0 = UInt((log2Ceil(32)).W)
}

class Reg_PHY(implicit p: Parameters) extends RiftBundle {
  val rs = Vec( 5,  UInt((log2Ceil(maxRegNum)).W) )
  val rd = Vec( 16, UInt((log2Ceil(maxRegNum)).W) )

}



class Instruction_set(implicit p: Parameters) extends RiftBundle{
  val alu_isa = new Alu_isa
  val bru_isa = new Bru_isa
  val lsu_isa = new Lsu_isa
  val csr_isa = new Csr_isa
  val mul_isa = new Mul_isa
  val privil_isa = new Privil_isa
  val fpu_isa = new Fpu_isa
  val vectorIsa = new VectorIsa



  def is_fwb =
    lsu_isa.is_fwb | fpu_isa.is_fwb | vectorIsa.isFwb

  def is_iwb = 
    alu_isa.is_alu | bru_isa.is_bru | lsu_isa.is_iwb | csr_isa.is_csr | mul_isa.is_mulDiv | fpu_isa.is_iwb | vectorIsa.isXwb
  // ~is_fwb

  def isVwb = vectorIsa.isVwb

  def is_privil_dpt = privil_isa.is_privil
  def is_fpu_dpt = fpu_isa.is_fpu

  def is_illeage = ~(alu_isa.is_alu | bru_isa.is_bru | lsu_isa.is_lsu | csr_isa.is_csr | mul_isa.is_mulDiv | privil_isa.is_privil | fpu_isa.is_fpu) 

  def isRS1 = alu_isa.is_alu | bru_isa.is_bru | lsu_isa.is_lsu             | csr_isa.is_csr | mul_isa.is_mulDiv | (fpu_isa.is_fpu & ~fpu_isa.is_fop) | vectorIsa.isLookUpRS1
  def isRS2 = alu_isa.is_alu | bru_isa.is_bru | (lsu_isa.is_lsu & ~is_fst) | csr_isa.is_csr | mul_isa.is_mulDiv | (fpu_isa.is_fpu & ~fpu_isa.is_fop) | vectorIsa.isLookUpRS2
  def isFS1 = fpu_isa.is_fop | vectorIsa.isLookUpFS1
  def isFS2 = fpu_isa.is_fop
  def isFS3 = fpu_isa.is_fop
  def isVS1 = vectorIsa.isLookUpVS1
  def isVS2 = vectorIsa.isLookUpVS2
  def isVS3 = vectorIsa.isLookUpVS3
  def isVS4 = vectorIsa.isLookUpVS2P | vectorIsa.isLookUpVS3P
  def isVMS = vectorIsa.isVector


}



class Instruction_param(implicit p: Parameters) extends RiftBundle {
  val is_rvc = Bool()
  val pc = UInt(vlen.W)
  
  val imm = UInt(64.W)
  val rm = UInt(3.W)
  val raw = new Reg_RAW

  // val vm = Bool()
  // def width = rm
  // val mew = Bool()
  // val mop = UInt(2.W)
  // val nf  = UInt(3.W)
  // def lumop = raw.rs2
  // def sumop = raw.rs2

}

class Info_instruction(implicit p: Parameters) extends Instruction_set {
  val param = new Instruction_param

}


class Dpt_info(implicit p: Parameters) extends Info_instruction {
  val phy = new Reg_PHY
  val csrr = UInt( (12+log2Ceil(4)).w )
  val csrw = UInt( (12+log2Ceil(4)).w )
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
  val rd1_raw = UInt(5.W)
  val rd0_phy = UInt((log2Ceil(maxRegNum)).W)
  val rd1_phy = UInt((log2Ceil(maxRegNum)).W)

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

  val isXcmm = Bool()
  val isFcmm = Bool()
  val isVcmm = Bool()

  val privil = new Privil_isa
  val is_illeage = Bool()

  val csrw = UInt((12+log2Ceil(4)).W)

}





class Csr_function(implicit p: Parameters) extends RiftBundle {
  val rw  = Bool()
  val rs  = Bool()
  val rc  = Bool()
}

class Csr_param(implicit p: Parameters) extends RD_PHY {
  val dat = new Operation_source(dw=64)
  val csrw = UInt( (12+log2Ceil(4)).W)

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



class WriteBack_info(dw:Int)(implicit p: Parameters) extends RD_PHY {
  val res = UInt(dw.W)
}

// class VWriteBack_info(implicit p: Parameters) extends RiftBundle {
//   val res = Vec( atNum, UInt(atw.W) )
//   val rd0 = UInt((log2Ceil(vRegNum)).W)
//   val v0  = Vec(atNum, Bool())
// }



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
  val is_store_commit = Vec(cmChn, Bool())
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

class ReadOp_Rsp_Bundle(dw: Int)(implicit p: Parameters) extends RiftBundle{
  val phy = UInt((log2Ceil(maxRegNum)).W)
  val op  = UInt(dw.W)
}

