/*
  Copyright (c) 2020 - 2024 Wuhan University of Technology <295054118@whut.edu.cn>

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



trait VDecode32{ this: DecodeBase =>

  info.vecIsa.vadd.ivv         := ( x === BitPat("b000000???????????000?????1010111") )
  info.vecIsa.vadd.ivx         := ( x === BitPat("b000000???????????100?????1010111") )
  info.vecIsa.vadd.ivi         := ( x === BitPat("b000000???????????011?????1010111") )

  info.vecIsa.vsub.ivv         := ( x === BitPat("b000010???????????000?????1010111") )
  info.vecIsa.vsub.ivx         := ( x === BitPat("b000010???????????100?????1010111") )
  info.vecIsa.vsub.ivi         := false.B

  info.vecIsa.vrsub.ivv        := false.B
  info.vecIsa.vrsub.ivx        := ( x === BitPat("b000011???????????100?????1010111") )
  info.vecIsa.vrsub.ivi        := ( x === BitPat("b000011???????????011?????1010111") )

  info.vecIsa.vminu.ivv        := ( x === BitPat("b000100???????????000?????1010111") )
  info.vecIsa.vminu.ivx        := ( x === BitPat("b000100???????????100?????1010111") )
  info.vecIsa.vminu.ivi        := false.B

  info.vecIsa.vmin.ivv         := ( x === BitPat("b000101???????????000?????1010111") )
  info.vecIsa.vmin.ivx         := ( x === BitPat("b000101???????????100?????1010111") )
  info.vecIsa.vmin.ivi         := false.B

  info.vecIsa.vmaxu.ivv        := ( x === BitPat("b000110???????????000?????1010111") )
  info.vecIsa.vmaxu.ivx        := ( x === BitPat("b000110???????????100?????1010111") )
  info.vecIsa.vmaxu.ivi        := false.B

  info.vecIsa.vmax.ivv         := ( x === BitPat("b000111???????????000?????1010111") )
  info.vecIsa.vmax.ivx         := ( x === BitPat("b000111???????????100?????1010111") )
  info.vecIsa.vmax.ivi         := false.B

  info.vecIsa.vand.ivv         := ( x === BitPat("b001001???????????000?????1010111") )
  info.vecIsa.vand.ivx         := ( x === BitPat("b001001???????????100?????1010111") )
  info.vecIsa.vand.ivi         := ( x === BitPat("b001001???????????011?????1010111") )

  info.vecIsa.vor.ivv          := ( x === BitPat("b001010???????????000?????1010111") )
  info.vecIsa.vor.ivx          := ( x === BitPat("b001010???????????100?????1010111") )
  info.vecIsa.vor.ivi          := ( x === BitPat("b001010???????????011?????1010111") )

  info.vecIsa.vxor.ivv         := ( x === BitPat("b001011???????????000?????1010111") )
  info.vecIsa.vxor.ivx         := ( x === BitPat("b001011???????????100?????1010111") )
  info.vecIsa.vxor.ivi         := ( x === BitPat("b001011???????????011?????1010111") )

  info.vecIsa.vrgather.ivv     := ( x === BitPat("b001100???????????000?????1010111") )
  info.vecIsa.vrgather.ivx     := ( x === BitPat("b001100???????????100?????1010111") )
  info.vecIsa.vrgather.ivi     := ( x === BitPat("b001100???????????011?????1010111") )

  info.vecIsa.vslideup.ivv     := false.B
  info.vecIsa.vslideup.ivx     := ( x === BitPat("b001110???????????100?????1010111") )
  info.vecIsa.vslideup.ivi     := ( x === BitPat("b001110???????????011?????1010111") )

  info.vecIsa.vrgatherei16.ivv := ( x === BitPat("b001110???????????000?????1010111") )
  info.vecIsa.vrgatherei16.ivx := false.B
  info.vecIsa.vrgatherei16.ivi := false.B

  info.vecIsa.vslidedown.ivv   := false.B
  info.vecIsa.vslidedown.ivx   := ( x === BitPat("b001111???????????100?????1010111") )
  info.vecIsa.vslidedown.ivi   := ( x === BitPat("b001111???????????011?????1010111") )



  info.vecIsa.vredsum.mvv      := ( x === BitPat("b000000???????????010?????1010111") )
  info.vecIsa.vredsum.mvx      := false.B

  info.vecIsa.vredand.mvv      := ( x === BitPat("b000001???????????010?????1010111") )
  info.vecIsa.vredand.mvx      := false.B

  info.vecIsa.vredor.mvv       := ( x === BitPat("b000010???????????010?????1010111") )
  info.vecIsa.vredor.mvx       := false.B

  info.vecIsa.vredxor.mvv      := ( x === BitPat("b000011???????????010?????1010111") )
  info.vecIsa.vredxor.mvx      := false.B

  info.vecIsa.vredminu.mvv     := ( x === BitPat("b000100???????????010?????1010111") )
  info.vecIsa.vredminu.mvx     := false.B

  info.vecIsa.vredmin.mvv      := ( x === BitPat("b000101???????????010?????1010111") )
  info.vecIsa.vredmin.mvx      := false.B

  info.vecIsa.vredmaxu.mvv     := ( x === BitPat("b000110???????????010?????1010111") )
  info.vecIsa.vredmaxu.mvx     := false.B

  info.vecIsa.vredmax.mvv      := ( x === BitPat("b000111???????????010?????1010111") )
  info.vecIsa.vredmax.mvx      := false.B

  info.vecIsa.vaaddu.mvv       := ( x === BitPat("b001000???????????010?????1010111") )
  info.vecIsa.vaaddu.mvx       := ( x === BitPat("b001000???????????110?????1010111") )

  info.vecIsa.vaadd.mvv        := ( x === BitPat("b001001???????????010?????1010111") )
  info.vecIsa.vaadd.mvx        := ( x === BitPat("b001001???????????110?????1010111") )

  info.vecIsa.vasubu.mvv       := ( x === BitPat("b001010???????????010?????1010111") )
  info.vecIsa.vasubu.mvx       := ( x === BitPat("b001010???????????110?????1010111") )

  info.vecIsa.vasub.mvv        := ( x === BitPat("b001011???????????010?????1010111") )
  info.vecIsa.vasub.mvx        := ( x === BitPat("b001011???????????110?????1010111") )

  info.vecIsa.vslide1up.mvv    := false.B
  info.vecIsa.vslide1up.mvx    := ( x === BitPat("b001110???????????110?????1010111") )

  info.vecIsa.vslide1down.mvv  := false.B
  info.vecIsa.vslide1down.mvx  := ( x === BitPat("b001111???????????110?????1010111") )

  info.vecIsa.vfadd.fvv        := ( x === BitPat("b000000???????????001?????1010111") )
  info.vecIsa.vfadd.fvf        := ( x === BitPat("b000000???????????101?????1010111") )

  info.vecIsa.vfredusum.fvv    := ( x === BitPat("b000001???????????001?????1010111") )
  info.vecIsa.vfredusum.fvf    := false.B

  info.vecIsa.vfsub.fvv        := ( x === BitPat("b000010???????????001?????1010111") )
  info.vecIsa.vfsub.fvf        := ( x === BitPat("b000010???????????101?????1010111") )

  info.vecIsa.vfredosum.fvv    := ( x === BitPat("b000011???????????001?????1010111") )
  info.vecIsa.vfredosum.fvf    := false.B

  info.vecIsa.vfmin.fvv        := ( x === BitPat("b000100???????????001?????1010111") )
  info.vecIsa.vfmin.fvf        := ( x === BitPat("b000100???????????101?????1010111") )

  info.vecIsa.vfredmin.fvv     := ( x === BitPat("b000101???????????001?????1010111") )
  info.vecIsa.vfredmin.fvf     := false.B

  info.vecIsa.vfmax.fvv        := ( x === BitPat("b000110???????????001?????1010111") )
  info.vecIsa.vfmax.fvf        := ( x === BitPat("b000110???????????101?????1010111") )

  info.vecIsa.vfredmax.fvv     := ( x === BitPat("b000111???????????001?????1010111") )
  info.vecIsa.vfredmax.fvf     := false.B

  info.vecIsa.vfsgnj.fvv       := ( x === BitPat("b001000???????????001?????1010111") )
  info.vecIsa.vfsgnj.fvf       := ( x === BitPat("b001000???????????101?????1010111") )

  info.vecIsa.vfsgnjn.fvv      := ( x === BitPat("b001001???????????001?????1010111") )
  info.vecIsa.vfsgnjn.fvf      := ( x === BitPat("b001001???????????101?????1010111") )

  info.vecIsa.vfsgnjx.fvv      := ( x === BitPat("b001010???????????001?????1010111") )
  info.vecIsa.vfsgnjx.fvf      := ( x === BitPat("b001010???????????101?????1010111") )

  info.vecIsa.vfslide1up.fvv   := false.B
  info.vecIsa.vfslide1up.fvf   := ( x === BitPat("b001110???????????101?????1010111") )

  info.vecIsa.vfslide1down.fvv := false.B
  info.vecIsa.vfslide1down.fvf := ( x === BitPat("b001111???????????101?????1010111") )

  info.vecIsa.vadc.ivv      := ( x === BitPat("b010000???????????000?????1010111") )
  info.vecIsa.vadc.ivx      := ( x === BitPat("b010000???????????100?????1010111") )
  info.vecIsa.vadc.ivi      := ( x === BitPat("b010000???????????011?????1010111") )

  info.vecIsa.vmadc.ivv     := ( x === BitPat("b010001???????????000?????1010111") )
  info.vecIsa.vmadc.ivx     := ( x === BitPat("b010001???????????100?????1010111") )
  info.vecIsa.vmadc.ivi     := ( x === BitPat("b010001???????????011?????1010111") )

  info.vecIsa.vsbc.ivv      := ( x === BitPat("b010010???????????000?????1010111") )
  info.vecIsa.vsbc.ivx      := ( x === BitPat("b010010???????????100?????1010111") )
  info.vecIsa.vsbc.ivi      := false.B

  info.vecIsa.vmsbc.ivv     := ( x === BitPat("b010011???????????000?????1010111") )
  info.vecIsa.vmsbc.ivx     := ( x === BitPat("b010011???????????100?????1010111") )
  info.vecIsa.vmsbc.ivi     := false.B

  info.vecIsa.vmerge.ivv    := ( x === BitPat("b010111???????????000?????1010111") ) & ( ~x(25) | (x(24,20) =/= 0.U) )
  info.vecIsa.vmerge.ivx    := ( x === BitPat("b010111???????????100?????1010111") ) & ( ~x(25) | (x(24,20) =/= 0.U) )
  info.vecIsa.vmerge.ivi    := ( x === BitPat("b010111???????????011?????1010111") ) & ( ~x(25) | (x(24,20) =/= 0.U) )

  info.vecIsa.vmv.ivv       := ( x === BitPat("b010111100000?????000?????1010111") )
  info.vecIsa.vmv.ivx       := ( x === BitPat("b010111100000?????100?????1010111") )
  info.vecIsa.vmv.ivi       := ( x === BitPat("b010111100000?????011?????1010111") )

  info.vecIsa.vmseq.ivv     := ( x === BitPat("b011000???????????000?????1010111") )
  info.vecIsa.vmseq.ivx     := ( x === BitPat("b011000???????????100?????1010111") )
  info.vecIsa.vmseq.ivi     := ( x === BitPat("b011000???????????011?????1010111") )

  info.vecIsa.vmsne.ivv     := ( x === BitPat("b011001???????????000?????1010111") )
  info.vecIsa.vmsne.ivx     := ( x === BitPat("b011001???????????100?????1010111") )
  info.vecIsa.vmsne.ivi     := ( x === BitPat("b011001???????????011?????1010111") )

  info.vecIsa.vmsltu.ivv    := ( x === BitPat("b011010???????????000?????1010111") )
  info.vecIsa.vmsltu.ivx    := ( x === BitPat("b011010???????????100?????1010111") )
  info.vecIsa.vmsltu.ivi    := false.B

  info.vecIsa.vmslt.ivv     := ( x === BitPat("b011011???????????000?????1010111") )
  info.vecIsa.vmslt.ivx     := ( x === BitPat("b011011???????????100?????1010111") )
  info.vecIsa.vmslt.ivi     := false.B

  info.vecIsa.vmsleu.ivv    := ( x === BitPat("b011100???????????000?????1010111") )
  info.vecIsa.vmsleu.ivx    := ( x === BitPat("b011100???????????100?????1010111") )
  info.vecIsa.vmsleu.ivi    := ( x === BitPat("b011100???????????011?????1010111") )

  info.vecIsa.vmsle.ivv     := ( x === BitPat("b011101???????????000?????1010111") )
  info.vecIsa.vmsle.ivx     := ( x === BitPat("b011101???????????100?????1010111") )
  info.vecIsa.vmsle.ivi     := ( x === BitPat("b011101???????????011?????1010111") )

  info.vecIsa.vmsgtu.ivv    := false.B
  info.vecIsa.vmsgtu.ivx    := ( x === BitPat("b011110???????????100?????1010111") )
  info.vecIsa.vmsgtu.ivi    := ( x === BitPat("b011110???????????011?????1010111") )

  info.vecIsa.vmsgt.ivv     := false.B
  info.vecIsa.vmsgt.ivx     := ( x === BitPat("b011111???????????100?????1010111") )
  info.vecIsa.vmsgt.ivi     := ( x === BitPat("b011111???????????011?????1010111") )


  info.vecIsa.vmv_x_s   := ( x === BitPat("b010000??????00000010?????1010111") )
  info.vecIsa.vpopc     := ( x === BitPat("b010000??????10000010?????1010111") )
  info.vecIsa.vfirst    := ( x === BitPat("b010000??????10001010?????1010111") )

  info.vecIsa.vmv_s_x   := ( x === BitPat("b010000?00000?????110?????1010111") )

  info.vecIsa.vzext_vf8 := ( x === BitPat("b010010??????00010010?????1010111") )
  info.vecIsa.vsext_vf8 := ( x === BitPat("b010010??????00011010?????1010111") )
  info.vecIsa.vzext_vf4 := ( x === BitPat("b010010??????00100010?????1010111") )
  info.vecIsa.vsext_vf4 := ( x === BitPat("b010010??????00101010?????1010111") )
  info.vecIsa.vzext_vf2 := ( x === BitPat("b010010??????00110010?????1010111") )
  info.vecIsa.vsext_vf2 := ( x === BitPat("b010010??????00111010?????1010111") )

  info.vecIsa.vmsbf := ( x === BitPat("b010100??????00001010?????1010111") )
  info.vecIsa.vmsof := ( x === BitPat("b010100??????00010010?????1010111") )
  info.vecIsa.vmsif := ( x === BitPat("b010100??????00011010?????1010111") )
  info.vecIsa.viota := ( x === BitPat("b010100??????10000010?????1010111") )
  info.vecIsa.vid   := ( x === BitPat("b010100??????10001010?????1010111") )

  info.vecIsa.vcompress.mvv := ( x === BitPat("b010111???????????010?????1010111") )
  info.vecIsa.vcompress.mvx := false.B

  info.vecIsa.vmandnot.mvv  := ( x === BitPat("b011000???????????010?????1010111") )
  info.vecIsa.vmandnot.mvx  := false.B

  info.vecIsa.vmand.mvv     := ( x === BitPat("b011001???????????010?????1010111") )
  info.vecIsa.vmand.mvx     := false.B

  info.vecIsa.vmor.mvv      := ( x === BitPat("b011010???????????010?????1010111") )
  info.vecIsa.vmor.mvx      := false.B

  info.vecIsa.vmxor.mvv     := ( x === BitPat("b011011???????????010?????1010111") )
  info.vecIsa.vmxor.mvx     := false.B

  info.vecIsa.vmornot.mvv   := ( x === BitPat("b011100???????????010?????1010111") )
  info.vecIsa.vmornot.mvx   := false.B

  info.vecIsa.vmnand.mvv    := ( x === BitPat("b011101???????????010?????1010111") )
  info.vecIsa.vmnand.mvx    := false.B

  info.vecIsa.vmnor.mvv     := ( x === BitPat("b011110???????????010?????1010111") )
  info.vecIsa.vmnor.mvx     := false.B

  info.vecIsa.vmxnor.mvv    := ( x === BitPat("b011111???????????010?????1010111") )
  info.vecIsa.vmxnor.mvx    := false.B

  info.vecIsa.vfmv_f_s     := ( x === BitPat("b010000??????00000001?????1010111") )


  info.vecIsa.vfmv_s_f     := ( x === BitPat("b010000?00000?????101?????1010111") )

  info.vecIsa.vfcvt_xu_f_v     := ( x === BitPat("b010010??????00000001?????1010111") )
  info.vecIsa.vfcvt_x_f_v      := ( x === BitPat("b010010??????00001001?????1010111") )
  info.vecIsa.vfcvt_f_xu_v     := ( x === BitPat("b010010??????00010001?????1010111") )
  info.vecIsa.vfcvt_f_x_v      := ( x === BitPat("b010010??????00011001?????1010111") )
  info.vecIsa.vfcvt_rtz_xu_f_v := ( x === BitPat("b010010??????00110001?????1010111") )
  info.vecIsa.vfcvt_rtz_x_f_v  := ( x === BitPat("b010010??????00111001?????1010111") )

  info.vecIsa.vfwcvt_xu_f_v     := ( x === BitPat("b010010??????01000001?????1010111") )
  info.vecIsa.vfwcvt_x_f_v      := ( x === BitPat("b010010??????01001001?????1010111") )
  info.vecIsa.vfwcvt_f_xu_v     := ( x === BitPat("b010010??????01010001?????1010111") )
  info.vecIsa.vfwcvt_f_x_v      := ( x === BitPat("b010010??????01011001?????1010111") )
  info.vecIsa.vfwcvt_f_f_v      := ( x === BitPat("b010010??????01100001?????1010111") )
  info.vecIsa.vfwcvt_rtz_xu_f_v := ( x === BitPat("b010010??????01110001?????1010111") )
  info.vecIsa.vfwcvt_rtz_x_f_v  := ( x === BitPat("b010010??????01111001?????1010111") )

  info.vecIsa.vfncvt_xu_f_w     := ( x === BitPat("b010010??????10000001?????1010111") )
  info.vecIsa.vfncvt_x_f_w      := ( x === BitPat("b010010??????10001001?????1010111") )
  info.vecIsa.vfncvt_f_xu_w     := ( x === BitPat("b010010??????10010001?????1010111") )
  info.vecIsa.vfncvt_f_x_w      := ( x === BitPat("b010010??????10011001?????1010111") )
  info.vecIsa.vfncvt_f_f_w      := ( x === BitPat("b010010??????10100001?????1010111") )
  info.vecIsa.vfncvt_rod_f_f_w  := ( x === BitPat("b010010??????10101001?????1010111") )
  info.vecIsa.vfncvt_rtz_xu_f_w := ( x === BitPat("b010010??????10110001?????1010111") )
  info.vecIsa.vfncvt_rtz_x_f_w  := ( x === BitPat("b010010??????10111001?????1010111") )


  info.vecIsa.vfsqrt_v     := ( x === BitPat("b010011??????00000001?????1010111") )
  info.vecIsa.vfrsqrt7_v   := ( x === BitPat("b010011??????00100001?????1010111") )
  info.vecIsa.vfrec7_v     := ( x === BitPat("b010011??????00101001?????1010111") )
  info.vecIsa.vfclass_v    := ( x === BitPat("b010011??????10000001?????1010111") )

  info.vecIsa.vfmerge.fvv   := false.B
  info.vecIsa.vfmerge.fvf   := ( x === BitPat("b010111???????????101?????1010111") ) & (~x(25) | (x(24,20) =/= 0.U))

  info.vecIsa.vfmv.fvv      := false.B
  info.vecIsa.vfmv.fvf      := ( x === BitPat("b010111100000?????101?????1010111") )


  info.vecIsa.vmfeq.fvv     := ( x === BitPat("b011000???????????001?????1010111") )
  info.vecIsa.vmfeq.fvf     := ( x === BitPat("b011000???????????101?????1010111") )

  info.vecIsa.vmfle.fvv     := ( x === BitPat("b011001???????????001?????1010111") )
  info.vecIsa.vmfle.fvf     := ( x === BitPat("b011001???????????101?????1010111") )

  info.vecIsa.vmflt.fvv     := ( x === BitPat("b011011???????????001?????1010111") )
  info.vecIsa.vmflt.fvf     := ( x === BitPat("b011011???????????101?????1010111") )

  info.vecIsa.vmfne.fvv     := ( x === BitPat("b011100???????????001?????1010111") )
  info.vecIsa.vmfne.fvf     := ( x === BitPat("b011100???????????101?????1010111") )

  info.vecIsa.vmfgt.fvv     := false.B
  info.vecIsa.vmfgt.fvf     := ( x === BitPat("b011101???????????101?????1010111") )

  info.vecIsa.vmfge.fvv     := false.B
  info.vecIsa.vmfge.fvf     := ( x === BitPat("b011111???????????101?????1010111") )


  info.vecIsa.vsaddu.ivv  := ( x === BitPat("b100000???????????000?????1010111") )
  info.vecIsa.vsaddu.ivx  := ( x === BitPat("b100000???????????100?????1010111") )
  info.vecIsa.vsaddu.ivi  := ( x === BitPat("b100000???????????011?????1010111") )

  info.vecIsa.vsadd.ivv   := ( x === BitPat("b100001???????????000?????1010111") )
  info.vecIsa.vsadd.ivx   := ( x === BitPat("b100001???????????100?????1010111") )
  info.vecIsa.vsadd.ivi   := ( x === BitPat("b100001???????????011?????1010111") )

  info.vecIsa.vssubu.ivv  := ( x === BitPat("b100010???????????000?????1010111") )
  info.vecIsa.vssubu.ivx  := ( x === BitPat("b100010???????????100?????1010111") )
  info.vecIsa.vssubu.ivi  := false.B

  info.vecIsa.vssub.ivv   := ( x === BitPat("b100011???????????000?????1010111") )
  info.vecIsa.vssub.ivx   := ( x === BitPat("b100011???????????100?????1010111") )
  info.vecIsa.vssub.ivi   := false.B

  info.vecIsa.vsll.ivv    := ( x === BitPat("b100101???????????000?????1010111") )
  info.vecIsa.vsll.ivx    := ( x === BitPat("b100101???????????100?????1010111") )
  info.vecIsa.vsll.ivi    := ( x === BitPat("b100101???????????011?????1010111") )

  info.vecIsa.vsmul.ivv   := ( x === BitPat("b100101???????????000?????1010111") )
  info.vecIsa.vsmul.ivx   := ( x === BitPat("b100101???????????100?????1010111") )
  info.vecIsa.vsmul.ivi   := false.B

  info.vecIsa.vmvnr.ivv   := false.B
  info.vecIsa.vmvnr.ivx   := false.B
  info.vecIsa.vmvnr.ivi   := ( x === BitPat("b???1011??????????011?????1010111") ) & ( x(24, 20) =/= x(11, 7) ) & (
    ( x(31,29) === "b000".U ) | ( x(31,29) === "b001".U ) | ( x(31,29) === "b011".U ) | ( x(31,29) === "b111".U )      
  )


  info.vecIsa.vsrl.ivv    := ( x === BitPat("b101000???????????000?????1010111") )
  info.vecIsa.vsrl.ivx    := ( x === BitPat("b101000???????????100?????1010111") )
  info.vecIsa.vsrl.ivi    := ( x === BitPat("b101000???????????011?????1010111") )

  info.vecIsa.vsra.ivv    := ( x === BitPat("b101001???????????000?????1010111") )
  info.vecIsa.vsra.ivx    := ( x === BitPat("b101001???????????100?????1010111") )
  info.vecIsa.vsra.ivi    := ( x === BitPat("b101001???????????011?????1010111") )

  info.vecIsa.vssrl.ivv   := ( x === BitPat("b101010???????????000?????1010111") )
  info.vecIsa.vssrl.ivx   := ( x === BitPat("b101010???????????100?????1010111") )
  info.vecIsa.vssrl.ivi   := ( x === BitPat("b101010???????????011?????1010111") )

  info.vecIsa.vssra.ivv   := ( x === BitPat("b101011???????????000?????1010111") )
  info.vecIsa.vssra.ivx   := ( x === BitPat("b101011???????????100?????1010111") )
  info.vecIsa.vssra.ivi   := ( x === BitPat("b101011???????????011?????1010111") )

  info.vecIsa.vnsrl.ivv   := ( x === BitPat("b101100???????????000?????1010111") )
  info.vecIsa.vnsrl.ivx   := ( x === BitPat("b101100???????????100?????1010111") )
  info.vecIsa.vnsrl.ivi   := ( x === BitPat("b101100???????????011?????1010111") )

  info.vecIsa.vnsra.ivv   := ( x === BitPat("b101101???????????000?????1010111") )
  info.vecIsa.vnsra.ivx   := ( x === BitPat("b101101???????????100?????1010111") )
  info.vecIsa.vnsra.ivi   := ( x === BitPat("b101101???????????011?????1010111") )

  info.vecIsa.vnclipu.ivv := ( x === BitPat("b101110???????????000?????1010111") )
  info.vecIsa.vnclipu.ivx := ( x === BitPat("b101110???????????100?????1010111") )
  info.vecIsa.vnclipu.ivi := ( x === BitPat("b101110???????????011?????1010111") )

  info.vecIsa.vnclip.ivv  := ( x === BitPat("b101111???????????000?????1010111") )
  info.vecIsa.vnclip.ivx  := ( x === BitPat("b101111???????????100?????1010111") )
  info.vecIsa.vnclip.ivi  := ( x === BitPat("b101111???????????011?????1010111") )

  info.vecIsa.vdivu.mvv   := ( x === BitPat("b100000???????????010?????1010111") )
  info.vecIsa.vdivu.mvx   := ( x === BitPat("b100000???????????110?????1010111") )

  info.vecIsa.vdiv.mvv    := ( x === BitPat("b100001???????????010?????1010111") )
  info.vecIsa.vdiv.mvx    := ( x === BitPat("b100001???????????110?????1010111") )

  info.vecIsa.vremu.mvv   := ( x === BitPat("b100010???????????010?????1010111") )
  info.vecIsa.vremu.mvx   := ( x === BitPat("b100010???????????110?????1010111") )

  info.vecIsa.vrem.mvv    := ( x === BitPat("b100011???????????010?????1010111") )
  info.vecIsa.vrem.mvx    := ( x === BitPat("b100011???????????110?????1010111") )

  info.vecIsa.vmulhu.mvv  := ( x === BitPat("b100100???????????010?????1010111") )
  info.vecIsa.vmulhu.mvx  := ( x === BitPat("b100100???????????110?????1010111") )

  info.vecIsa.vmul.mvv    := ( x === BitPat("b100101???????????010?????1010111") )
  info.vecIsa.vmul.mvx    := ( x === BitPat("b100101???????????110?????1010111") )

  info.vecIsa.vmulhsu.mvv := ( x === BitPat("b100110???????????010?????1010111") )
  info.vecIsa.vmulhsu.mvx := ( x === BitPat("b100110???????????110?????1010111") )

  info.vecIsa.vmulh.mvv   := ( x === BitPat("b100111???????????010?????1010111") )
  info.vecIsa.vmulh.mvx   := ( x === BitPat("b100111???????????110?????1010111") )

  info.vecIsa.vmadd.mvv   := ( x === BitPat("b101001???????????010?????1010111") )
  info.vecIsa.vmadd.mvx   := ( x === BitPat("b101001???????????110?????1010111") )

  info.vecIsa.vnmsub.mvv  := ( x === BitPat("b101011???????????010?????1010111") )
  info.vecIsa.vnmsub.mvx  := ( x === BitPat("b101011???????????110?????1010111") )

  info.vecIsa.vmacc.mvv   := ( x === BitPat("b101101???????????010?????1010111") )
  info.vecIsa.vmacc.mvx   := ( x === BitPat("b101101???????????110?????1010111") )

  info.vecIsa.vnmsac.mvv  := ( x === BitPat("b101111???????????010?????1010111") )
  info.vecIsa.vnmsac.mvx  := ( x === BitPat("b101111???????????110?????1010111") )

  info.vecIsa.vfdiv.fvv   := ( x === BitPat("b100000???????????001?????1010111") )
  info.vecIsa.vfdiv.fvf   := ( x === BitPat("b100000???????????101?????1010111") )

  info.vecIsa.vfrdiv.fvv  := false.B
  info.vecIsa.vfrdiv.fvf  := ( x === BitPat("b100001???????????101?????1010111") )

  info.vecIsa.vfmul.fvv   := ( x === BitPat("b100100???????????001?????1010111") )
  info.vecIsa.vfmul.fvf   := ( x === BitPat("b100100???????????101?????1010111") )

  info.vecIsa.vfrsub.fvv  := false.B
  info.vecIsa.vfrsub.fvf  := ( x === BitPat("b100111???????????101?????1010111") )

  info.vecIsa.vfmadd.fvv  := ( x === BitPat("b101000???????????001?????1010111") )
  info.vecIsa.vfmadd.fvf  := ( x === BitPat("b101000???????????101?????1010111") )

  info.vecIsa.vfnmadd.fvv := ( x === BitPat("b101001???????????001?????1010111") )
  info.vecIsa.vfnmadd.fvf := ( x === BitPat("b101001???????????101?????1010111") )

  info.vecIsa.vfmsub.fvv  := ( x === BitPat("b101010???????????001?????1010111") )
  info.vecIsa.vfmsub.fvf  := ( x === BitPat("b101010???????????101?????1010111") )

  info.vecIsa.vfnmsub.fvv := ( x === BitPat("b101011???????????001?????1010111") )
  info.vecIsa.vfnmsub.fvf := ( x === BitPat("b101011???????????101?????1010111") )

  info.vecIsa.vfmacc.fvv  := ( x === BitPat("b101100???????????001?????1010111") )
  info.vecIsa.vfmacc.fvf  := ( x === BitPat("b101100???????????101?????1010111") )

  info.vecIsa.vfnmacc.fvv := ( x === BitPat("b101101???????????001?????1010111") )
  info.vecIsa.vfnmacc.fvf := ( x === BitPat("b101101???????????101?????1010111") )

  info.vecIsa.vfmsac.fvv  := ( x === BitPat("b101110???????????001?????1010111") )
  info.vecIsa.vfmsac.fvf  := ( x === BitPat("b101110???????????101?????1010111") )

  info.vecIsa.vfnmsac.fvv := ( x === BitPat("b101111???????????001?????1010111") )
  info.vecIsa.vfnmsac.fvf := ( x === BitPat("b101111???????????101?????1010111") )

  info.vecIsa.vwredsumu.ivv  := ( x === BitPat("b110000???????????000?????1010111") )
  info.vecIsa.vwredsumu.ivx  := false.B
  info.vecIsa.vwredsumu.ivi  := false.B

  info.vecIsa.vwredsum.ivv   := ( x === BitPat("b110001???????????000?????1010111") )
  info.vecIsa.vwredsum.ivx   := false.B
  info.vecIsa.vwredsum.ivi   := false.B

  info.vecIsa.vwaddu.mvv     := ( x === BitPat("b110000???????????010?????1010111") )
  info.vecIsa.vwaddu.mvx     := ( x === BitPat("b110000???????????110?????1010111") )

  info.vecIsa.vwadd.mvv      := ( x === BitPat("b110001???????????010?????1010111") )
  info.vecIsa.vwadd.mvx      := ( x === BitPat("b110001???????????110?????1010111") )

  info.vecIsa.vwsubu.mvv     := ( x === BitPat("b110010???????????010?????1010111") )
  info.vecIsa.vwsubu.mvx     := ( x === BitPat("b110010???????????110?????1010111") )

  info.vecIsa.vwsub.mvv      := ( x === BitPat("b110011???????????010?????1010111") )
  info.vecIsa.vwsub.mvx      := ( x === BitPat("b110011???????????110?????1010111") )

  info.vecIsa.vwaddu_w.mvv   := ( x === BitPat("b110100???????????010?????1010111") )
  info.vecIsa.vwaddu_w.mvx   := ( x === BitPat("b110100???????????110?????1010111") )

  info.vecIsa.vwadd_w.mvv    := ( x === BitPat("b110101???????????010?????1010111") )
  info.vecIsa.vwadd_w.mvx    := ( x === BitPat("b110101???????????110?????1010111") )

  info.vecIsa.vwsubu_w.mvv   := ( x === BitPat("b110110???????????010?????1010111") )
  info.vecIsa.vwsubu_w.mvx   := ( x === BitPat("b110110???????????110?????1010111") )

  info.vecIsa.vwsub_w.mvv    := ( x === BitPat("b110111???????????010?????1010111") )
  info.vecIsa.vwsub_w.mvx    := ( x === BitPat("b110111???????????110?????1010111") )

  info.vecIsa.vwmulu.mvv     := ( x === BitPat("b111000???????????010?????1010111") )
  info.vecIsa.vwmulu.mvx     := ( x === BitPat("b111000???????????110?????1010111") )

  info.vecIsa.vwmulsu.mvv    := ( x === BitPat("b111010???????????010?????1010111") )
  info.vecIsa.vwmulsu.mvx    := ( x === BitPat("b111010???????????110?????1010111") )

  info.vecIsa.vwmul.mvv      := ( x === BitPat("b111011???????????010?????1010111") )
  info.vecIsa.vwmul.mvx      := ( x === BitPat("b111011???????????110?????1010111") )

  info.vecIsa.vwmaccu.mvv    := ( x === BitPat("b111100???????????010?????1010111") )
  info.vecIsa.vwmaccu.mvx    := ( x === BitPat("b111100???????????110?????1010111") )

  info.vecIsa.vwmacc.mvv     := ( x === BitPat("b111101???????????010?????1010111") )
  info.vecIsa.vwmacc.mvx     := ( x === BitPat("b111101???????????110?????1010111") )

  info.vecIsa.vwmaccus.mvv   := false.B
  info.vecIsa.vwmaccus.mvx   := ( x === BitPat("b111110???????????110?????1010111") )

  info.vecIsa.vwmaccsu.mvv   := ( x === BitPat("b111111???????????010?????1010111") )
  info.vecIsa.vwmaccsu.mvx   := ( x === BitPat("b111111???????????110?????1010111") )


  info.vecIsa.vfwadd.fvv     := ( x === BitPat("b110000???????????001?????1010111") )
  info.vecIsa.vfwadd.fvf     := ( x === BitPat("b110000???????????101?????1010111") )

  info.vecIsa.vfwredusum.fvv := ( x === BitPat("b110001???????????001?????1010111") )
  info.vecIsa.vfwredusum.fvf := ( x === BitPat("b110001???????????101?????1010111") )

  info.vecIsa.vfwsub.fvv     := ( x === BitPat("b110010???????????001?????1010111") )
  info.vecIsa.vfwsub.fvf     := ( x === BitPat("b110010???????????101?????1010111") )

  info.vecIsa.vfwredosum.fvv := ( x === BitPat("b110011???????????001?????1010111") )
  info.vecIsa.vfwredosum.fvf := ( x === BitPat("b110011???????????101?????1010111") )

  info.vecIsa.vfwadd_w.fvv   := ( x === BitPat("b110100???????????001?????1010111") )
  info.vecIsa.vfwadd_w.fvf   := ( x === BitPat("b110100???????????101?????1010111") )

  info.vecIsa.vfwsub_w.fvv   := ( x === BitPat("b110110???????????001?????1010111") )
  info.vecIsa.vfwsub_w.fvf   := ( x === BitPat("b110110???????????101?????1010111") )

  info.vecIsa.vfwmul.fvv     := ( x === BitPat("b111000???????????001?????1010111") )
  info.vecIsa.vfwmul.fvf     := ( x === BitPat("b111000???????????101?????1010111") )

  info.vecIsa.vfwmacc.fvv    := ( x === BitPat("b111100???????????001?????1010111") )
  info.vecIsa.vfwmacc.fvf    := ( x === BitPat("b111100???????????101?????1010111") )

  info.vecIsa.vfwnmacc.fvv   := ( x === BitPat("b111101???????????001?????1010111") )
  info.vecIsa.vfwnmacc.fvf   := ( x === BitPat("b111101???????????101?????1010111") )

  info.vecIsa.vfwmsac.fvv    := ( x === BitPat("b111110???????????001?????1010111") )
  info.vecIsa.vfwmsac.fvf    := ( x === BitPat("b111110???????????101?????1010111") )

  info.vecIsa.vfwnmsac.fvv   := ( x === BitPat("b111111???????????001?????1010111") )
  info.vecIsa.vfwnmsac.fvf   := ( x === BitPat("b111111???????????101?????1010111") )



  info.csrIsa.vsetvli  := ( x === BitPat("b0????????????????111?????1010111") )
  info.csrIsa.vsetivli := ( x === BitPat("b11???????????????111?????1010111") )
  info.csrIsa.vsetvl   := ( x === BitPat("b1000000??????????111?????1010111") )




  info.lsuIsa.vle8      := ( x === BitPat("b????00?00000?????000?????0000111") )
  info.lsuIsa.vle16     := ( x === BitPat("b????00?00000?????101?????0000111") )
  info.lsuIsa.vle32     := ( x === BitPat("b????00?00000?????110?????0000111") )
  info.lsuIsa.vle64     := ( x === BitPat("b????00?00000?????111?????0000111") )
    
  info.lsuIsa.vlm       := ( x === BitPat("b????00?01011?????000?????0000111") )
  info.lsuIsa.vsm       := ( x === BitPat("b????00?01011?????000?????0100111") )
  
  info.lsuIsa.vlse8     := ( x === BitPat("b????10???????????000?????0000111") )
  info.lsuIsa.vlse16    := ( x === BitPat("b????10???????????101?????0000111") )
  info.lsuIsa.vlse32    := ( x === BitPat("b????10???????????110?????0000111") )
  info.lsuIsa.vlse64    := ( x === BitPat("b????10???????????111?????0000111") )
 
  info.lsuIsa.vloxei8   := ( x === BitPat("b?????1???????????000?????0000111") )
  info.lsuIsa.vloxei16  := ( x === BitPat("b?????1???????????101?????0000111") )
  info.lsuIsa.vloxei32  := ( x === BitPat("b?????1???????????110?????0000111") )
  info.lsuIsa.vloxei64  := ( x === BitPat("b?????1???????????111?????0000111") )
  
  info.lsuIsa.vle8ff    := ( x === BitPat("b????00?10000?????000?????0000111") )
  info.lsuIsa.vle16ff   := ( x === BitPat("b????00?10000?????101?????0000111") )
  info.lsuIsa.vle32ff   := ( x === BitPat("b????00?10000?????110?????0000111") )
  info.lsuIsa.vle64ff   := ( x === BitPat("b????00?10000?????111?????0000111") )
  
  info.lsuIsa.vlre8    := ( x === BitPat("b????00?01000?????000?????0000111") )
  info.lsuIsa.vlre16   := ( x === BitPat("b????00?01000?????101?????0000111") )
  info.lsuIsa.vlre32   := ( x === BitPat("b????00?01000?????110?????0000111") )
  info.lsuIsa.vlre64   := ( x === BitPat("b????00?01000?????111?????0000111") )
  
  info.lsuIsa.vse8      := ( x === BitPat("b????00?00000?????000?????0100111") )
  info.lsuIsa.vse16     := ( x === BitPat("b????00?00000?????101?????0100111") )
  info.lsuIsa.vse32     := ( x === BitPat("b????00?00000?????110?????0100111") )
  info.lsuIsa.vse64     := ( x === BitPat("b????00?00000?????111?????0100111") )
  
  info.lsuIsa.vsse8     := ( x === BitPat("b????10???????????000?????0100111") )
  info.lsuIsa.vsse16    := ( x === BitPat("b????10???????????101?????0100111") )
  info.lsuIsa.vsse32    := ( x === BitPat("b????10???????????110?????0100111") )
  info.lsuIsa.vsse64    := ( x === BitPat("b????10???????????111?????0100111") )
  
  info.lsuIsa.vsoxei8   := ( x === BitPat("b?????1???????????000?????0100111") )
  info.lsuIsa.vsoxei16  := ( x === BitPat("b?????1???????????101?????0100111") )
  info.lsuIsa.vsoxei32  := ( x === BitPat("b?????1???????????110?????0100111") )
  info.lsuIsa.vsoxei64  := ( x === BitPat("b?????1???????????111?????0100111") )
  
  info.lsuIsa.vsr       := ( x === BitPat("b????00?01000?????000?????0100111") )




  info.vAttach.get.vm  := x(25).asBool
  info.vAttach.get.nf  := x(31,29)

  info.vAttach.get.vtype     := 0.U
  info.vAttach.get.vlIdx     := 0.U
  info.vAttach.get.lmulSel   := 0.U
  info.vAttach.get.nfSel     := 0.U
  info.vAttach.get.widenSel  := 0.U
  info.vAttach.get.microIdx  := 0.U
  
  info.vAttach.get.eleIdx    := 0.U
  // info.vAttach.get.tEleIdx    := 0.U
  info.vAttach.get.vop0      := false.B
  info.vAttach.get.vop1      := 0.U
  info.vAttach.get.vop2      := 0.U


  
}

trait NVDecode{ this: DecodeBase =>
  info.vecIsa := 0.U.asTypeOf( new VecIsa )

  info.csrIsa.vsetvli  := false.B
  info.csrIsa.vsetivli := false.B
  info.csrIsa.vsetvl   := false.B

  info.lsuIsa.vle8      := false.B
  info.lsuIsa.vle16     := false.B
  info.lsuIsa.vle32     := false.B
  info.lsuIsa.vle64     := false.B
  info.lsuIsa.vlm       := false.B
  info.lsuIsa.vsm       := false.B
  info.lsuIsa.vlse8     := false.B
  info.lsuIsa.vlse16    := false.B
  info.lsuIsa.vlse32    := false.B
  info.lsuIsa.vlse64    := false.B
  info.lsuIsa.vloxei8   := false.B
  info.lsuIsa.vloxei16  := false.B
  info.lsuIsa.vloxei32  := false.B
  info.lsuIsa.vloxei64  := false.B
  info.lsuIsa.vle8ff    := false.B
  info.lsuIsa.vle16ff   := false.B
  info.lsuIsa.vle32ff   := false.B
  info.lsuIsa.vle64ff   := false.B
  info.lsuIsa.vlre8    := false.B
  info.lsuIsa.vlre16   := false.B
  info.lsuIsa.vlre32   := false.B
  info.lsuIsa.vlre64   := false.B
  info.lsuIsa.vse8      := false.B
  info.lsuIsa.vse16     := false.B
  info.lsuIsa.vse32     := false.B
  info.lsuIsa.vse64     := false.B
  info.lsuIsa.vsse8     := false.B
  info.lsuIsa.vsse16    := false.B
  info.lsuIsa.vsse32    := false.B
  info.lsuIsa.vsse64    := false.B
  info.lsuIsa.vsoxei8   := false.B
  info.lsuIsa.vsoxei16  := false.B
  info.lsuIsa.vsoxei32  := false.B
  info.lsuIsa.vsoxei64  := false.B
  info.lsuIsa.vsr       := false.B

  if( hasVector ){
    info.vAttach.get := 0.U.asTypeOf(new VRename_Attach_Bundle)
  }
}
