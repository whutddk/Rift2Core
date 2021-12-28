
/*
  Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

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
import rift2Core.define._
import rift2Core.backend.fpu._

trait FPU_util {

    def ieeeWidth32 = 8 + 24
    def ieeeWidth64 = 11 + 53

    def recodedWidth32 = 33
    def recodedWidth64 = 65

    def qNaN32 = "b011100000010000000000000000000000".U(33.W)
    def sNaN32 = "b011100000000000000000000000000000".U(33.W)

    def qNaN64 = "b01110000000001000000000000000000000000000000000000000000000000000".U(65.W)
    def sNaN64 = "b01110000000000000000000000000000000000000000000000000000000000000".U(65.W)

    def isNaN32(x: UInt) = x(31, 29).andR
    def isNaN64(x: UInt) = x(63, 61).andR

    def isSNaN32(x: UInt) = isNaN32(x) & ~x(22)
    def isQNaN32(x: UInt) = isNaN32(x) &  x(22)
    def isSNaN64(x: UInt) = isNaN64(x) & ~x(51)
    def isQNaN64(x: UInt) = isNaN64(x) &  x(51)

    def recode64(x: UInt) = hardfloat.recFNFromFN(11, 53, x)
    def ieee64(x: UInt) = hardfloat.fNFromRecFN(11, 53, x)
    def recode32(x: UInt) = hardfloat.recFNFromFN(8, 24, x)
    def ieee32(x: UInt) = hardfloat.fNFromRecFN(8, 24, x)
}

class FPU_out extends Bundle {
    val res = Wire(UInt(64.W))
    val exceptionFlags = Wire(UInt(5.W))
}


class Fpu_float2int_iss_info extends Bundle {
  val is_f32toi32 = Bool()
  val is_f32toi64 = Bool()
  val is_f64toi32 = Bool()
  val is_f64toi64 = Bool()
  val is_fmvfto32 = Bool()
  val is_fmvfto64 = Bool()
  val is_fclass32 = Bool()
  val is_fclass64 = Bool()

  val is_usi        = Bool()

  val op1 = UInt(64.W)
  val rm = UInt(3.W)

  val rd0_phy = UInt(6.W)
}

class Fpu_int2float_iss_info extends Bundle {
  val is_i32tof32 = Bool()
  val is_i64tof32 = Bool()
  val is_i32tof64 = Bool()
  val is_i64tof64 = Bool()
  val is_fmv32tof = Bool()
  val is_fmv64tof = Bool()

  val is_usi = Bool()

  val op1 = UInt(64.W)
  val rm = UInt(3.W)

  val rd0_phy = UInt(6.W)
}

class Fpu_float2float_iss_info extends Bundle {
  val is_f32tof64 = Bool()
  val is_f64tof32 = Bool()
  val is_fSign32  = Bool()
  val is_fSign64  = Bool()

  val op1 = UInt(64.W)
  val op2 = UInt(64.W)
  val rm = UInt(3.W)

  val rd0_phy = UInt(6.W)
}

class Fpu_floatCmp_iss_info extends Bundle {
  val is_eq  = Bool()
  val is_lt  = Bool()
  val is_gt  = Bool()
  val is_min = Bool()
  val is_max = Bool()

  val is_in_64_32n  = Bool()

  val op1 = UInt(64.W)
  val op2 = UInt(64.W)
  val rm = UInt(3.W)

  val rd0_phy = UInt( (6 max 6).W)

}

class Fpu_floatFma_iss_info extends Bundle {
  val is_64_32n = Bool()
  val op_sel = UInt(4.W)
  val rm =  UInt(3.W)
  val op1 = UInt(64.W)
  val op2 = UInt(64.W)
  val op3 = UInt(64.W)

  val rd0_phy = UInt(6.W)
}

class Fpu_floatDivSqrt_iss_info extends Bundle {
  val is_div32 = Bool()
  val is_div64 = Bool()
  val is_sqrt32 = Bool()
  val is_sqrt64 = Bool()

  val op1 = UInt(64.W)
  val op2 = UInt(64.W)

  val rm = UInt(3.W)

  val rd0_phy = UInt(6.W)
}

class Fpu_floatCsr_iss_info extends Bundle {
  val is_frw = Bool()
  val is_frs = Bool()
  val is_frc = Bool()

  val data  = UInt(8.W)
  val addr  = UInt(2.W)
  val rd0_phy = UInt( (6 max 6).W)

}



// maxType
//   def unsafeConvert(x: UInt, to: FType) = if (this == to) x else {
//     val sign = x(sig + exp)
//     val fractIn = x(sig - 2, 0)
//     val expIn = x(sig + exp - 1, sig - 1)
//     val fractOut = fractIn << to.sig >> sig
//     val expOut = {
//       val expCode = expIn(exp, exp - 2)
//       val commonCase = (expIn + (1 << to.exp)) - (1 << exp)
//       Mux(expCode === 0 || expCode >= 6, Cat(expCode, commonCase(to.exp - 3, 0)), commonCase(to.exp, 0))
//     }
//     Cat(sign, expOut, fractOut)
//   }









