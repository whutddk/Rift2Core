
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
import rift2Core.define._
import rift2Core.backend.fpu._



case class FType(exp: Int, sig: Int) {
  def ieeeWidth = exp + sig
  def recodedWidth = ieeeWidth + 1

  def ieeeQNaN = (((BigInt(1) << (ieeeWidth - 1)) - (BigInt(1) << (sig - 2))).U)(ieeeWidth.W)
  def qNaN = (((BigInt(7) << (exp + sig - 3)) + (BigInt(1) << (sig - 2))).U)(recodedWidth.W)
  def isNaN(x: UInt) = x(sig + exp - 1, sig + exp - 3).andR
  def isSNaN(x: UInt) = isNaN(x) && !x(sig - 2)

  def classify(x: UInt) = {
    val sign = x(sig + exp)
    val code = x(exp + sig - 1, exp + sig - 3)
    val codeHi = code(2, 1)
    val isSpecial = codeHi === 3.U

    val isHighSubnormalIn = x(exp + sig - 3, sig - 1) < 2.U
    val isSubnormal = code === 1.U || codeHi === 1.U && isHighSubnormalIn
    val isNormal = codeHi === 1.U && !isHighSubnormalIn || codeHi === 2.U
    val isZero = code === 0.U
    val isInf = isSpecial && !code(0)
    val isNaN = code.andR
    val isSNaN = isNaN && !x(sig-2)
    val isQNaN = isNaN && x(sig-2)

    Cat(isQNaN, isSNaN, isInf && !sign, isNormal && !sign,
        isSubnormal && !sign, isZero && !sign, isZero && sign,
        isSubnormal && sign, isNormal && sign, isInf && sign)
  }

  // convert between formats, ignoring rounding, range, NaN
  def unsafeConvert(x: UInt, to: FType) = if (this == to) x else {
    val sign = x(sig + exp)
    val fractIn = x(sig - 2, 0)
    val expIn = x(sig + exp - 1, sig - 1)
    val fractOut = fractIn << to.sig >> sig
    val expOut = {
      val expCode = expIn(exp, exp - 2)
      val commonCase = (expIn + (1.U << to.exp)) - (1.U << exp)
      Mux(expCode === 0.U || expCode >= 6.U, Cat(expCode, commonCase(to.exp - 3, 0)), commonCase(to.exp, 0))
    }
    Cat(sign, expOut, fractOut)
  }

  private def ieeeBundle = {
    val expWidth = exp
    class IEEEBundle extends Bundle {
      val sign = Bool()
      val exp = UInt(expWidth.W)
      val sig = UInt((ieeeWidth-expWidth-1).W)
    }
    new IEEEBundle
  }

  def unpackIEEE(x: UInt) = x.asTypeOf(ieeeBundle)

  def recode(x: UInt) = hardfloat.recFNFromFN(exp, sig, x)
  def ieee(x: UInt) = hardfloat.fNFromRecFN(exp, sig, x)
}

object FType {
  val S = new FType(8, 24)
  val D = new FType(11, 53)

  val all = List(S, D)
}



trait HasFPUParameters {
  val fLen: Int       = 64
  require(fLen == 0 || FType.all.exists(_.ieeeWidth == fLen))
  val minFLen: Int    = 32
  def xLen: Int       = 64
  val minXLen = 32
  val nIntTypes = 2
  val floatTypes = FType.all
  // def minType = FType.S
  // def maxType = FType.D
  def prevType(t: FType) = floatTypes(typeTag(t) - 1)
  def maxExpWidth = 11
  def maxSigWidth = 53
  def typeTag(t: FType) = floatTypes.indexOf(t)
  def typeTagWbOffset = (FType.all.indexOf(FType.S) + 1).U

  // typeTag
  def S = 0.U
  def D = 1.U
  def I = 1.U


  private def isBox(x: UInt, t: FType): Bool = x(t.sig + t.exp, t.sig + t.exp - 4).andR

  private def box(x: UInt, xt: FType, y: UInt, yt: FType): UInt = {
    require(xt.ieeeWidth == 2 * yt.ieeeWidth)
    val swizzledNaN = Cat(
      x(xt.sig + xt.exp, xt.sig + xt.exp - 3),
      x(xt.sig - 2, yt.recodedWidth - 1).andR,
      x(xt.sig + xt.exp - 5, xt.sig),
      y(yt.recodedWidth - 2),
      x(xt.sig - 2, yt.recodedWidth - 1),
      y(yt.recodedWidth - 1),
      y(yt.recodedWidth - 3, 0))
    Mux(xt.isNaN(x), swizzledNaN, x)
  }

  // implement NaN unboxing for FU inputs
  def unbox(x: UInt, tag: UInt, exactType: Option[FType]): UInt = {
    val outType = exactType.getOrElse(FType.D)
    def helper(x: UInt, t: FType): Seq[(Bool, UInt)] = {
      val prev =
        if (t == FType.S) {
          Seq()
        } else { //FType.D
          val unswizzled = Cat(
            x(31),
            x(24 - 1),
            x(30, 0))
          val prev = helper(unswizzled, FType.S)
          val isbox = isBox(x, t)
          prev.map(p => (isbox && p._1, p._2))
        }
      prev :+ (true.B, t.unsafeConvert(x, outType))
    }

    val (oks, floats) = helper(x, FType.D).unzip
    if (exactType.isEmpty || floatTypes.size == 1) {
      Mux1H(Seq(
        ( tag === 0.U ) -> Mux(oks(0), floats(0), FType.D.qNaN),
        ( tag === 1.U ) -> Mux(oks(1), floats(1), FType.D.qNaN),
      ))

    } else {
      val t = exactType.get
      floats(typeTag(t)) | Mux(oks(typeTag(t)), 0.U, t.qNaN)
    }
  }

  // make sure that the redundant bits in the NaN-boxed encoding are consistent
  def consistent(x: UInt): Bool = {
    val unswizzled = Cat(
      x(31),
      x(52),
      x(30, 0))
    val prevOK = !isBox(x, FType.D) || true.B
    val curOK = !FType.D.isNaN(x) || x(60) === x(51, 32).andR
    prevOK && curOK
  }

  // generate a NaN box from an FU result
  def box(x: UInt, t: FType): UInt = {
    if (t == FType.D) {
      x
    } else { //FType.S
      val bigger = box(((BigInt(1) << 65)-1).U, FType.D, x, FType.S)
      bigger
    }
  }

  // generate a NaN box from an FU result
  // def box(x: UInt, tag: UInt): UInt = {
  //   val opts = floatTypes.map(t => box(x, t))
  //   opts(tag)
  // }

  // zap bits that hardfloat thinks are don't-cares, but we do care about
  def sanitizeNaN(x: UInt, t: FType): UInt = {
    if ( t == FType.S ) {
      x
    } else {
      val maskedNaN = x & ~(((BigInt(1) << (t.sig-1)) | (BigInt(1) << (t.sig+t.exp-4))).U)((t.recodedWidth).W)

      // val maskedNaN = Wire(UInt(65.W))
      //   maskedNaN := x & ~(( 1 << 52).U | ( 1 << 60).U )
      Mux(t.isNaN(x), maskedNaN, x)
    }
  }

  // implement NaN boxing and recoding for FL*/fmv.*.x
  def recode(x: UInt, tag: Int): UInt = {
    def helper(x: UInt, t: FType): UInt = {
      if ( t == FType.S ) {
        t.recode(x)
      } else {
        box(t.recode(x), t, helper(x, FType.S), FType.S)
      }
    }

    // fill MSBs of subword loads to emulate a wider load of a NaN-boxed value
    val boxes = floatTypes.map(t => ((BigInt(1) << 64) - (BigInt(1) << t.ieeeWidth)).U )
    helper(boxes(tag) | x, FType.D)
  }

  // implement NaN unboxing and un-recoding for FS*/fmv.x.*
  def ieee(x: UInt, t: FType = FType.D): UInt = {
    if ( t == FType.S ) {
      t.ieee(x)
    } else { //FType.D
      val unrecoded = t.ieee(x)
      val prevRecoded = Cat(
        x(31),
        x(53-1),
        x(30, 0))
      val prevUnrecoded = ieee(prevRecoded, FType.S)
      Cat(unrecoded >> 32, Mux(t.isNaN(x), prevUnrecoded, unrecoded(32-1, 0)))
    }
  }
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

class Fpu_param extends Rd_Param(64){
  val dat = new Operation_source
  val rm = UInt(3.W)

}

class Fpu_iss_info() extends Bundle {
          // val rm = Bits(width = FPConstants.RM_SZ)
  // val fmaCmd = Bits(width = 2)
  // val typ = Bits(width = 2)
  // val fmt = Bits(width = 2)
          // val in1 = Bits(width = 64+1)
          // val in2 = Bits(width = 64+1)
          // val in3 = Bits(width = 64+1)

  val fun = new Fpu_isa
  val param = new Fpu_param

}



// class Fpu_int2float_iss_info extends Bundle {
//   val is_i32tof32 = Bool()
//   val is_i64tof32 = Bool()
//   val is_i32tof64 = Bool()
//   val is_i64tof64 = Bool()
//   val is_fmv32tof = Bool()
//   val is_fmv64tof = Bool()

//   val is_usi = Bool()

//   val op1 = UInt(64.W)
//   val rm = UInt(3.W)

//   val rd0_phy = UInt(6.W)
// }

// class Fpu_float2float_iss_info extends Bundle {
//   val is_f32tof64 = Bool()
//   val is_f64tof32 = Bool()
//   val is_fSign32  = Bool()
//   val is_fSign64  = Bool()

//   val op1 = UInt(64.W)
//   val op2 = UInt(64.W)
//   val rm = UInt(3.W)

//   val rd0_phy = UInt(6.W)
// }

// class Fpu_floatCmp_iss_info extends Bundle {
//   val is_eq  = Bool()
//   val is_lt  = Bool()
//   val is_gt  = Bool()
//   val is_min = Bool()
//   val is_max = Bool()

//   val is_in_64_32n  = Bool()

//   val op1 = UInt(64.W)
//   val op2 = UInt(64.W)
//   val rm = UInt(3.W)

//   val rd0_phy = UInt( (6 max 6).W)

// }

// class Fpu_floatFma_iss_info extends Bundle {
//   val is_64_32n = Bool()
//   val op_sel = UInt(4.W)
//   val rm =  UInt(3.W)
//   val op1 = UInt(64.W)
//   val op2 = UInt(64.W)
//   val op3 = UInt(64.W)

//   val rd0_phy = UInt(6.W)
// }

// class Fpu_floatDivSqrt_iss_info extends Bundle {
//   val is_div32 = Bool()
//   val is_div64 = Bool()
//   val is_sqrt32 = Bool()
//   val is_sqrt64 = Bool()

//   val op1 = UInt(64.W)
//   val op2 = UInt(64.W)

//   val rm = UInt(3.W)

//   val rd0_phy = UInt(6.W)
// }

// class Fpu_floatCsr_iss_info extends Bundle {
//   val is_frw = Bool()
//   val is_frs = Bool()
//   val is_frc = Bool()

//   val data  = UInt(8.W)
//   val addr  = UInt(2.W)
//   val rd0_phy = UInt( (6 max 6).W)

// }



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









