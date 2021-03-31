// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.util._
import scala.collection.immutable.ListMap

abstract class TLBundleBase(params: TLBundleParameters) extends GenericParameterizedBundle(params)

// common combos in lazy policy:
//   Put + Acquire
//   Release + AccessAck

object TLMessages
{
  //                                  A    B    C    D    E
  def PutFullData    = UInt(0) //     .    .                   => AccessAck
  def PutPartialData = UInt(1) //     .    .                   => AccessAck
  def ArithmeticData = UInt(2) //     .    .                   => AccessAckData
  def LogicalData    = UInt(3) //     .    .                   => AccessAckData
  def Get            = UInt(4) //     .    .                   => AccessAckData
  def Hint           = UInt(5) //     .    .                   => HintAck
  def AcquireBlock   = UInt(6) //     .                        => Grant[Data]
  def AcquirePerm    = UInt(7) //     .                        => Grant[Data]
  def Probe          = UInt(6) //          .                   => ProbeAck[Data]
  def AccessAck      = UInt(0) //               .    .
  def AccessAckData  = UInt(1) //               .    .
  def HintAck        = UInt(2) //               .    .
  def ProbeAck       = UInt(4) //               .
  def ProbeAckData   = UInt(5) //               .
  def Release        = UInt(6) //               .              => ReleaseAck
  def ReleaseData    = UInt(7) //               .              => ReleaseAck
  def Grant          = UInt(4) //                    .         => GrantAck
  def GrantData      = UInt(5) //                    .         => GrantAck
  def ReleaseAck     = UInt(6) //                    .
  def GrantAck       = UInt(0) //                         .

  def isA(x: UInt) = x <= AcquirePerm
  def isB(x: UInt) = x <= Probe
  def isC(x: UInt) = x <= ReleaseData
  def isD(x: UInt) = x <= ReleaseAck

  def adResponse = Vec(AccessAck, AccessAck, AccessAckData, AccessAckData, AccessAckData, HintAck, Grant, Grant)
  def bcResponse = Vec(AccessAck, AccessAck, AccessAckData, AccessAckData, AccessAckData, HintAck, ProbeAck, ProbeAck)

  def a = Seq( ("PutFullData",TLPermissions.PermMsgReserved),
               ("PutPartialData",TLPermissions.PermMsgReserved),
               ("ArithmeticData",TLAtomics.ArithMsg),
               ("LogicalData",TLAtomics.LogicMsg),
               ("Get",TLPermissions.PermMsgReserved),
               ("Hint",TLHints.HintsMsg),
               ("AcquireBlock",TLPermissions.PermMsgGrow),
               ("AcquirePerm",TLPermissions.PermMsgGrow))

  def b = Seq( ("PutFullData",TLPermissions.PermMsgReserved),
               ("PutPartialData",TLPermissions.PermMsgReserved),
               ("ArithmeticData",TLAtomics.ArithMsg),
               ("LogicalData",TLAtomics.LogicMsg),
               ("Get",TLPermissions.PermMsgReserved),
               ("Hint",TLHints.HintsMsg),
               ("Probe",TLPermissions.PermMsgCap))

  def c = Seq( ("AccessAck",TLPermissions.PermMsgReserved),
               ("AccessAckData",TLPermissions.PermMsgReserved),
               ("HintAck",TLPermissions.PermMsgReserved),
               ("Invalid Opcode",TLPermissions.PermMsgReserved),
               ("ProbeAck",TLPermissions.PermMsgReport),
               ("ProbeAckData",TLPermissions.PermMsgReport),
               ("Release",TLPermissions.PermMsgReport),
               ("ReleaseData",TLPermissions.PermMsgReport))

  def d = Seq( ("AccessAck",TLPermissions.PermMsgReserved),
               ("AccessAckData",TLPermissions.PermMsgReserved),
               ("HintAck",TLPermissions.PermMsgReserved),
               ("Invalid Opcode",TLPermissions.PermMsgReserved),
               ("Grant",TLPermissions.PermMsgCap),
               ("GrantData",TLPermissions.PermMsgCap),
               ("ReleaseAck",TLPermissions.PermMsgReserved))

}

/**
  * The three primary TileLink permissions are:
  *   (T)runk: the agent is (or is on inwards path to) the global point of serialization.
  *   (B)ranch: the agent is on an outwards path to
  *   (N)one:
  * These permissions are permuted by transfer operations in various ways.
  * Operations can cap permissions, request for them to be grown or shrunk,
  * or for a report on their current status.
  */
object TLPermissions
{
  val aWidth = 2
  val bdWidth = 2
  val cWidth = 3

  // Cap types (Grant = new permissions, Probe = permisions <= target)
  def toT = UInt(0, bdWidth)
  def toB = UInt(1, bdWidth)
  def toN = UInt(2, bdWidth)
  def isCap(x: UInt) = x <= toN

  // Grow types (Acquire = permissions >= target)
  def NtoB = UInt(0, aWidth)
  def NtoT = UInt(1, aWidth)
  def BtoT = UInt(2, aWidth)
  def isGrow(x: UInt) = x <= BtoT

  // Shrink types (ProbeAck, Release)
  def TtoB = UInt(0, cWidth)
  def TtoN = UInt(1, cWidth)
  def BtoN = UInt(2, cWidth)
  def isShrink(x: UInt) = x <= BtoN

  // Report types (ProbeAck, Release)
  def TtoT = UInt(3, cWidth)
  def BtoB = UInt(4, cWidth)
  def NtoN = UInt(5, cWidth)
  def isReport(x: UInt) = x <= NtoN

  def PermMsgGrow:Seq[String] = Seq("Grow NtoB", "Grow NtoT", "Grow BtoT")
  def PermMsgCap:Seq[String] = Seq("Cap toT", "Cap toB", "Cap toN")
  def PermMsgReport:Seq[String] = Seq("Shrink TtoB", "Shrink TtoN", "Shrink BtoN", "Report TotT", "Report BtoB", "Report NtoN")
  def PermMsgReserved:Seq[String] = Seq("Reserved")
}

object TLAtomics
{
  val width = 3

  // Arithmetic types
  def MIN  = UInt(0, width)
  def MAX  = UInt(1, width)
  def MINU = UInt(2, width)
  def MAXU = UInt(3, width)
  def ADD  = UInt(4, width)
  def isArithmetic(x: UInt) = x <= ADD

  // Logical types
  def XOR  = UInt(0, width)
  def OR   = UInt(1, width)
  def AND  = UInt(2, width)
  def SWAP = UInt(3, width)
  def isLogical(x: UInt) = x <= SWAP

  def ArithMsg:Seq[String] = Seq("MIN", "MAX", "MINU", "MAXU", "ADD")
  def LogicMsg:Seq[String] = Seq("XOR", "OR", "AND", "SWAP")
}


object TLHints
{
  val width = 1

  def PREFETCH_READ  = UInt(0, width)
  def PREFETCH_WRITE = UInt(1, width)
  def isHints(x: UInt) = x <= PREFETCH_WRITE

  def HintsMsg:Seq[String] = Seq("PrefetchRead", "PrefetchWrite")
}

sealed trait TLChannel extends TLBundleBase {
  val channelName: String
}

sealed trait TLDataChannel extends TLChannel
sealed trait TLAddrChannel extends TLDataChannel

final class TLBundleA(params: TLBundleParameters)
  extends TLBundleBase(params) with TLAddrChannel
{
  val channelName = "'A' channel"
  // fixed fields during multibeat:
  val opcode  = UInt(width = 3)
  val param   = UInt(width = List(TLAtomics.width, TLPermissions.aWidth, TLHints.width).max) // amo_opcode || grow perms || hint
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits) // from
  val address = UInt(width = params.addressBits) // to
  val user    = BundleMap(params.requestFields)
  val echo    = BundleMap(params.echoFields)
  // variable fields during multibeat:
  val mask    = UInt(width = params.dataBits/8)
  val data    = UInt(width = params.dataBits)
  val corrupt = Bool() // only applies to *Data messages
}
final class TLBundleB(params: TLBundleParameters)
  extends TLBundleBase(params) with TLAddrChannel
{
  val channelName = "'B' channel"
  // fixed fields during multibeat:
  val opcode  = UInt(width = 3)
  val param   = UInt(width = TLPermissions.bdWidth) // cap perms
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits) // to
  val address = UInt(width = params.addressBits) // from
  // variable fields during multibeat:
  val mask    = UInt(width = params.dataBits/8)
  val data    = UInt(width = params.dataBits)
  val corrupt = Bool() // only applies to *Data messages
}

final class TLBundleC(params: TLBundleParameters)
  extends TLBundleBase(params) with TLAddrChannel
{
  val channelName = "'C' channel"
  // fixed fields during multibeat:
  val opcode  = UInt(width = 3)
  val param   = UInt(width = TLPermissions.cWidth) // shrink or report perms
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits) // from
  val address = UInt(width = params.addressBits) // to
  val user    = BundleMap(params.requestFields)
  val echo    = BundleMap(params.echoFields)
  // variable fields during multibeat:
  val data    = UInt(width = params.dataBits)
  val corrupt = Bool() // only applies to *Data messages
}

final class TLBundleD(params: TLBundleParameters)
  extends TLBundleBase(params) with TLDataChannel
{
  val channelName = "'D' channel"
  // fixed fields during multibeat:
  val opcode  = UInt(width = 3)
  val param   = UInt(width = TLPermissions.bdWidth) // cap perms
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits) // to
  val sink    = UInt(width = params.sinkBits)   // from
  val denied  = Bool() // implies corrupt iff *Data
  val user    = BundleMap(params.responseFields)
  val echo    = BundleMap(params.echoFields)
  // variable fields during multibeat:
  val data    = UInt(width = params.dataBits)
  val corrupt = Bool() // only applies to *Data messages
}

final class TLBundleE(params: TLBundleParameters)
  extends TLBundleBase(params) with TLChannel
{
  val channelName = "'E' channel"
  val sink = UInt(width = params.sinkBits) // to
}

class TLBundle(val params: TLBundleParameters) extends Record
{
  // Emulate a Bundle with elements abcde or ad depending on params.hasBCE

  private val optA = Some                (Decoupled(new TLBundleA(params)))
  private val optB = params.hasBCE.option(Decoupled(new TLBundleB(params)).flip)
  private val optC = params.hasBCE.option(Decoupled(new TLBundleC(params)))
  private val optD = Some                (Decoupled(new TLBundleD(params)).flip)
  private val optE = params.hasBCE.option(Decoupled(new TLBundleE(params)))

  def a: DecoupledIO[TLBundleA] = optA.getOrElse(Wire(Decoupled(new TLBundleA(params))))
  def b: DecoupledIO[TLBundleB] = optB.getOrElse(Wire(Decoupled(new TLBundleB(params))))
  def c: DecoupledIO[TLBundleC] = optC.getOrElse(Wire(Decoupled(new TLBundleC(params))))
  def d: DecoupledIO[TLBundleD] = optD.getOrElse(Wire(Decoupled(new TLBundleD(params))))
  def e: DecoupledIO[TLBundleE] = optE.getOrElse(Wire(Decoupled(new TLBundleE(params))))

  override def cloneType: this.type = (new TLBundle(params)).asInstanceOf[this.type]
  val elements =
    if (params.hasBCE) ListMap("e" -> e, "d" -> d, "c" -> c, "b" -> b, "a" -> a)
    else ListMap("d" -> d, "a" -> a)

  def tieoff(): Unit = {
    a.ready.dir match {
      case INPUT =>
        a.ready := Bool(false)
        c.ready := Bool(false)
        e.ready := Bool(false)
        b.valid := Bool(false)
        d.valid := Bool(false)
      case OUTPUT =>
        a.valid := Bool(false)
        c.valid := Bool(false)
        e.valid := Bool(false)
        b.ready := Bool(false)
        d.ready := Bool(false)
      case _ =>
    }
  }
}

object TLBundle
{
  def apply(params: TLBundleParameters) = new TLBundle(params)
}

class TLAsyncBundleBase(params: TLAsyncBundleParameters) extends GenericParameterizedBundle(params)

class TLAsyncBundle(params: TLAsyncBundleParameters) extends TLAsyncBundleBase(params)
{
  val a = new AsyncBundle(new TLBundleA(params.base), params.async)
  val b = new AsyncBundle(new TLBundleB(params.base), params.async).flip
  val c = new AsyncBundle(new TLBundleC(params.base), params.async)
  val d = new AsyncBundle(new TLBundleD(params.base), params.async).flip
  val e = new AsyncBundle(new TLBundleE(params.base), params.async)
}

class TLRationalBundle(params: TLBundleParameters) extends TLBundleBase(params)
{
  val a = RationalIO(new TLBundleA(params))
  val b = RationalIO(new TLBundleB(params)).flip
  val c = RationalIO(new TLBundleC(params))
  val d = RationalIO(new TLBundleD(params)).flip
  val e = RationalIO(new TLBundleE(params))
}

class TLCreditedBundle(params: TLBundleParameters) extends TLBundleBase(params)
{
  val a = CreditedIO(new TLBundleA(params))
  val b = CreditedIO(new TLBundleB(params)).flip
  val c = CreditedIO(new TLBundleC(params))
  val d = CreditedIO(new TLBundleD(params)).flip
  val e = CreditedIO(new TLBundleE(params))
}
