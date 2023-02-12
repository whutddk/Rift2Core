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


package rift2Chip

import chisel3._
import chisel3.util._

import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import chipsalliance.rocketchip.config._

// class GatewayPLICIO extends Bundle{
//   val valid = Output(Bool())
//   val ready = Output(Bool())
//   val complete = Input(Bool())
// }

// class LevelGateway extends Module{
//   val io = new Bundle{
//     val interrupt = Input(Bool())
//     val plic = new GatewayPLICIO
//   }

//   val inFlight = RegInit(Bool())
//   when (io.interrupt && io.plic.ready) { inFlight := true }
//   when (io.plic.complete) { inFlight := false }
//   io.plic.valid := io.interrupt && !inFlight
// }





class Plic( nHarts: Int = 1, nPriorities: Int = 8, nDevices: Int = 1023 )(implicit p: Parameters) extends LazyModule {
  require( nHarts > 0 )
  require( nPriorities != 0 )
  require( isPow2(nPriorities) )
  require( nDevices < 1024 && nDevices > 0 )
  // require( (nDevices % 8) == 0 )

  val device: SimpleDevice = new SimpleDevice("Platform-Level Interrupt Controller", Seq("wuhan university of technology, PLIC"))

  val node : TLRegisterNode = TLRegisterNode(
    address   = Seq(AddressSet(0x10000000L, 0x0fffffffL)),
    device    = device,
    beatBytes = 8,
    undefZero = true,
    concurrency = 1)


  abstract class PlicBase extends LazyModuleImp(this){
    val io = IO(new Bundle{
      val interrupt = Input( Vec(nDevices, Bool()) )
      val context = Output( Vec( nHarts, Bool()) )
    })

    // val gateWaysReady    = Wire( (Vec(nDevices, Bool())) )
    // val gateWaysValid    = Wire( (Vec(nDevices, Bool())) )
    // val gateWaysComplete = Wire( (Vec(nDevices, Bool())) )

    val prioBits = log2Ceil(nPriorities+1); require( prioBits <= 32 )
    val priority  = Reg(Vec(nDevices, UInt(prioBits.W)))

    val threshold = Reg(Vec(nHarts, UInt(prioBits.W)))

    val pending   = RegInit(VecInit( Seq.fill(nDevices){false.B} ))
    val pendingUInt = Cat(pending.reverse)
    
    val enables = Seq.fill(nHarts) { Seq.fill(nDevices){ RegInit(false.B) } }
    val enableVec  = VecInit(enables.map(x => Cat(x.reverse)))
    val enableVec0 = VecInit(enableVec.map(x => Cat(x, 0.U(1.W))))

    val maxDevs = Reg(Vec(nHarts, UInt((log2Ceil(nDevices+1).W))))


    val claimer = Wire(Vec(nHarts, Bool()))
    // dontTouch(claimer)
    assert( PopCount(claimer) <= 1.U )

    val completer = Wire(Vec(nHarts, Bool()))
    assert( PopCount(completer) <= 1.U)

    val claiming    = Seq.tabulate(nHarts){i => Mux(claimer(i), maxDevs(i), 0.U)}.reduceLeft(_|_)
  
    // Mux1H(( 0 until nHarts ).map{ i => ((claimer(i) === true.B), maxDevs(i)) })

    val claimedDevs = UIntToOH(claiming, nDevices+1).asBools

    val completerDev  = Wire(UInt((log2Ceil(nDevices + 1)).W))
    val completedDevs = Mux( completer.contains( true.B ), UIntToOH(completerDev, nDevices+1), 0.U )
  }

  trait PlicGateWays{ this: PlicBase =>

    val inFlight = RegInit( VecInit(Seq.fill(nDevices){false.B}))


    //There are nDevices geatways
    for ( i <- 0 until nDevices ) {
      when (completedDevs(i+1)) { inFlight(i) := false.B }      
      .elsewhen ( io.interrupt(i) & ~pending(i) ) { inFlight(i) := true.B }

      when ( claimedDevs(i+1) ) { pending(i) := false.B }
      .elsewhen ( io.interrupt(i) & ~inFlight(i) ) { pending(i) := true.B }
    }



  }

  trait PlicCore{ this: PlicBase =>

    for ( i <- 0 until nHarts ) {

      /** for the one, that is not enable or noPending, will not be compared*/


      val effectivePriority = Seq( 1.U << prioBits) ++ ( 0 until nDevices ).map{ j =>
        val isActive = enables(i)(j) & pending(j)
        Cat(isActive, priority(j))
      }

      val (maxPri, maxDev) = findMax(effectivePriority)
      io.context(i) := ShiftRegister(RegNext(maxPri(prioBits-1, 0)) > threshold(i), 2)
      maxDevs(i) := maxDev

    }

    /** Dichotomy to find the maximum value */
    def findMax(x: Seq[UInt]): (UInt, UInt) = {
      if ( x.length > 1 ) {
        val half = 1 << (log2Ceil(x.length) - 1)
        val left = findMax(x take half)
        val right = findMax(x drop half)

        val pri = Wire( UInt((prioBits+1).W ) )
        val dev = Wire( UInt(log2Ceil(nDevices+1).W)  )
        when( left._1 >= right._1 ) { pri := left._1; dev := left._2 }
        .otherwise{ pri := right._1; dev := half.U | right._2 } //right-side should plus offset
        ( pri, dev )
      } else {
        ( x.head, 0.U )        
      }
    }
  }

  class PlicImp extends PlicBase with PlicGateWays with PlicCore {
    val priorityRegFields = Seq(
      0x000000 -> RegFieldGroup( s"priority", Some(s"Acting priority of interrupt source"), Seq(
        RegField(32)) ++ ( 0 until nDevices ).map{ i =>
          RegField( 32, priority(i), RegFieldDesc(
                                                    name  = s"priority_$i",
                                                    desc  = s"Acting priority of interrupt source $i",
                                                    reset = None)
                                                  )
        }
      )
    )
        
    val pendingRegFields = Seq(
      0x001000 -> RegFieldGroup("pending", Some("Pending Bit Array. 1 Bit for each interrupt source."), Seq(
        RegField(1)) ++ ( 0 until nDevices ).map{ i =>
          RegField.r(1, pending(i), RegFieldDesc( s"pending_$i", s"Set to 1 if interrupt source $i is pending, regardless of its enable or priority setting.", volatile = true) )
        }
      )
    )

    val enableRegFields = ( 0 until nHarts ).map{ i =>
      (0x002000 + 0x80*i) -> RegFieldGroup( s"enables_${i}", Some(s"Enable bits for each interrupt source for target $i. 1 bit for each interrupt source."), 
        Seq( RegField(1) ) ++ ( 0 until nDevices ).map{ j =>
          RegField( 1, enables(i)(j), RegFieldDesc(
            name      = s"enables_${j}",
            desc      = s"Set bits to 1 if interrupt should be enabled.") ) 
      })
    }
    







    val hartRegFields = ( 0 until nHarts ).map{ i =>
      (0x200000 + 0x1000*i) -> RegFieldGroup( s"hart_${i}", Some(s"hart_${i}"), Seq(
      RegField(32, threshold(i), RegFieldDesc( name = s"threshold_$i", desc = s"Interrupt & claim threshold for target $i. Maximum value is ${nPriorities}.")),
      RegField(32,
        RegReadFn { valid => {claimer(i) := valid; ( true.B, maxDevs(i) )} },
        RegWriteFn { (valid, data) =>
          //assert(completerDev === data(),  "completerDev should be consistent for all harts")
          completerDev := data
          completer(i) := valid && enableVec0(i)(completerDev)
          true.B
        },
        Some(RegFieldDesc(s"claim_complete_$i",
          s"Claim/Complete register for Target $i. Reading this register returns the claimed interrupt number and makes it no longer pending." +
          s"Writing the interrupt number back completes the interrupt.",
          reset = None,
          wrType = Some(RegFieldWrType.MODIFY),
          rdAction = Some(RegFieldRdAction.MODIFY),
          volatile = true))
      )))
    }

    node.regmap((priorityRegFields ++ pendingRegFields ++ enableRegFields ++ hartRegFields):_*)
  }

  lazy val module = new PlicImp {
  }
}

