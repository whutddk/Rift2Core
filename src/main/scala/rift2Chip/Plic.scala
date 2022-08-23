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


package rift2Chip

import chisel3._
import chisel3.util._

class GatewayPLICIO extends Bundle{
  val valid = Output(Bool())
  val ready = Output(Bool())
  val complete = Input(Bool())
}

class LevelGateway extends Module{
  val io = new Bundle{
    val interrupt = Input(Bool())
    val plic = new GatewayPLICIO
  }

  val inFlight = RegInit(Bool())
  when (io.interrupt && io.plic.ready) { inFlight := true }
  when (io.plic.complete) { inFlight := false }
  io.plic.valid := io.interrupt && !inFlight
}

// class PLICFanIn(nDevices: Int, prioBits: Int) extends Module {

// }




class Plic( nHarts: Int = 1, nPriorities: Int = 8, nDevices: Int = 1023 )(implicit p: Parameters) extends LazyModule {
  required( nHarts > 0 )
  required( nPriorities != 0 )
  required( isPow2(nPriorities) )
  required( nDevices < 1024 && nDevices > 0 )
  required( (nDevices % 8) == 0 )

  val device: SimpleDevice = new SimpleDevice("Platform-Level Interrupt Controller", Seq("wuhan university of technology, PLIC"))

  val node : TLRegisterNode = TLRegisterNode(
    address   = Seq(params.address),
    device    = device,
    beatBytes = 8,
    undefZero = true,
    concurrency = 1)


  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle{
      val interrupt = Input( Vec(nDevices, Bool()) )

      val int = Output( Vec( nHarts, new Plic_Bundle) )
    }

    //There are nDevices geatways
    val gateways = io.interrupts.map { case int =>
      val gateway = Module(new LevelGateway)
      gateway.io.interrupt := int
      gateway.io.plic
    }


    val prioBits = log2Ceil(nPriorities+1); require( prioBits <= 32 )
    val priority  = Reg(Vec(nDevices, UInt(prioBits.W)))

    val threshold = Reg(Vec(nHarts, UInt(prioBits.W)))

    val pending   = RegInit(VecInit( Seq.fill(nDevices){false.B} ))
    val pendingUInt = Cat(pending.reverse)
    
    val enables = Seq.fill(nHarts) { Seq.fill(nDevices){ RegInit(false.B) } }
    val enableVec = Vec(enables.map(x => Cat(x.reverse)))



    val maxDevs = Reg(Vec(nHarts, UInt(width = log2Ceil(nDevices+1))))

    ( 0 unitl nHarts ).map{ i =>

      /** for the one, that is not enable or noPending, will not be compared*/
      val effectivePriority = ( 1.U << prioBits) +: ((enableVec(i) & pendingUInt) zip priority).map { case (p, x) => Cat(p, x) }

      val (maxPri, maxDev) = findMax(effectivePriority)
      io.int(i).mei := ShiftRegister(RegNext(maxPri) > threshold(i), 2)
      io.int(i).sei := ShiftRegister(RegNext(maxPri) > threshold(i), 2)
      maxDevs(i) := maxDev

    }

    /** Dichotomy to find the maximum value */
    def findMax(x: Seq[UInt]): (UInt, UInt) = {
      if ( x.length > 1 ) {
        val half = 1 << (log2Ceil(x.length) - 1)
        val left = findMax(x take half)
        val right = findMax(x drop half)
        when( left._1 >= right._1 ) { return( left._1, left._2 ) }
        .otherwise{ return(right._1, (half.U) | right._2) } //right-side should plus offset
      } else {
        return ( x.head, 0.U )        
      }
    }





    val priorityRegFields = 
      0x000000 -> RegFieldGroup( s"priority", Some(s"Acting priority of interrupt source"), Seq(
        RegField(32)) ++ ( 0 until nDevices ).map{ i =>
          RegField( 32, priority(i), RegFieldDesc(
                                                    name  = s"priority_$i",
                                                    desc  = s"Acting priority of interrupt source $i",
                                                    reset = None)
                                                  )
        }
      )
    
    
    // ( 0 until nDevices ).map{ i =>
    //   PLICConsts.priorityBase+PLICConsts.priorityBytes*(i+1) -> RegField(prioBits, priority(i), RegFieldDesc(
    //                                                               name      = s"priority_$i",
    //                                                               desc      = s"Acting priority of interrupt source $i",
    //                                                               group     = Some(s"priority_${i}"),
    //                                                               groupDesc = Some(s"Acting priority of interrupt source ${i}"),
    //                                                               reset     = None)
    //                                                             )
    // }

    
    val pendingRegFields = 
      0x001000 -> RegFieldGroup("pending", Some("Pending Bit Array. 1 Bit for each interrupt source."), Seq(
        RegField(1)) ++ ( 0 until nDevices ).map{ i =>
          RegField.r(1, pending(i), RegFieldDesc( s"pending_$i", s"Set to 1 if interrupt source $i is pending, regardless of its enable or priority setting.", volatile = true)
        }
      )

    // Seq(
    //   PLICConsts.pendingBase -> ( RegField(1) +: pending.zipWithIndex.map { case (b, i) =>
    //     RegField.r(1, b, pendingRegDesc(i+1))
    //   }))

    val enableRegFields = ( 0 until nHarts ).map{ i =>
      (0x002000 + 0x80*i) -> RegFieldGroup( s"enables_${i}", Some(s"Enable bits for each interrupt source for target $i. 1 bit for each interrupt source."), 
        Seq( RegField(1) ) ++ ( 0 until nDevices ).map{ j =>
          RegField( 1, enables(i)(j), RegFieldDesc(
            name      = s"enables_${j}",
            desc      = s"Set bits to 1 if interrupt should be enabled.") ) 
      }
    }
    
    // enables.zipWithIndex.map { case (e, i) =>
    //   PLICConsts.enableBase(i) -> ( RegField(1) +: e.zipWithIndex.map { case (x, j) =>
    //     RegField(x.getWidth, x, enableRegDesc(i, j, x.getWidth)) 
    //   })}


    val claimer = Wire(Vec(nHarts, Bool()))
    assert( PopCount(claimer) <= 1.U )

    val completer = Wire(Vec(nHarts, Bool()))
    assert( PopCount(completer) <= 1.U)


    val claiming = Mux1H(( 0 until nHarts ).map{ claimer(i) -> maxDevs(i) })
    val claimedDevs = Vec(UIntToOH(claiming, nDevices+1).asBools)

    val completerDev = Wire(UInt((log2Ceil(nDevices + 1)).W))
    val completedDevs = Mux( completer.contains( x => (x === true.B) ), UIntToOH(completerDev, nDevices+1), 0.U )

    ( 0 until nDevices ).map{ i =>
      gateways(i).ready := ~pending(i)
      when (claimedDevs(i) | gateways(i).valid) { pending(i) := ~claimedDevs(i) }
      gateways(i).complete := completedDevs(i)
    }

    // ((pending zip gateways) zip claimedDevs.tail) foreach { case ((pend, gate), claim) =>
    //   gate.ready := ~pend
    //   when (claim || gate.valid) { pend := ~claim }
    // }




    // (gateways zip completedDevs.asBools.tail) foreach { case (gate, complete) =>
    //    gate.complete := complete
    // }

    def thresholdRegField(x: UInt, i: Int) = RegField(prioBits, x, thresholdRegDesc(i))


    val hartRegFields = ( 0 until nHarts ).map{ i =>
      (0x200000 + 0x1000*i) -> RegFieldGroup( s"hart_${i}", Some(s"hart_${i}"), 
      RegField(32, threshold(i), RegFieldDesc( name = s"threshold_$i", desc = s"Interrupt & claim threshold for target $i. Maximum value is ${nPriorities}.")
      RegField(32,
        RegReadFn { valid => {claimer(i) := valid; ( true.B, maxDevs(i) )} },
        RegWriteFn { (valid, data) =>
          assert(completerDev === data.extract(log2Ceil(nDevices+1)-1, 0),  "completerDev should be consistent for all harts")
          completerDev := data.extract(log2Ceil(nDevices+1)-1, 0)
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
      )
    }

    node.regmap((priorityRegFields ++ pendingRegFields ++ enableRegFields ++ hartRegFields):_*)


  }
}

