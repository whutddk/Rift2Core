

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

package base

import chisel3._
import chisel3.util._


class XArbiter[T <: Data]( gen: T, in: Int,out: Int) extends Module {
  val io = IO(new Bundle{
    val enq = Vec(in, Flipped(Decoupled(gen)))
    val deq = Vec(out, Decoupled(gen))
    val chosen = Output(Vec(out, UInt(log2Ceil(in).W) ))
  })


  for ( i <- 0 until out ) {
    io.chosen(i) := 0.U //init
    for ( j <- in-1 to 0 by -1 ) {
      when( io.enq(j).valid ) {
        if ( i == 0 ) { io.chosen(i) := j.U } 
        else {
          when( j.U > io.chosen(i-1) ) {
            io.chosen(i) := j.U
          }
        }
      }
    }    
  }


  /** init */
  for ( i <- 0 until out ) yield { io.deq(i).valid := false.B; io.deq(i).bits := DontCare }
  for ( i <- 0 until in ) yield { io.enq(i).ready := false.B }

  for ( i <- 0 until out ) yield {
    when( io.enq.count( (x:DecoupledIO[T]) => x.valid === true.B ) > i.U ) {
      val idx = io.chosen(i)
      io.deq(i).bits := io.enq(idx).bits
      io.deq(i).valid := true.B
      io.enq(idx).ready := io.deq(i).ready
    }
  }





}



