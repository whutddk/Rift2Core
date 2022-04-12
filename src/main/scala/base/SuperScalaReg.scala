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

package base._


import chisel3._
import chisel3.util._

object SuperscalarReg {
  def apply[T<:Data]( init: T, is_reitred: Seq[Bool] ): (T, Seq[T]) = 
  val len = is_reitred.length
  val qout = RegInit(init)
  val dnxt = WireDefault( VecInit( Seq.fill(len)(qout) ) )
  qout := MuxCase(qout, Array( ( len-1 to 0 ).map{ i => is_retired(i) -> dnxt(i) } ) )

  return ( qout, dnxt )
}

