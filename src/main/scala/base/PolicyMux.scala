
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
package base

import chisel3._
import chisel3.util._

object PolicyMux{
  def apply[T <: Data]( in: Iterable[(Bool, T)], isMinArea: Boolean, isLowLatency: Boolean ): T = {
    require( (!isMinArea && isLowLatency) || (isMinArea && !isLowLatency) )
    assert(PopCount( in.map(_._1) ) <= 1.U )
    if( isMinArea ) {
      PriorityMux(in.toSeq )
    } else {
      Mux1H(in)
    }
  }
}
