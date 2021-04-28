

/*
* @Author: Ruige Lee
* @Date:   2021-04-27 19:49:17
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-28 10:52:18
*/


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

package base


import chisel3._
import chisel3.util._


object PMux {
	def apply(mapping: Seq[(Bool, UInt)]): UInt = {
		var res = 0.U
		var chk = false.B
		for ((t, v) <- mapping){

			assert( chk === true.B & t =/= true.B, "Assert Fail at PMux, two true.B happend" )

			res = res | Mux( t, v, 0.U )
			chk = chk | t

		}

	res
	}
}






// object MuxCase {
//   * @param default the default value if none are enabled
//     * @param mapping a set of data values with associated enables
//     * @return the first value in mapping that is enabled 
//   def apply[T <: Data] (default: T, mapping: Seq[(Bool, T)]): T = {
//     var res = default
//     for ((t, v) <- mapping.reverse){
//       res = Mux(t, v, res)
//     }
//     res
//   }
// }








