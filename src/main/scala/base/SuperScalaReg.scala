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

// object SuperscalarReg {
//   def apply[T<:Data]( init: T, is_retired: Seq[Bool] ): (T, Seq[T]) = {
//     val len = is_retired.length
//     val qout = RegInit(init)
//     val dnxt = Wire(Vec( len, new chiselCloneType(init) ))
    
//     // WireDefault( VecInit( Seq.fill(len)(qout) ) )
//     ( 0 until len ).map{ t => { if ( t == 0 ) dnxt(t) := qout else dnxt(t) := dnxt(t-1) } }
//     qout := MuxCase(qout,  ( len-1 to 0 ).map{ i => {is_retired(i) -> dnxt(i)} }  )

//     // def idx_fun( i: Int ) = {
//     //   if ( i >=0 && i < len ) dnxt(0)
//     //   else qout
//     // } 

//     return ( qout, dnxt )
//   }

// }

// object PkgSSReg {

// }

