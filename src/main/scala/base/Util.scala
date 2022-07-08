

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

object sextXTo{
  def apply(x: UInt, n: Int): UInt = {
    require(x.getWidth <= n)
    if (x.getWidth == n) x
    else Cat(Fill(n - x.getWidth, x(x.getWidth-1)), x)
  }  
}


object padXTo{
  def apply(x: UInt, n: Int): UInt = {
    require(x.getWidth <= n)
    if (x.getWidth == n) x
    else Cat( (0.U)((n - x.getWidth).W), x)
  }  
}

object extVaddr{
  def apply(vaddr: UInt, vlen: Int): UInt = {
    require(vaddr.getWidth == vlen)
    val v64 = Wire(UInt(64.W))

    v64 := Cat( Fill(25, vaddr(vlen-1)), vaddr(vlen-1, 0) )
    return v64
  }  
}

