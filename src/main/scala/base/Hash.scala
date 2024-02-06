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

object HashHalf0{
  def apply( in: UInt ): UInt = {
    val in_w = in.getWidth
    // require(in_w % 2 == 0, "in_w is "+in_w )


    val out = 
      if(in_w % 2 == 0) {Wire( UInt((in_w/2).W) )}
      else {Wire( UInt(((in_w/2) + 1).W) )}

    out := in(in_w/2-1,0) ^ in(in_w-1 , in_w/2)

    return out
  }  
}


object HashHalf1{
  def apply( in: UInt ): UInt = {
    val in_w = in.getWidth
    // require(in_w % 2 == 0, "in_w is "+in_w )

    val out = 
      if(in_w % 2 == 0) {Wire( UInt((in_w/2).W) )}
      else {Wire( UInt(((in_w/2) + 1).W) )}

    out := in(in_w/2-1, 0) ^ (Reverse(in))(in_w/2-1, 0)

    return out
  }  
}


object HashHalf2{
  def apply( in: UInt ): UInt = {
    val in_w = in.getWidth
    // require(in_w % 2 == 0, "in_w is "+in_w )

    val out = 
      if(in_w % 2 == 0) {Wire( UInt((in_w/2).W) )}
      else {Wire( UInt(((in_w/2) + 1).W) )}

    for ( i <- 0 until in_w/2 ) yield {
      out(i) := in(2*i) ^ in(2*i+1)  
    }

    return out.asUInt
  }  
}


object HashTwo0{
  def apply( in1: UInt, in2: UInt ): UInt = {
    val in1_w = in1.getWidth
    val in2_w = in2.getWidth
    
    // require(in1_w == in2_w, "in1_w is "+in1_w+"in2_w is "+in2_w )

    val out = Wire( UInt((in1_w max in2_w).W) )

    out := in1 ^ in2

    return out
  }  
}


object HashTwo1{
  def apply( in1: UInt, in2: UInt ): UInt = {
    val in1_w = in1.getWidth
    val in2_w = in2.getWidth
    
    // require(in1_w == in2_w, "in1_w is "+in1_w+"in2_w is "+in2_w)

    val out = Wire( UInt((in1_w max in2_w).W) )


    out := in1 ^ Reverse(in2)

    return out
  }  
}





object HashTo0{
  def apply( in: UInt, len: Int ): UInt = {
    val in_w = in.getWidth
      
    require( in_w > len )

    val out = Wire( UInt(len.W) )
    if ( 2*len >= in_w ) {
      val in1 = ~in(len-1, 0)
      val in2 = in(in_w-1, in_w-1-len)


      out := in1 ^ in2
 
    } else {
      out := HashTo0( HashHalf0(in), len )
    }
    return out      
  }  
}



object HashTo1{
  def apply( in: UInt, len: Int ): UInt = {
    val in_w = in.getWidth
      
    require( in_w > len )

    val out = Wire( UInt(len.W) )
    if ( 2*len >= in_w ) {
      val in1 = ~in(len-1, 0)
      val in2 = Reverse(in(in_w-1, in_w-1-len))

      out := in1 ^ in2
          
    } else {
      out := HashTo1( HashHalf1(in), len )
    }
    return out 

  }  
}

