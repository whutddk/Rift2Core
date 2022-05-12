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

object HashHalf0{
  def apply( in: UInt ): UInt = {
    val in_w = in.getWidth
    require(in_w % 2 == 0)

    val out = Wire( UInt((in_w/2).W) )

    for ( i <- 0 until in_w/2 ) yield {
      out(i) := in(i) ^ in(i+1)
    }
    return out
  }  
}


object HashHalf1{
  def apply( in: UInt ): UInt = {
    val in_w = in.getWidth
    require(in_w % 2 == 0)

    val out = Wire( UInt((in_w/2).W) )

    for ( i <- 0 until in_w/2 ) yield {
      out(i) := in(i) ^ Reverse(in)(i)
    }
    return out
  }  
}


object HashHalf2{
  def apply( in: UInt ): UInt = {
    val in_w = in.getWidth
    require(in_w % 2 == 0)

    val out = Wire( UInt((in_w/2).W) )

    for ( i <- 0 until in_w/2 ) yield {
      out(i) := in(i) ^ in(in_w/2+i)
    }
    return out
  }  
}


object HashTwo0{
  def apply( in1: UInt, in2: UInt ): UInt = {
    val in1_w = in1.getWidth
    val in2_w = in2.getWidth
    
    require(in1_w == in2_w)

    val out = Wire( UInt(in1_w.W) )

    for ( i <- 0 until in1_w ) yield {
      out(i) := in1(i) ^ in2(i)
    }
    return out
  }  
}


object HashTwo1{
  def apply( in1: UInt, in2: UInt ): UInt = {
    val in1_w = in1.getWidth
    val in2_w = in2.getWidth
    
    require(in1_w == in2_w)

    val out = Wire( UInt(in1_w.W) )

    for ( i <- 0 until in1_w ) yield {
      out(i) := in1(i) ^ Reverse(in2)(i)
    }
    return out
  }  
}





object HashTo0{
  def apply( in: UInt, len: Int ): UInt = {
    val in_w = in.getWidth
      
    require( in_w > len )


    if ( 2*len > in_w ) {
      val in1 = ~in(len, 0)
      val in2 = in(in_w-1, in_w-1-len)

      val out = Wire( UInt(len.W) )

      for ( i <- 0 until len ) yield {
        out(i) := in1(i) ^ in2(i)
      }
      return out      
    } else {
      return HashTo0( HashHalf0(in), len/2 )
    }

  }  
}



object HashTo1{
  def apply( in: UInt, len: Int ): UInt = {
    val in_w = in.getWidth
      
    require( in_w > len )


    if ( 2*len > in_w ) {
      val in1 = ~in(len, 0)
      val in2 = Reverse(in(in_w-1, in_w-1-len))

      val out = Wire( UInt(len.W) )

      for ( i <- 0 until len ) yield {
        out(i) := in1(i) ^ in2(i)
      }
      return out      
    } else {
      return HashTo1( HashHalf1(in), len/2 )
    }

  }  
}

