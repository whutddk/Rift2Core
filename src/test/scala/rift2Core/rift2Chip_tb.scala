package test

/*
* @Author: Ruige Lee
* @Date:   2021-03-18 16:14:36
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-22 16:20:02
*/





import chisel3._
import _root_.rift2Core.decode.decode32
 
object testMain extends App {
  Driver.execute(args, () => new decode32 )
}



