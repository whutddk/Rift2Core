package test

/*
* @Author: Ruige Lee
* @Date:   2021-03-18 16:14:36
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-22 17:45:00
*/





import chisel3._
import rift2Core.Decode

object testMain extends App {
  Driver.execute(args, () => new Decode )
}



