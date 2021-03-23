package test

/*
* @Author: Ruige Lee
* @Date:   2021-03-18 16:14:36
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-23 11:07:47
*/





import chisel3._
import rift2Core._

object testMain extends App {
  Driver.execute(args, () => new Regfiles )
}



