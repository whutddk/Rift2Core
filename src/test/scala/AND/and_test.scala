package test
 
import chisel3._
 
object testMain extends App {
  Driver.execute(args, () => new AND)
}
