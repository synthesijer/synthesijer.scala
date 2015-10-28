package synthesijer.scala.xilinx.series7

/**
 * @author miyo
 */

import synthesijer.scala._
import synthesijer.scala.xilinx._

import synthesijer.hdl.HDLSignal

class LUT_OR_MEM6 extends CombinationLogic("LUT_OR_MEM6") {
  
  val O5 = outP("O5")
  val O6 = outP("O6")
  val MC31 = outP("MC31")
  
  val WE   = inP("WE")
  val CLK  = inP("CLK")
  val DI1  = inP("DI1")
  val DI2  = inP("DI2")
  val WA   = for(i <- 1 to 8) yield inP("WA" + i)
  val A    = for(i <- 1 to 6) yield inP("A" + i)
  
}

class LUT_OR_MEM6_test() extends Module("LUT_OR_MEM6_test", "clk", "reset") {
  val m = new LUT_OR_MEM6()
  val u = instance(m)
  u.signalFor(m.CLK) := sysClk

  val O5 = outP("O5")
  val O6 = outP("O6")
  val MC31 = outP("MC31")
  
  val WE   = inP("WE")
  val DI1  = inP("DI1")
  val DI2  = inP("DI2")
  val WA   = for(i <- 1 to 8) yield inP("WA" + i)
  val A    = for(i <- 1 to 6) yield inP("A" + i)

  O5 := u.signalFor(m.O5)
  O6 := u.signalFor(m.O6)
  MC31 := u.signalFor(m.MC31)

  u.signalFor(m.WE) := WE
  u.signalFor(m.DI1) := DI1
  u.signalFor(m.DI2) := DI2
  for(i <- 0 to 5) { u.signalFor(m.A(i)) := A(i) }
  for(i <- 0 to 7) { u.signalFor(m.WA(i)) := WA(i) }

}

object LUT_OR_MEM6 {

  private val tmpl = new LUT_OR_MEM6()
  
  def main(args:Array[String]) = {
    val test = new LUT_OR_MEM6_test()
    Xilinx.init(test)
    test.genVHDL()
  }
  
}
