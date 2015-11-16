package synthesijer.scala.utils

import synthesijer.scala._

class ExtFifo(name:String, width:Int) extends Module(name, "clk", "srst"){

  val din   = inP("din", width)
  val dout  = outP("dout", width)
  val re    = inP("rd_en")
  val we    = inP("wr_en")
  val empty = outP("empty")
  val full  = outP("full")

}
