package synthesijer.scala

class SimpleBlockRAM(name:String, clk:String, reset:String) extends Module(name, clk, reset){
  
	def this(width:Int, depth:Int, length:Int) = {
	  this("simple_dualportram", "clk", "reset")
	  parameter("WIDTH", width)
	  parameter("DEPTH", depth)
	  parameter("WORDS", length)
	  inP("we")
	  inP("oe")
	  outP("length", 32)
	  inP("raddress", depth)
	  outP("dout", width)
	  inP("waddress", depth)
	  inP("din", width)
  }

}