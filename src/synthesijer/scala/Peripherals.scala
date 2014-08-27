package synthesijer.scala

class BRAM(m:Module, prefix:String, width:Int){
	val address = m.outP(prefix + "_address", 32)
  val we = m.outP(prefix + "_we")
  val din = m.inP(prefix  + "_din", width)
  val dout = m.outP(prefix + "_dout", width)
}

class FIFO_OUT(m:Module, prefix:String, width:Int){
  val we = m.outP(prefix + "_we")
  val dout = m.outP(prefix + "_dout", width)
  val full = m.inP(prefix + "_full")
}

class FIFO_IN(m:Module, prefix:String, width:Int){
  val re = m.outP(prefix + "_re")
  val din = m.outP(prefix + "_din", width)
  val empty = m.inP(prefix + "_empty")
}
