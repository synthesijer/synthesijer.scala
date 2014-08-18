package synthesijer.scala

class UPLIn(m:Module, prefix:String, width:Int){
	val req = m.inP(prefix  + "Req")
  val en = m.inP(prefix  + "En")
  val ack = m.outP(prefix + "Ack")
  val data = m.inP(prefix  + "Data", width)
}

class UPLOut(m:Module, prefix:String, width:Int){
	val req = m.outP(prefix + "Req")
  val en = m.outP(prefix + "En")
  val ack = m.inP(prefix  + "Ack")
  val data = m.outP(prefix + "Data", width)
}
