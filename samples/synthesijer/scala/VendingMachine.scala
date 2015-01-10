package synthesijer.scala

class VendingMachine(n:String, c:String, r:String) extends Module(n, c, r){
  
  def this() = this("vendingmachine", "clk", "reset")
  
  val nickel = inP("nickel")
  val dime = inP("dime")
  val rdy = outP("rdy")
  val seq = sequencer("main")
  
  val s5,s10,s15,s_ok = seq.add()
  rdy <= seq.idle * LOW
  
  rdy <= s_ok * HIGH
  seq.idle * nickel -> s5
  seq.idle * dime -> s10

  s5 * nickel -> s10
  s5 * dime -> s15

  s10 * nickel -> s15
  s10 * dime -> s_ok

  s15 * nickel -> s_ok
  s15 * dime -> s_ok

  s_ok -> seq.idle

}