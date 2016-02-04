package synthesijer.scala

import synthesijer.scala._

class mkJigsaw(w:Int, h:Int) extends Module("jigsaw", "clk", "reset"){

  val piece_null = value(0, 2)

  val din = inP("din", h * 2)
  val kick = inP("kick")

  val dout = outP("dout", w)
  val valid = outP("valid"); valid.default(LOW)

  val ZERO_P  = signal("ZERO_P", 2);  ZERO_P  := value(0, 2)
  val ONE_P   = signal("ONE_P", 2);   ONE_P   := value(1, 2)
  val TWO_P   = signal("TWO_P", 2);   TWO_P   := value(2, 2)
  val UNKNOWN = signal("UNKNOWN", 2); UNKNOWN := value(3, 2)

  // initialization of each piece (lldd)
  val map = for(i <- 0 until w * h; val s = signal("signal_" + i, 4)) yield {
    s.reset(value(0xf,4))
    s
  }

  // return new piece expr(lldd)
  def next_piece(u:ExprItem, r:ExprItem) = {
    ?(u == ZERO_P and r == ZERO_P, ZERO_P & ZERO_P,
    ?(u == ZERO_P and r == ONE_P,  ZERO_P & ONE_P,
    ?(u == ZERO_P and r == TWO_P,  ONE_P  & ZERO_P,
    ?(u == ONE_P  and r == ZERO_P, ONE_P  & ONE_P,
    ?(u == ONE_P  and r == ONE_P,  TWO_P  & ZERO_P,
    ?(u == ONE_P  and r == TWO_P,  TWO_P  & ONE_P,
    UNKNOWN & UNKNOWN))))))
  }

  // stop condition
  var stop_flag:ExprItem = HIGH
  for(i <- 0 until h) {
    val e = ?(map((i+1)*w-1).range(3, 2) == ZERO_P, HIGH, LOW)
    stop_flag = stop_flag and e
  }

  // result
  var result:ExprItem = ?(map(w*(h-1)).range(1, 0) == ZERO_P, value(0,1), value(1,1))
  for(i <- 1 until w) {
    val e = ?(map(w*(h-1)+i).range(1, 0) == ZERO_P, value(0,1), value(1,1))
    result = e & result
  }

  val seq = sequencer("main")
  val s0 = seq.idle * kick -> seq.add()

  for(i <- 0 until w * h){
    if(i == 0){
      val r = din.range(1, 0) // input[0]
      map(i) <= s0 * next_piece(ZERO_P, r)
    }else if(i%w == 0){ // right edge
      val r = din.range((i / w) * 2+1, (i / w) * 2) // input[i/w]
      val u = map(i-w).range(1, 0)  // dd of the upper piece
      map(i) <= s0 * next_piece(u, r)
    }else if(i/w == 0){ // upper edge
      val r = map(i-1).range(3, 2) // ll of the right piece
      map(i) <= s0 * next_piece(ZERO_P, r)
    }else{
      val u = map(i-w).range(1, 0) // dd of the upper piece
      val r = map(i-1).range(3, 2) // ll of the right piece
      map(i) <= s0 * next_piece(u, r)
    }
  }
  val s1 = s0 * (stop_flag == HIGH) -> seq.add()

  valid <= s1 * HIGH
  dout <= s1 * result

  s1 -> seq.idle

}

class JigsawSim(m:mkJigsaw) extends SimModule("jigsaw_sim"){
  
  val (clk, reset, counter) = system(10)
  
  val inst = instance(m, "U")
  inst.sysClk := clk
  inst.sysReset := reset
  
  inst.signalFor(m.kick) $ clk := ?(counter == 100, HIGH, LOW)
  
}


object mkJigsaw {

  def main(args:Array[String]) = {
    val m = new mkJigsaw(5, 3)
    val sim = new JigsawSim(m)
    // 102 -> 01_00_10 -> 0x12
    sim.inst.signalFor(m.din) := sim.value(0x12, 6)

    m.genVHDL()
    m.genVerilog()
    sim.genVHDL()
    sim.genVerilog()
  }

}
