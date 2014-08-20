package synthesijer.scala

object led {
  
  def generate_led() : Module = {
    val m = new Module("led")
    val q = m.outP("q")
    val counter = m.signal("counter", 32)
    q <= m.expr(Op.REF, counter, 5)
    
    // at main state, counter <= counter + 1
    val seq = m.sequencer("main")
    val s0 = seq.add()
    seq.idle -> s0
    counter <= (seq.idle, Constant.VECTOR_ZERO)
    counter <= (s0, m.expr(Op.+, counter, 1))
    return m
  }
  
  def generate_sim(target:Module, name:String) : SimModule = {

	  val sim = new SimModule(name)
	  val inst = sim.instance(target, "U")
	  
	  val clk = sim.signal("clk")
	  val reset = sim.signal("reset")
	  val counter = sim.signal("counter", 32)

	  val seq = sim.sequencer("main")
	  seq.tick(10)

	  val ss = seq.idle
	  val s0 = seq.add("S0")
	  ss -> s0 -> ss

	  clk <= (ss, Constant.LOW)
	  clk <= (s0, Constant.HIGH)

	  val expr = sim.expr(Op.+, counter, 1)
	  counter <= (ss, expr)
	  counter <= (s0, expr)

	  reset.reset(Constant.LOW)
	  reset <= (ss, sim.expr(Op.IF,
			                     sim.expr(Op.and, sim.expr(Op.>, counter, 3), sim.expr(Op.<, counter, 8)),
			                     Constant.HIGH,
			                     Constant.LOW))

	  inst.sysClk <= clk
	  inst.sysReset <= reset
	  
	  return sim
  }

  
  def main(args:Array[String]) = {
    val led = generate_led()
    val sim = generate_sim(led, "led_sim")
    led.genVHDL()
    sim.genVHDL()
  }

}