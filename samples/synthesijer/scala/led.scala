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
    counter <= (seq.idle, m.VECTOR_ZERO)
    counter <= (s0, m.expr(Op.+, counter, 1))
    return m
  }
  
  def generate_sim(target:Module, name:String) : SimModule = {

	  val sim = new SimModule(name)
	  val inst = sim.instance(target, "U")
	  
	  val (clk, reset, counter) = sim.system(10)

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