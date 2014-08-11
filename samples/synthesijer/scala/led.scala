package synthesijer.scala

import synthesijer.hdl.HDLPort
import synthesijer.hdl.HDLPrimitiveType
import synthesijer.hdl.HDLOp
import synthesijer.hdl.expr.HDLPreDefinedConstant

object led {
  
  def generate_led() : Module = {
    val m = new Module("led")
    val q = m.outP("q")
    val counter = m.signal("counter", 32)
    q <= m.expr(HDLOp.REF, counter, 5)
    
    // at main state, counter <= counter + 1
    val seq = m.newSequencer("main");
    val ss = seq.getIdleState();
    counter <= (ss, m.expr(HDLOp.ADD, counter, 1));
    return m
  }
  
  def generate_sim(target:Module, name:String) : SimModule = {

	  val sim = new SimModule(name)
	  val inst = sim.newModuleInstance(target, "U");

	  val clk = sim.signal("clk")
	  val reset = sim.signal("reset")
	  val counter = sim.signal("counter", 32);

	  val seq = sim.sequencer("main");
	  seq.tick(10);

	  val ss = seq.idle;
	  val s0 = seq.add("S0");
	  ss -> s0;
	  s0 -> ss;

	  clk <= (ss, HDLPreDefinedConstant.LOW);
	  clk <= (s0, HDLPreDefinedConstant.HIGH);

	  val expr = sim.expr(HDLOp.ADD, counter, 1);
	  counter <= (ss, expr);
	  counter <= (s0, expr);

	  reset.signal.setResetValue(HDLPreDefinedConstant.LOW);
	  reset <= (ss, sim.expr(HDLOp.IF,
			                     sim.expr(HDLOp.AND, sim.expr(HDLOp.GT, counter, 3), sim.expr(HDLOp.LT, counter, 8)),
			                     HDLPreDefinedConstant.HIGH,
			                     HDLPreDefinedConstant.LOW));

	  inst.getSignalForPort("clk").setAssign(null, clk.signal);
	  inst.getSignalForPort("reset").setAssign(null, reset.signal);
	  
	  return sim
  }

  
  def main(args:Array[String]) = {
    val led = generate_led()
    val sim = generate_sim(led, "led_sim")
    led.genVHDL()
    sim.genVHDL()
  }

}