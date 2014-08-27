package synthesijer.scala

import synthesijer.scala._
import synthesijer.lib.SimpleBlockRAM

class Bram2Fifo(n:String, c:String, r:String, words:Int, width:Int, debug:Boolean) extends Module(n,c,r){
  
  def this(words:Int, width:Int, n:String) = this(n, "clk", "reset", words, width, true)
  
  def this(words:Int, width:Int) = this("bram2fifo", "clk", "reset", words, width, false)

  val init = inP("init")
  val kick = inP("kick")
  val busy = outP("busy")
  
  val bram = new BRAM(this, "bram", width)
  val fifo = new FIFO_OUT(this, "fifo", width)
  
  val seq = sequencer("main")
  
  val busy_reg = signal()
  busy <= expr(Op.or, busy_reg, expr(Op.or, kick, init))
  busy_reg <= (seq.idle, expr(Op.or, kick, init))
  busy_reg.default(Constant.HIGH)

  bram.we.default(Constant.LOW)
  fifo.we.default(Constant.LOW)
  bram.address <= (seq.idle, Constant.VECTOR_ZERO)
  
  val write_addr = signal(32)
  write_addr <= (seq.idle, Constant.VECTOR_ZERO)
  
  def gen_init_entry():State = {
    val s = seq.add()
    write_addr <= (s, expr(Op.+, write_addr, 1))
    bram.address <= (s, write_addr)
    if(debug){
    	bram.dout <= (s, write_addr)
    }else{
    	bram.dout <= (s, Constant.VECTOR_ZERO)
    }
    bram.we <= (s, Constant.HIGH)
    return s
  }

  def gen_emit_prepare():State = {
    val s = seq.add()
    bram.address <= (s, expr(Op.+, bram.address, 1))
    return s
  }

  def gen_emit_entry():State = {
    val s = seq.add()
    bram.address <= (s, expr(Op.+, bram.address, 1))
    fifo.we <= (s, Constant.HIGH)
    fifo.dout <= (s, bram.din)
    return s
  }

  val init_seq = gen_init_entry()
  val emit_seq = gen_emit_entry()
  seq.idle -> (init, init_seq)
  seq.idle -> (kick, gen_emit_prepare()) -> emit_seq
  init_seq -> (expr(Op.==, write_addr, words-1), seq.idle)
  emit_seq -> (expr(Op.==, bram.address, words), seq.idle)
}

class Bram2FifoSim(name:String, target:Bram2Fifo) extends SimModule(name){
  
	def this(target:Bram2Fifo) = this("bram2fifo_sim", target)

	val inst = instance(target, "U")
	val clk = signal("clk")
	val reset = signal("reset")
	val counter = signal("counter", 32)
	
	val ram = instance(new SimpleBlockRAM(32, 5, 32), "MEM")

	val seq = sequencer("main")
	seq.tick(10)

	val ss = seq.idle
	val s0 = seq.add("S0")
	ss -> s0 -> ss

	clk <= (ss, Constant.LOW)
	clk <= (s0, Constant.HIGH)

	val countup = expr(Op.+, counter, 1)
	counter <= (s0, countup)

	reset.reset(Constant.LOW)
	reset <= (ss, expr(Op.IF, expr(Op.and, expr(Op.>, counter, 3), expr(Op.<, counter, 8)), Constant.HIGH, Constant.LOW))
	
	inst.signalFor(target.kick) <= expr(Op.IF, expr(Op.==, counter, 200), Constant.HIGH, Constant.LOW)
	inst.signalFor(target.init) <= expr(Op.IF, expr(Op.==, counter, 10), Constant.HIGH, Constant.LOW)
			
	inst.sysClk <= clk
	inst.sysReset <= reset

	ram.signalFor("address_b") <= inst.signalFor("bram_address")
	ram.signalFor("we_b") <= inst.signalFor("bram_we")
	inst.signalFor("bram_din") <= ram.signalFor("dout_b") 
	ram.signalFor("din_b") <= inst.signalFor("bram_dout")
	ram.sysClk <= clk
}

object Bram2Fifo {

  def main(args:Array[String]) = {
	  val m = new Bram2Fifo(1024, 32)
	  m.visualize_statemachine();
	  m.genVHDL()
	  val debug = new Bram2Fifo(32,32,"bram2fifo_debug")
	  debug.genVHDL()
	  val sim = new Bram2FifoSim(debug)
	  sim.genVHDL()
  }
}

