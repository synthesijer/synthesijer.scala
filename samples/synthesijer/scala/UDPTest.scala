package synthesijer.scala

class UPLIn(m:Module, prefix:String){
	val req = m.inP(prefix  + "Req")
  val en = m.inP(prefix  + "En")
  val ack = m.outP(prefix + "Ack")
  val data = m.inP(prefix  + "Data", 32)
}

class UPLOut(m:Module, prefix:String){
	val req = m.outP(prefix + "Req")
  val en = m.outP(prefix + "En")
  val ack = m.inP(prefix  + "Ack")
  val data = m.outP(prefix + "Data", 32)
}

object UPLTest {

  def gen_module() : Module = {
 
    val m = new Module("UPLTest")
    val uplin = new UPLIn(m, "pI0")
    val uplout = new UPLOut(m, "pO0")
 
    val ipaddr = m.inP("pMyIpAddr", 32)
    val port = m.inP("pMyPort", 16)
    val server_addr = m.inP("pServerIpAddr", 32)
    val server_port = m.inP("pServerPort", 16)

    val trigger = m.inP("trigger")
  
    // reset
    uplout.req.reset(Constant.LOW)
    uplout.en.reset(Constant.LOW)
    uplin.ack.reset(Constant.LOW)
 
    val sequencer = m.sequencer("main")
    val idle = sequencer.idle
    
    def wait_trigger(s:State):State = {
    	uplout.req <= (s, Constant.LOW)
    	uplout.en <= (s, Constant.LOW)
    	uplout.data <= (s, Constant.VECTOR_ZERO)
    	return s
    }

    val ack_ready = m.expr(Op.==, uplout.ack, Constant.HIGH)
    def wait_ack_and_send_data(s:State):State = {
    	uplout.data <= (s, ipaddr)
    	uplout.en <= (s, m.expr(Op.IF, ack_ready, Constant.HIGH, Constant.LOW))
    	uplout.req <= (s, m.expr(Op.IF, ack_ready, Constant.LOW, Constant.HIGH))
    	uplout.data <= (s, ipaddr)
    	return s
    }
    
    def send_dest_addr(s:State):State = {
    	uplout.data <= (s, server_addr)
      return s
    }

    def send_port(s:State):State = {
    	uplout.data <= (s, m.expr(Op.&, port, server_port))
      return s
    }
    
    def send_length(s:State):State = {
      uplout.data <= (s, new Value(4, 32));
      return s
    }
    
    def send_data(s:State):State = {
      uplout.data <= (s, new Value(0xDEADBEEF, 32));
      return s
    }

    (idle -> (m.expr(Op.==, trigger, Constant.HIGH), wait_trigger(sequencer.add()))
          -> (ack_ready, wait_ack_and_send_data(sequencer.add()))
          -> send_dest_addr(sequencer.add())
          -> send_port(sequencer.add())
          -> send_length(sequencer.add())
          -> send_data(sequencer.add()) -> idle)
          
    return m
  }
 
  def main(args:Array[String]) = {
    val m = gen_module()
    m.visualize_statemachine()
    m.visualize_resource()
    m.genVHDL()
    m.genVerilog()
  }

}
