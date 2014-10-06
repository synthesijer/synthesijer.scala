package synthesijer.scala


object UPLTest {

  def gen_module() : Module = {
 
    val m = new Module("UPLTest")
    val uplin = new UPLIn(m, "pI0", 32)
    val uplout = new UPLOut(m, "pO0", 32)
 
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
    
    def wait_trigger():State = {
      val s = sequencer.add()
    	uplout.req <= (s, Constant.LOW)
    	uplout.en <= (s, Constant.LOW)
    	uplout.data <= (s, Constant.VECTOR_ZERO)
    	return s
    }

    val ack_ready = m.expr(Op.==, uplout.ack, Constant.HIGH)
    def wait_ack_and_send_data():State = {
      val s = sequencer.add()
    	uplout.data <= (s, ipaddr)
    	uplout.en <= (s, m.expr(Op.IF, ack_ready, Constant.HIGH, Constant.LOW))
    	uplout.req <= (s, m.expr(Op.IF, ack_ready, Constant.LOW, Constant.HIGH))
    	uplout.data <= (s, ipaddr)
    	return s
    }
    
    def send_dest_addr():State = {
      val s = sequencer.add()
    	uplout.data <= (s, server_addr)
      return s
    }

    def send_port():State = {
      val s = sequencer.add()
    	uplout.data <= (s, m.expr(Op.&, port, server_port))
      return s
    }
    
    def send_length():State = {
      val s = sequencer.add()
      uplout.data <= (s, new Value(4, 32));
      return s
    }
    
    def send_data():State = {
      val s = sequencer.add()
      uplout.data <= (s, new Value(0xDEADBEEF, 32));
      return s
    }

    (idle -> (m.expr(Op.==, trigger, Constant.HIGH), wait_trigger())
          -> (ack_ready, wait_ack_and_send_data())
          -> send_dest_addr()
          -> send_port()
          -> send_length()
          -> send_data() -> idle)
          
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
