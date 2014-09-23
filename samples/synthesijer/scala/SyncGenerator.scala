package synthesijer.scala

class SyncGenerator(n:String, c:String, r:String) extends Module(n, c, r){
  
  def this() = this("sync_generator", "clk", "reset")
  
  val VSYNC = outP("VSYNC")
  val HSYNC = outP("HSYNC")
  val DE = outP("DE")
  val DATA = outP("DATA", 24) // r, g, b
  val fifo_rd = outP("fifo_rd")
  val fifo_din = inP("fifo_din", 24)
  val wakeup = inP("wakeup")
  
  val H_DISP_WIDTH  = 1920
  val H_BACK_PORCH  = 148
  val H_FRONT_PORCH = 88
  val H_PULSE_WIDTH = 44
  val V_DISP_WIDTH  = 1080
  val V_BACK_PORCH  = 36
  val V_FRONT_PORCH = 4
  val V_PULSE_WIDTH = 5
  
  val h_p   = H_PULSE_WIDTH
  val h_pb  = h_p + H_BACK_PORCH
  val h_pbd = h_pb + H_DISP_WIDTH
  val h_all = H_DISP_WIDTH + H_BACK_PORCH + H_FRONT_PORCH + H_PULSE_WIDTH

  val v_p   = V_PULSE_WIDTH
  val v_pb  = v_p + V_BACK_PORCH
  val v_pbd = v_pb + V_DISP_WIDTH
  val v_all = V_DISP_WIDTH + V_BACK_PORCH + V_FRONT_PORCH + V_PULSE_WIDTH

  val hsync_counter = signal("hsync_counter", 32)
  val vsync_counter = signal("vsync_counter", 32)
  
  hsync_counter.reset(Constant.VECTOR_ZERO)
  
  val seq = sequencer("main")
  val s0 = seq.idle -> (wakeup, seq.add())
  
  hsync_counter <= (s0, expr(Op.IF, expr(Op.==, hsync_counter, h_all-1),
		                               Constant.VECTOR_ZERO,
                                   expr(Op.+, hsync_counter, 1)))
  
  val hsync0 = signal()
  val hsync1 = signal()
  val hsync2 = signal()
  
  hsync0 <= (s0, expr(Op.IF, expr(Op.==, hsync_counter, h_all-1), Constant.LOW,
                expr(Op.IF, expr(Op.==, hsync_counter, h_p-1), Constant.HIGH,
                hsync0)))
  hsync1 <= (s0, hsync0)
  hsync2 <= (s0, expr(Op.and, hsync1, expr(Op.not, hsync0)))
  
  vsync_counter <= (s0, expr(Op.IF, expr(Op.==, vsync_counter, v_all), Constant.VECTOR_ZERO,
                        expr(Op.IF, hsync2, expr(Op.+, vsync_counter, 1),
                        vsync_counter)))
                        
  val v_valid = signal()
  val h_valid = signal()
  
  v_valid <= (s0, expr(Op.IF, expr(Op.==, vsync_counter, v_pbd), Constant.LOW,
                  expr(Op.IF, expr(Op.==, vsync_counter, v_pb), Constant.HIGH,
                  v_valid)))
                
  h_valid <= (s0, expr(Op.IF, expr(Op.==, hsync_counter, h_pbd-1), Constant.LOW,
                  expr(Op.IF, expr(Op.==, hsync_counter, h_pb-1), Constant.HIGH,
                  h_valid)))
  
  val d_valid = signal()
  
  d_valid <= expr(Op.IF, expr(Op.==, v_valid, Constant.HIGH), h_valid, Constant.LOW)
                

  val vsync0 = signal()
  val vsync1 = signal()
  val vsync2 = signal()
  vsync0 <= (s0, expr(Op.IF, expr(Op.==, vsync_counter, v_all), Constant.LOW,
                 expr(Op.IF, expr(Op.==, vsync_counter, v_p), Constant.HIGH,
                 vsync0)))
  vsync1 <= (s0, vsync0)  
  vsync2 <= (s0, vsync1)  
  
  fifo_rd.reset(Constant.LOW)
  val data_en = expr(Op.and, expr(Op.==, hsync_counter, h_pb-1-2),
                expr(Op.and, expr(Op.geq, vsync_counter, v_pb),
                           expr(Op.<, vsync_counter, v_pbd)))
                           
  fifo_rd <= (s0, expr(Op.IF, expr(Op.==, hsync_counter, h_pbd-1-2), Constant.LOW,
                  expr(Op.IF, data_en, Constant.HIGH,
                  fifo_rd)))
  
  DATA <= (s0, fifo_din)
  VSYNC <= expr(Op.not, vsync2)
  HSYNC <= expr(Op.not, hsync0)
  DE <= d_valid

}

object SyncGenerator{
  
  def main(args:Array[String]) = {
    val m = new SyncGenerator()
    m.genVHDL()
    
    val sim = new SimModule("sync_generator_sim")
    val (clk, reset, counter) = sim.system(10)
    val instance = sim.instance(m, "U")
    instance.sysClk <= clk
    instance.sysReset <= reset
    instance.signalFor(m.wakeup) <= sim.expr(Op.IF, sim.expr(Op.>, counter, 20), Constant.HIGH, Constant.LOW)
    sim.genVHDL()
  }
  
}