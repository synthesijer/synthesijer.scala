package synthesijer.scala

import synthesijer.hdl.HDLExpr
import synthesijer.hdl.HDLModule
import synthesijer.hdl.HDLOp
import synthesijer.hdl.HDLPort
import synthesijer.hdl.HDLPrimitiveType
import synthesijer.hdl.HDLSequencer
import synthesijer.hdl.HDLSignal
import synthesijer.hdl.sequencer.SequencerState
import synthesijer.hdl.HDLUtils
import synthesijer.hdl.expr.HDLValue
import synthesijer.hdl.HDLInstance
import synthesijer.hdl.expr.HDLPreDefinedConstant

class Module(name:String, sysClkName:String, sysRsetName:String) extends HDLModule(name, sysClkName, sysRsetName){
  
	def this(name:String) = this(name, "clk", "reset")
	
  def genVHDL() = Utils.genVHDL(this)
  def genVerilog() = Utils.genVerilog(this)
  
  def outP(name:String) : Port = new Port(newPort(name, HDLPort.DIR.OUT, HDLPrimitiveType.genBitType()))
  def inP(name:String) : Port = new Port(newPort(name, HDLPort.DIR.IN, HDLPrimitiveType.genBitType()))
  
	def signal(name:String, width:Integer) : Signal = new Signal(newSignal(name, HDLPrimitiveType.genSignedType(width)))
	
	def signal(name:String) : Signal = new Signal(newSignal(name, HDLPrimitiveType.genBitType()));
  
  def expr(op:HDLOp, e0:Any, e1:Any, e2:Any) : Expr = new Expr(newExpr(op, Utils.toExpr(e0), Utils.toExpr(e1), Utils.toExpr(e2)))
  def expr(op:HDLOp, e0:Any, e1:Any) : Expr = new Expr(newExpr(op, Utils.toExpr(e0), Utils.toExpr(e1)));
  
  def sequencer(name:String) : Sequencer = new Sequencer(newSequencer(name))
  
  def instance(target:Module, name:String) : Instance = new Instance(newModuleInstance(target, name))

}

class Instance(target:HDLInstance) {
  
	val sysClk = new Signal(target.getSignalForPort(target.getSubModule().getSysClkName()))
  val sysReset = new Signal(target.getSignalForPort(target.getSubModule().getSysResetName()))
	
  def signalFor(name:String) = target.getSignalForPort(name)
  
}

class SimModule(name:String) extends Module(name:String){

  
}

class Port(p:HDLPort) {
  
	def <= (e:Expr):Unit = p.getSignal().setAssign(null, e.expr)
  
}

class Signal(val signal:HDLSignal){
	
	def <= (e:Expr) : Unit = signal.setAssign(null, e.expr)
	def <= (e:HDLExpr) : Unit = signal.setAssign(null, e)
	def <= (s:Signal) : Unit = signal.setAssign(null, s.signal)
	
	def <= (t:(Any, Any)) : Unit = signal.setAssign(Utils.toState(t._1), Utils.toExpr(t._2))

}

class Expr(val expr:HDLExpr){
  
}

class Sequencer(seq: HDLSequencer){
  
	def tick(t: Integer) = seq.setTransitionTime(t);
	
	val idle = new State(seq.getIdleState())
	
	def add(name:String) : State = new State(seq.addSequencerState(name));
	  
}

class State(val state: SequencerState){
  
	def -> (s:State) : Unit = state.addStateTransit(s.state)
	
}

object Utils {
  
  def genVHDL(m:Module) = HDLUtils.generate(m, HDLUtils.VHDL);
  
  def genVerilog(m:Module) = HDLUtils.generate(m, HDLUtils.VHDL);

	
  def toExpr(o:Any) : HDLExpr = o match {
	  case e:HDLExpr => return e
	  case e:Expr => return e.expr
	  case e:Signal => return e.signal
	  case e:Integer => new HDLValue(String.valueOf(e), HDLPrimitiveType.genIntegerType())
	}  

	def toState(o:Any) : SequencerState = o match {
	  case s:SequencerState => return s
	  case s:State => return s.state
	}
	
	def toSignal(o:Any) : HDLSignal = o match {
	  case s:HDLSignal => return s
	  case s:Signal => return s.signal
	}  

}

object Constant{
  
	val VECTOR_ZERO = HDLPreDefinedConstant.VECTOR_ZERO
	val ZERO = HDLPreDefinedConstant.INTEGER_ZERO
	val ONE = HDLPreDefinedConstant.INTEGER_ONE
	val TRUE = HDLPreDefinedConstant.BOOLEAN_TRUE
	val FALSE = HDLPreDefinedConstant.BOOLEAN_FALSE
	val LOW = HDLPreDefinedConstant.LOW
	val HIGH = HDLPreDefinedConstant.HIGH

}

object Op{
	val + = HDLOp.ADD
	val - = HDLOp.SUB
	val and = HDLOp.AND
	val or = HDLOp.OR
	val xor = HDLOp.XOR
	val not = HDLOp.NOT
	val == = HDLOp.EQ
	val < = HDLOp.LT
	val > = HDLOp.GT
	val <= = HDLOp.LEQ
	val >= = HDLOp.GEQ
	val /= = HDLOp.NEQ
	val REF = HDLOp.REF
	val IF = HDLOp.IF
	val & = HDLOp.CONCAT
	val drop = HDLOp.DROPHEAD
	val padding = HDLOp.PADDINGHEAD
	val padding0 = HDLOp.PADDINGHEAD_ZERO
	val id = HDLOp.ID
	val >>> = HDLOp.ARITH_RSHIFT // TODO check
	val >> = HDLOp.LOGIC_RSHIFT
	val << = HDLOp.LSHIFT
}