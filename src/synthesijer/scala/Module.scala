package synthesijer.scala

import java.io.FileOutputStream
import java.io.PrintWriter
import synthesijer.hdl.HDLExpr
import synthesijer.hdl.HDLInstance
import synthesijer.hdl.HDLModule
import synthesijer.hdl.HDLOp
import synthesijer.hdl.HDLPort
import synthesijer.hdl.HDLPrimitiveType
import synthesijer.hdl.HDLSignal
import synthesijer.hdl.HDLUtils
import synthesijer.hdl.expr.HDLPreDefinedConstant
import synthesijer.hdl.expr.HDLValue
import synthesijer.hdl.tools.HDLSequencerToDot
import synthesijer.hdl.tools.ResourceUsageTable

class Module(name:String, sysClkName:String, sysRsetName:String) extends HDLModule(name, sysClkName, sysRsetName){
  
  var id = 0
  
	def this(name:String) = this(name, "clk", "reset")
	
  def genVHDL() = Utils.genVHDL(this)
  def genVerilog() = Utils.genVerilog(this)
  
  def outP(name:String) : Port = new Port(newPort(name, HDLPort.DIR.OUT, HDLPrimitiveType.genBitType()))
  def outP(name:String, width:Int) : Port = new Port(newPort(name, HDLPort.DIR.OUT, HDLPrimitiveType.genVectorType(width)))
	
  def inP(name:String) : Port = new Port(newPort(name, HDLPort.DIR.IN, HDLPrimitiveType.genBitType()))
  def inP(name:String, width:Int) : Port = new Port(newPort(name, HDLPort.DIR.IN, HDLPrimitiveType.genVectorType(width)))
  
	def signal(name:String, width:Integer) : Signal = new Signal(newSignal(name, HDLPrimitiveType.genSignedType(width)))
  def signal(width:Integer) : Signal = {
	  val sig = new Signal(newSignal("synthesier_scala_tmp_" + id, HDLPrimitiveType.genSignedType(width)))
	  id = id + 1
	  return sig
  }
	
	def signal(name:String) : Signal = new Signal(newSignal(name, HDLPrimitiveType.genBitType()))
	
	def signal() : Signal = {
	  val sig = new Signal(newSignal("synthesier_scala_tmp_" + id, HDLPrimitiveType.genBitType()))
	  id = id + 1
	  return sig
	}
  
  def expr(op:HDLOp, e0:ExprItem, e1:ExprItem, e2:ExprItem) : Expr = new Expr(newExpr(op, e0.toHDLExpr(), e1.toHDLExpr(), e2.toHDLExpr()))
  def expr(op:HDLOp, e0:ExprItem, e1:ExprItem) : Expr = new Expr(newExpr(op, e0.toHDLExpr(), e1.toHDLExpr()))
  def expr(op:HDLOp, e0:ExprItem) : Expr = new Expr(newExpr(op, e0.toHDLExpr()))
  
  def expr(op:HDLOp, e0:ExprItem, e1:ExprItem, e2:Int) : Expr = new Expr(newExpr(op, e0.toHDLExpr(), e1.toHDLExpr(), Utils.toHDLValue(e2)));
  def expr(op:HDLOp, e0:ExprItem, e1:Int) : Expr = new Expr(newExpr(op, e0.toHDLExpr(), Utils.toHDLValue(e1)));
  
  def sequencer(name:String) : Sequencer = new Sequencer(newSequencer(name))
  
  def instance(target:Module, name:String) : Instance = new Instance(newModuleInstance(target, name))
  
  def visualize_statemachine() : Unit =  HDLUtils.genHDLSequencerDump(this)

  def visualize_resource() : Unit = HDLUtils.genResourceUsageTable(this)

}

class Instance(target:HDLInstance) {
  
	val sysClk = new Signal(target.getSignalForPort(target.getSubModule().getSysClkName()))
  val sysReset = new Signal(target.getSignalForPort(target.getSubModule().getSysResetName()))
	
  def signalFor(name:String) = target.getSignalForPort(name)
  
}

class SimModule(name:String) extends Module(name:String){
  
}

class Port(val port:HDLPort) extends ExprItem{
  
	def <= (e:ExprItem):Unit = port.getSignal().setAssign(null, e.toHDLExpr)
	
	def <= (t:(State, ExprItem)) : Unit = port.getSignal().setAssign(t._1.state, t._2.toHDLExpr)
	
	def <= (t:(State, Int, ExprItem)) : Unit = port.getSignal().setAssign(t._1.state, t._2, t._3.toHDLExpr)
	
	def reset(e:ExprItem): Unit = port.getSignal().setResetValue(e.toHDLExpr)
	
  def toHDLExpr() = port.getSignal()
  
}

trait ExprItem {
  
	def toHDLExpr() : HDLExpr
  
}

class Signal(val signal:HDLSignal) extends ExprItem{
	
	def <= (e:ExprItem) : Unit = signal.setAssign(null, e.toHDLExpr)
	
	def <= (t:(State, ExprItem)) : Unit = signal.setAssign(t._1.state, t._2.toHDLExpr)

	def reset(e:ExprItem): Unit = signal.setResetValue(e.toHDLExpr)
	
	def toHDLExpr() = signal

}

class Expr(val expr:HDLExpr) extends ExprItem{
  
	def toHDLExpr() = expr

}

class Value(n:Int, width:Int) extends ExprItem{
	val value = new HDLValue(n.toString(), HDLPrimitiveType.genVectorType(width))
	
	def toHDLExpr() = value
}


object Utils {
  
  def genVHDL(m:Module) = HDLUtils.generate(m, HDLUtils.VHDL);
  
  def genVerilog(m:Module) = HDLUtils.generate(m, HDLUtils.VHDL);
  
  def toHDLValue(num:Int) = new HDLValue(num.toString, HDLPrimitiveType.genIntegerType())

}

sealed abstract class Constant(val v:HDLPreDefinedConstant) extends ExprItem{
    val name = toString
    def toHDLExpr() = v
}

object Constant{
  
  case object VECTOR_ZERO extends Constant(HDLPreDefinedConstant.VECTOR_ZERO)
  case object ZERO extends Constant(HDLPreDefinedConstant.INTEGER_ZERO)
  case object ONE extends Constant(HDLPreDefinedConstant.INTEGER_ONE)
  case object TRUE extends Constant(HDLPreDefinedConstant.BOOLEAN_TRUE)
  case object FALSE extends Constant(HDLPreDefinedConstant.BOOLEAN_FALSE)
  case object LOW extends Constant(HDLPreDefinedConstant.LOW)
  case object HIGH extends Constant(HDLPreDefinedConstant.HIGH)

}

object Op{
	val + = HDLOp.ADD
	val add = HDLOp.ADD
	val - = HDLOp.SUB
	val sub = HDLOp.SUB
	val and = HDLOp.AND
	val or = HDLOp.OR
	val xor = HDLOp.XOR
	val not = HDLOp.NOT
	val == = HDLOp.EQ
	val eq = HDLOp.EQ
	val < = HDLOp.LT
	val lt = HDLOp.LT
	val > = HDLOp.GT
	val gt = HDLOp.GT
	val <= = HDLOp.LEQ
	val leq = HDLOp.LEQ
	val >= = HDLOp.GEQ
	val geq = HDLOp.GEQ
	val /= = HDLOp.NEQ
	val neq = HDLOp.NEQ
	val REF = HDLOp.REF
	val IF = HDLOp.IF
	val & = HDLOp.CONCAT
	val concat = HDLOp.CONCAT
	val drop = HDLOp.DROPHEAD
	val padding = HDLOp.PADDINGHEAD
	val padding0 = HDLOp.PADDINGHEAD_ZERO
	val id = HDLOp.ID
	val >> = HDLOp.ARITH_RSHIFT // TODO check
	val >>> = HDLOp.LOGIC_RSHIFT
	val << = HDLOp.LSHIFT
}

