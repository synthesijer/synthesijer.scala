package synthesijer.scala

import synthesijer.hdl.sequencer.SequencerState
import synthesijer.hdl.HDLSequencer

class Sequencer(seq: HDLSequencer){
  
  var uid = 0;
  
	def tick(t: Integer) = seq.setTransitionTime(t);
	
	val idle = new State(seq.getIdleState())
	
	def add(name:String) : State = new State(seq.addSequencerState(name));
	
	def add() : State = {
	  val s = new State(seq.addSequencerState("S_" + uid))
    uid = uid + 1
    return s
	}
	  	
}

class State(val state: SequencerState){
  
	def -> (s:State) : State = {
	  state.addStateTransit(s.state)
	  return s
	}
	
  def -> (t:(ExprItem, State)) : State = {
    state.addStateTransit(t._1.toHDLExpr, t._2.state)
    return t._2
  }
	
  def max_delay(v:Int):Unit = {
	  state.setMaxConstantDelay(v)
	}

}
