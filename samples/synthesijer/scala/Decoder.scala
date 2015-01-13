package synthesijer.scala

class Test(n:String, c:String, r:String) extends Module(n, c, r){
  
  def decoder(s:ExprItem, l:List[(Int,Int)], w:Int) = {
	  l.foldRight(value(0,w)){
		  (a, z) => ?(s == a._1, value(a._2, w), z)
	  }
  }
  
  
  
}