//def f(x: Double) : Double = {
//  Range(2,10).foldLeft((1d,1d))((a,b) => (a._1+a._1*x/b,a._2+1))._1
//}

//def f(arg: Double) : Double = {
//  Range(1,10).foldLeft(1)((a : Double,b : Double) => a+(a*arg)/b)
//}

def f(x: Double) : String = {
  val result = Range(1,10).foldLeft((1d,1d,x))((a: (Double,Double,Double),b : Int) => (a._1+(a._2*a._3)/b.asInstanceOf[Double],(a._2*a._3)/b.asInstanceOf[Double],a._3))._1
  "%.4f".format(result)
}

//Sample Input
//
//4
//20.0000
//5.0000
//0.5000
//-0.5000
//Sample Output
//
//2423600.1887
//143.6895
//1.6487
//0.6065