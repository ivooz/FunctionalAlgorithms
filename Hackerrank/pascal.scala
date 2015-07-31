object Solution {
    
     def pascal(x:Int): Unit = {
        def recur(n : Int, xs: List[Int]) : Unit = {
          def makeRow(ls : List[Int]) : List[Int] = ls.foldLeft(0,List[Int]())((a,b) => (b,a._2:+a._1+b))._2 :+ 1
          n match {
            case 0 => return
            case _ => {
              println(xs.foldLeft("")((a : String,b) => a+" "+b).trim)
              recur(n-1,makeRow(xs))
            } 
          }
        }
        recur(x,List(1))
     }

    def main(args: Array[String]) {
         /** This will handle the input and output**/
         pascal(readLine().toInt)
    }
}
