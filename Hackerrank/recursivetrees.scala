object Solution {

  //There are 63 rows and 100 columns
  //18+2^(5-x)
  //18
  //19
  //21
  //25
  //32
    
    type Tree = (Int,Int,Int)
  
    def alg(n : Int) : Unit = {
      def recur(n : Int, t : Tree) : List[Tree] = n match {
        case 0 => t :: Nil
        case _ => t :: recur(n-1,(t._1+t._3,t._2+2*t._3,t._3/2)) ++ recur(n-1,(t._1-t._3,t._2+2*t._3,t._3/2))
      }
      recur(n).foldLeft((List.fill(63)(List.fill(100)('_')),recur(n,(0,31,16)),0))((a,b) => 
        (a.foldLeft(List[List[Char]](),0,)((a2,b2) => 
          (b2.foldLeft(k),,)),a._2,a._3+1)
      
    }
  
    def main(args: Array[String]) {
        alg(readInt())
    }
}