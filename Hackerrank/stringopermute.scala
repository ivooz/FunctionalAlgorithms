object Solution {

    def alg(str : Array[Char]) = str.foldRight((List[Char](),0,'a'))((b,a)=>(if(a._2%2==1) (a._3 :: b :: Nil) ++ a._1 else a._1 ,a._2+1,b))._1
  
    def main(args: Array[String]) {
        Range(0,readInt()).foreach { x => println(alg(readLine() toCharArray) mkString) }
    }
}