object Solution {

    def alg(xs : Array[Char], ys : Array[Char]) : List[Char] = {
      Range(0,xs.length).foldRight(List[Char]())((a,b) => (xs(a) :: ys(a) :: Nil) ++ b)
    }
    
    def main(args: Array[String]) {
      println(alg(readLine() toCharArray,readLine() toCharArray) mkString)
    }
}