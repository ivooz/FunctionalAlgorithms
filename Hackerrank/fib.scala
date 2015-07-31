object Solution {
  
    //1,2,3,5,8
     def fibonacci(x:Int):Int = if(x<4) 1 else fibonacci(x-1) + fibonacci(x-2)
     

    def main(args: Array[String]) {
         /** This will handle the input and output**/
         println(fibonacci(readInt()))

    }
}
