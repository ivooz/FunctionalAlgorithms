def h(n: Int, xs: List[Int], arr: List[Int]) : List[Int] = if(n==0) xs else if(n%2==1) h(n-1,arr.last :: xs,arr.dropRight(1)) else h(n-1,xs,arr.dropRight(1))

def f(arr:List[Int]):List[Int] = h(arr.length,List(),arr)