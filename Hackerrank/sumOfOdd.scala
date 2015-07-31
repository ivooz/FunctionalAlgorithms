import scala.math.abs

def f(arr:List[Int]):Int = arr.filter(abs(_)%2==1).sum