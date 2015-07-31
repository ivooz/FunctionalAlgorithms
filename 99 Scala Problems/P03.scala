

/**
 * Find the Kth element of a list.
 * @author izielinski
 */
object P03 {
  def solve[A](n: Int, xs: List[A]): A = n match {
    case 0 => xs head
    case _ => solve(n - 1,xs tail)
  }
}