

/**
 * Split a list into two parts.
 * @author izielinski
 */
object P17 {
  def apply[A](n: Int, xs : List[A]) : (List[A],List[A]) = n match {
    case 0 => xs
    case _ => xs.head :: apply(n-1, xs.slice(1,xs.length))
  }
}