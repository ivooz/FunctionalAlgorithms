

/**
 * Find the number of elements of a list.
 * @author izielinski
 */
object P04 {
  def apply[A](xs: List[A]): Int = xs match {
    case Nil => 0
    case _ => apply(xs tail) + 1
  }
}