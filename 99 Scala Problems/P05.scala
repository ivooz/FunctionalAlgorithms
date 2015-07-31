

/**
 * Reverse a list.
 * @author izielinski
 */
object P05 {
  def apply[A](xs: List[A]): List[A] = xs match {
    case h :: Nil => List(h)
    case h :: tail => apply(xs tail) :+ h
  }
}