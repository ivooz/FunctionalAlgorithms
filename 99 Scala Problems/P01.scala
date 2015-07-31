

/**
 * Find the last element of a list.
 * @author izielinski
 */
object P01 {
  def apply[A](xs: List[A]): A = xs match {
    case h :: Nil => h
    case _ :: tail => apply(tail)
  }
}