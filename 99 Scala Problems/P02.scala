

/**
 * Find the last but one element of a list.
 * @author izielinski
 */
object P02 {
  def apply[A](xs: List[A]): A = xs match {
    case h :: z :: Nil => h
    case _ :: tail => apply(tail)
  }
}