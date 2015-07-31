

/**
 * Flatten a nested list structure.
 * @author izielinski
 */
object P07 {
  def apply(xs : List[Any]) : List[Any] = xs flatMap {
    case e : List[_] => apply(e)
    case e => List(e)
  }
}  