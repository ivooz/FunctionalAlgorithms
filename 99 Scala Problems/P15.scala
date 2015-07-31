

/**
 * Duplicate the elements of a list a given number of times.
 * @author izielinski
 */
object P15 {
  def apply(n: Int, xs : List[Any]) : List[Any] = xs.foldLeft(List[Any]())((a : List[Any],b) => {
    a ++ List.fill(n)(b)
  })
}