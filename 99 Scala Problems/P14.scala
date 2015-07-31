

/**
 * Duplicate the elements of a list.
 * @author izielinski
 */
object P14 {
  def apply(xs : List[Any]) : List[Any] = xs.foldLeft(List[Any]())((a : List[Any],b) => {
    a ++ List.fill(2)(b)
  })
}