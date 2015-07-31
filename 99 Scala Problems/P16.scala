

/**
 * Drop every Nth element from a list.
 * @author izielinski
 */
object P16 {
  var nth = 0; 
  private def recur[A](counter : Int, xs : List[A]) : List[A] = {
    xs match {
      case Nil => Nil
      case ls =>if(counter % nth == 0) {
                  recur(counter +1, ls.slice(1, ls.length))
                } else {
                  ls.head :: recur(counter +1, ls.slice(1, ls.length))
                }
    }
  }
  def apply[A](n: Int, xs : List[A]) : List[A] = {
    nth = n
    recur(1, xs)
  }
}