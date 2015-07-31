

/**
 * Find out whether a list is a palindrome.
 * @author izielinski
 */
object P06 {
  def apply[A](xs: List[A]): Boolean = xs match {
    case h :: Nil => true
    case h :: tail if (h == tail.lastOption.get) => apply(tail slice(0,tail.length-1)) 
    case h => println(h); false
  }
} 