

/**
 * Eliminate consecutive duplicates of list elements.
 * @author izielinski
 */
object P08 {
  def apply[A](xs : List[A]) : List[A]  = xs match {
    case Nil => Nil
    case a :: Nil => List(a)
    case a :: b :: c => if(a equals b) b :: apply(c) else a :: b :: apply(c)
  }
  
  def alt[A](xs : List[A]) : List[A]  = xs.foldLeft(List[A]()){ (a : List[A], b : A) =>
    if(a isEmpty) {List(b)}
    else if(a.last!=b) a :+ b
    a
  }
}