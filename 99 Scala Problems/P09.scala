

/**
 * P09/13 If a list contains repeated elements they should be placed in separate sublists.
 * P10 Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
 * P11 Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
 * P12 Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
 * @author izielinski
 */
object P09 {
  def apply[A](xs : List[A]) : List[List[A]] = xs.foldLeft(List[List[A]]()){(a,b) => 
    a match {
      case Nil => List(List(b))
      case ls => if(b == (ls.last(0))) ls.slice(0,ls.length-1) :+ (ls.last :+ b) else ls :+ List(b)
    }
  }
  
  def p10[A](xs : List[List[A]]) : List[(Int,A)] = xs.foldLeft(List[(Int,A)]()){(a,b) => 
    a :+ (b.length, b(0))
  }
  
  def p11[A](xs : List[List[A]]) : List[Any] = xs.foldLeft(List[Any]()){(a,b) => 
    b match {
      case e :: Nil => a :+ e
      case b => a :+ (b.length, b(0))
    }
  }
  
  def p12[A](xs : List[(Int,A)]) : List[A] = xs.foldLeft(List[A]()) ((a : List[A], b : (Int,A) ) => a ++ List.fill(b._1)(b._2))
}