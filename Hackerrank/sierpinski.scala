object Solution {
  
  class V(xs: Int, ys: Int) {
    def x: Int = xs
    def y: Int= ys
  }
  
  def workaround(x :Int, y : Int) = y == 31 -x
  
  class T(lf : V, r : V, t : V) {
    def lft : V = lf
    def rgt : V = r
    def top : V = t
    def isWithin(x: Int, y: Int) : Boolean = {
      if(workaround(x,y)) return true
      if(x > rgt.x || x < top.x || y <= lft.y || y >= rgt.y) return false
      if(y == top.y) return true
      if(y<top.y) return x > lft.x - (y - lft.y)
      if(y>top.y) return x > lft.x - (rgt.y -y)
      return false
    }
  }
  
  def recurVer(n : Int, tr : T) : List[T] = n match {
    case 0 => List(tr)
    case _ => recurVer(n-1, new T(new V((tr.lft.x+tr.top.x)/2,(tr.lft.y+tr.top.y)/2),
                                    new V((tr.rgt.x+tr.top.x)/2,(tr.rgt.y+tr.top.y)/2),
                                    tr.top)) ++
              recurVer(n-1, new T(tr.lft,
                                    new V((tr.lft.x+tr.rgt.x)/2,(tr.lft.y+tr.rgt.y)/2),
                                    new V((tr.lft.x+tr.top.x)/2+1,(tr.lft.y+tr.top.y)/2))) ++
              recurVer(n-1, new T(new V((tr.lft.x+tr.rgt.x)/2,(tr.lft.y+tr.rgt.y)/2),
                                    tr.rgt,
                                    new V((tr.top.x+tr.rgt.x)/2+1,(tr.top.y+tr.rgt.y)/2)))
  }
  
  def drawTriangles(n: Int) {
    Range(0,32).to[List].map(x => Range(0,63).to[List].map(y => y match {
      case 31 => '1'
      case y => {
        if(y<31) if ((30-x) < y) '1' else '_' 
        else if (x < y -31) '_' else '1' 
      }
    })).foldLeft(List(List[Char]()),0,recurVer(n,new T(new V(31,0),new V(31,63),new V(0,31))))((acc,ls) => (
        ls.foldLeft(List[Char]('\n'),acc._2,0,acc._3)((acc2,e) =>
          ((if(acc2._4 exists(_.isWithin(acc2._2,acc2._3))) '1' else '_') :: 
              acc2._1,acc2._2,acc2._3+1,acc2._4 ))._1 :: acc._1,acc._2+1,acc._3))._1.reverse.foreach { x => x.foreach(y => print(y))}
  }

  def main(args: Array[String]) {
    drawTriangles(readInt())
  }
}

//scala -Dscala.repl.maxprintstring=64000
