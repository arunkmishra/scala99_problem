import scala.annotation.tailrec

/*scala> range(4, 9)
res0: List[Int] = List(4, 5, 6, 7, 8, 9)*/

object prob22 {
  def main(args: Array[String]): Unit = {

    println("range builtin : " + rangeBuiltIn(4,9))

    println("range tail rec : "+ rangeTailRec(4,9))

    println("range UnfoldRight : " + rangeUnFold(4,9))
  }

  def rangeBuiltIn(i: Int, n: Int) : List[Int] = List.range(i,n+1)

  def rangeTailRec(i: Int, n: Int): List[Int] = {
    @tailrec
    def rangeTail(res: List[Int], e: Int): List[Int] = e match {
      case (r) if i > r => res
      case (l) => rangeTail(e :: res, l-1)
    }
    rangeTail(List[Int](),n)
  }

  def unFoldRight[A,B](s: B)(f : B => Option[(A,B)]): List[A] = f(s) match {
    case None => Nil
    case Some((r,l)) => r :: unFoldRight(l)(f)
  }

  def rangeUnFold(i: Int,n : Int):List[Int] = unFoldRight(i){ b =>
    if(b > n) None
    else Some(b, b+1)
  }
}
