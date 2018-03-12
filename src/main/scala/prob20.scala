import java.util.NoSuchElementException

/*scala> removeAt(1, List('a, 'b, 'c, 'd))
res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)*/

object prob20 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'b, 'c, 'd)

    println("removeAt : " + removeAt(2,ls))
    println("removeAt1 : " + removeAt1(2,ls))
    println("removeAt2 : " + removeAt2(2,ls))

  }

  def removeAt[A](n: Int, ls: List[A]): (List[A],A) = (ls.take(n):::ls.drop(n+1),ls(n))   //exceptions not handled

  def removeAt1[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post)  => (pre ::: post, e)
    case (pre, Nil)        => throw new NoSuchElementException
  }

  def removeAt2[A](n: Int, ls: List[A]): (List[A], A) =
    if (n < 0) throw new NoSuchElementException
    else (n, ls) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, h :: tail) => (tail, h)
      case (_, h :: tail) => {
        val (t, e) = removeAt1(n - 1, ls.tail)
        (ls.head :: t, e)
      }
    }
}
