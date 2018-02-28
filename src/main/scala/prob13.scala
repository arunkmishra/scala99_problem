/*scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))*/

object prob13 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

    println("encode by me : "+ encodeByMe(ls))
  }

  def encodeByMe[A](ls: List[A]):List[(Int,A)]={
    if (ls.isEmpty)
      Nil
    else {
      val (packed, next) = ls.span(_ == ls.head)
      (packed.length,packed.head)::encodeByMe(next)
    }
  }
}
