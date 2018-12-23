/*scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...*/
object CombinationOps {
  implicit class CombinatorialList[A](l: List[A]) {

    def xcombinations(n: Int): List[List[A]] =
      if(n> l.size)
        Nil
      else l match {
        case _::_ if n == 1 => l.map(List(_))
        case h :: t => t.xcombinations(n-1).map(h :: _) ::: xcombinations(n)
        case _ => Nil
      }
  }
}
object prob26 {
  def main(args: Array[String]): Unit = {
    val ls = List(1,2,3)
    val obj = new CombinationOps.CombinatorialList(ls)

    println("combination patter : \n " + combinations(2,ls))
    println("combination patter : \n " + obj.xcombinations(2))
  }

  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {sl.head :: _}
    }
}
