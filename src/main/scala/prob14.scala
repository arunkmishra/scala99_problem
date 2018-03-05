/*scala> duplicate(List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)*/

object prob14 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'b, 'c, 'c, 'd)

    println("Duplicate : " + duplicate(ls))

    println("Duplicate2 : " + duplicate2(ls))
  }
  def duplicate[A](ls: List[A]): List[A] = ls.flatMap{ e => List.fill(2)(e)}

  def duplicate2[S](ls: List[S]) = ls flatMap(e => List(e, e))
}
