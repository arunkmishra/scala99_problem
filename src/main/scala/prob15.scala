/*scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)*/

object prob15 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'b, 'c, 'c, 'd)

    println("Duplicate : "+ duplicate(4,ls))
  }

  def duplicate[A](n: Int, ls: List[A]): List[A] = ls flatMap(e => List.fill(n)(e))
}
