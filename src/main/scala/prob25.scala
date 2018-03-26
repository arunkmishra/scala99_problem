/*scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)*/
import prob23.randomSelect

object prob25 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'b, 'c, 'd, 'e, 'f)

    println("Permutation : " + permute(ls))
  }

  def permute[A](ls: List[A]): List[A] = randomSelect(ls.size,ls)
}
