/*scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)*/

object prob12 {
  def main(args: Array[String]): Unit = {
    val ls = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))

    println("Decoded list By me : "+decodeByMe(ls))
    println("Decoded List using Flatmap : "+ decode(ls))
  }

  def decodeByMe[A](ls: List[(Int, A)]): List[A] ={
    def decodeTail(res: List[A], lis: List[(Int,A)]): List[A] = lis match {
      case Nil => res
      case (l,e) :: tail => decodeTail(res ::: List.fill(l)(e), tail)
    }
    decodeTail(List[A](),ls)
  }
  def decode[A](ls: List[(Int, A)]): List[A] =ls flatMap{case (c,e)=> List.fill(c)(e)}
}
