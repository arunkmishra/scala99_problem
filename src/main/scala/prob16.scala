import scala.annotation.tailrec

/*scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)*/

object prob16 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

    println("Dropped list(Zip) : "+ dropZip(3,ls))

    println("Dropped list(tail rec) : "+ drop(3,ls))
  }

  def drop[A](n: Int, ls: List[A]): List[A] = {
    @tailrec
    def dropTail(res: List[A], li: List[A], i: Int): List[A] = li match{
      case Nil => res
      case h :: tail if i==n => dropTail(res, tail, 1)
      case h :: tail => dropTail(res :+ h, tail, i+1)
    }
    dropTail(List[A](),ls,1)
  }

  def dropZip[A](n: Int,ls: List[A]) = ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }
}
