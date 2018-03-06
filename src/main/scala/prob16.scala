import scala.annotation.tailrec

/*scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)*/

object prob16 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

    println("Dropped list(Zip) : "+ dropZip(3,ls))

    println("Dropped list(tail rec) : "+ drop(3,ls))

    println("Dropped list(Zip and FlatMap) : "+ dropMap(3,ls))

    println("Dropped List (take rec) : " + dropTake(3,ls))
  }
//METHOD 1
  def drop[A](n: Int, ls: List[A]): List[A] = {
    @tailrec
    def dropTail(res: List[A], li: List[A], i: Int): List[A] = li match{
      case Nil => res
      case h :: tail if i==n => dropTail(res, tail, 1)
      case h :: tail => dropTail(res :+ h, tail, i+1)
    }
    dropTail(List[A](),ls,1)
  }
//METHOD 2
  def dropZip[A](n: Int,ls: List[A]) = ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }

  //METHOD 3
  def dropMap[A](i: Int, ls: List[A]): List[A] =
    ls.zipWithIndex.flatMap(t => if((t._2+1)%i == 0) List[A]() else List(t._1))

  //METHOD 4
  def dropTake[A](i : Int, ls: List[A]): List[A] =
    if(ls.lengthCompare(i)<0)
      ls
    else
      ls.take(i-1) ::: dropTake(i, ls.drop(i))
}
