import scala.annotation.tailrec

/*scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)*/


object prob21 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'b, 'c, 'd)

    println("InsertAt : " + insertAt('new, 1, ls))

    println("InsertAt tail rec : " + insertAtRec('new, -4, ls))

    println("InsertAt Split : " + insertAtSplit('new, 1, ls))
  }

  def insertAt[T](ele: T, pos: Int, ls: List[T]): List[T] = ls.take(pos) ::: List(ele) ::: ls.drop(pos)

  def insertAtRec[A](ele: A, n: Int, ls: List[A]): List[A] = {
    @tailrec
    def insertAtTail(n:Int, res: List[A], li: List[A]): List[A] = (n,li) match {
      case (i, Nil) if (i>0) || (i< -1) => res
      case (-1,lis) => res ::: lis
      case (0,lis) => insertAtTail(-1,res:+ele,lis)
      case (i,h::tail) => insertAtTail(i-1,res:+h,tail)
    }
    insertAtTail(n, List[A](),ls)
  }

  def insertAtSplit[A](ele: A, n: Int, ls: List[A]):List[A] = {
    val (pre, post) = ls.splitAt(n)
    pre ::: List(ele) ::: post
  }
}
