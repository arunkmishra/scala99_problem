import scala.annotation.tailrec

/*scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))*/

object prob17 {
  def main(args: Array[String]): Unit = {
    val ls = List()

    println("Split using inbuilt : "+ splitInBuilt(5,ls))

    println("Split using rec call : "+ splitRec(5,ls))

    println("Split using Tailrec call : "+ splitTailRec(5,ls))

    println("Split using Fxn : " + splitDrop(3,ls))
  }

  def splitInBuilt[A](i: Int, ls: List[A]): (List[A],List[A]) = ls.splitAt(i)

  //METHOD 2
  def splitRec[A](n: Int, ls: List[A]): (List[A], List[A]) = (n,ls) match {
    case (_,Nil) => (Nil,Nil)
    case (0,list) => (Nil,list)
    case (i,h :: tail) => val (first,second) = splitRec(i-1,tail)
      (h :: first, second)
  }

  def splitTailRec[A](n: Int, ls: List[A]): (List[A], List[A]) ={
    @tailrec
    def splitTail(i: Int, first: List[A], second: List[A]): (List[A], List[A]) = (i,second) match {
      case (_, Nil) => (first, Nil)
      case (0, list) => (first, list)
      case (c, h::tail) => splitTail(c-1,first:+h , tail)
    }
    splitTail(n,List[A](),ls)
  }

  def splitDrop[A](n:Int,ls: List[A]): (List[A],List[A]) = (ls.take(n), ls.drop(n))
}
