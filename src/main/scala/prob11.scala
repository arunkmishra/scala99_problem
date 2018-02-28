import scala.annotation.tailrec
import prob10.encodeByMe

/*
encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
*/


object prob11 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

    println("encoded List : " + encodeNewByMe(ls))
    println("encoded list using map : " + encodeMap(ls))
    println("encode using either : "+ encodeModified2(ls))
  }

  def encodeNewByMe[A](ls: List[A]): List[Any] = {

    val lis = encodeByMe(ls)

    @tailrec
    def encodeNewTail(res: List[Any],ls: List[(Int,A)]): List[Any] = ls match {
      case Nil => res
      case (1,e)::tail => encodeNewTail(e :: res,tail)
      case h :: tail => encodeNewTail(h :: res,tail)
    }
    encodeNewTail(List[Any](),lis).foldLeft(List[Any]()){(h,r) => r ::h}
  }
  def encodeMap[A](ls: List[A]): List[Any] = {
    val lis = encodeByMe(ls)        //encodeByMe is a function from prob 10

    lis.map(e => if(e._1 == 1) e._2 else e)
  }
  def encodeModified2[A](ls: List[A]): List[Either[A, (Int, A)]] =
    encodeByMe(ls) map { t => if (t._1 == 1) Left(t._2) else Right(t) }
}
