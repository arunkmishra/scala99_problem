/*encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))*/

import prob9.pack

import scala.annotation.tailrec

object prob10 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

    println("encode by me : "+ encodeByMe(ls))

    println("encoding     : "+ encodeMap(ls) )
  }
  def encodeByMe[A](ls: List[A]): List[(Int,A)]={
    val lis = pack(ls)          //here pack is a function from prob9.scala
    @tailrec
    def enTail(res: List[(Int,A)],lis: List[List[A]]): List[(Int,A)]= lis match{
      case Nil => res
      case h :: tail => enTail((h.length,h.head)::res,tail)
    }
    enTail(List[(Int,A)](),lis).foldLeft(List[(Int,A)]()){(h,r) => r :: h}
  }

  def encodeMap[A](ls: List[A]): List[(Int,A)] = pack(ls).map(l => (l.length,l.head))
}
