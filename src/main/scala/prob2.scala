import scala.annotation.tailrec

/*
Find the last but one element of a list.
  Example:
  scala> penultimate(List(1, 1, 2, 3, 5, 8))
res0: Int = 5
*/

object prob2 {

  def main(args: Array[String]): Unit = {
    val ls = List(1,2,3,5)
    val li = List("sd","sadasd","www","eee")

    println(lastSecond(li))
  }
  @tailrec
  def lastSecond[A](ls: List[A]):A=ls match{
    case h::_::Nil => h
    case _ :: h :: tail => lastSecond(h::tail)
    case _ => throw new NoSuchElementException
  }
}
