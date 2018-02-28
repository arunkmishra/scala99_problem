import scala.annotation.tailrec

/*
Reverse a list.
  Example:
  scala> reverse(List(1, 1, 2, 3, 5, 8))
res0: List[Int] = List(8, 5, 3, 2, 1, 1)
*/


object prob05 {
  def main(args: Array[String]): Unit ={
    val li= List(1,2,3,4)

    println("Reversed list : " + revers(li))
    println("tail rec reverse : "+ recReverse(li))
    println("fold reverse : "+ reverSe(li))
  }

  def revers[A](ls: List[A]): List[A] = {
    ls match{
      case Nil => ls
      case h :: tail => revers(tail) ::: List(h)
    }
  }


  def recReverse[A](ls : List[A]):List[A]= rev(ls,Nil)
@tailrec
  def rev[A](ls: List[A], res: List[A]): List[A] = ls match{
    case Nil => res
    case h:: tail =>rev(tail, h :: res)
  }

  def reverSe[A](li: List[A]):List[A] = li.foldLeft(List[A]()){(r,h) => h :: r}

}
/*
* Fold Worked As :
* r -> intial value
* h -> h is list which will traverse from head
* ()   1  2 3 4
*
* */