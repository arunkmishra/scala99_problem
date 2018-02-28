import scala.annotation.tailrec

/*
Find the number of elements of a list.
Example:
  scala> length(List(1, 1, 2, 3, 5, 8))
res0: Int = 6
*/


object prob04 {
  def main(args: Array[String]): Unit ={
    val li = List(1,2,3)

    println(length(li))

    println("length using fold : " + leng(li))
  }
  def length[A](li: List[A]): Int = {
    @tailrec
    def length[A](list: List[A],i: Int):Int= list match{
      case Nil => i
      case h:: tail => length(tail,i+1)
    }
    length(li, 0)
  }

  def leng(li : List[Int]): Int = li.foldLeft(0){(c,_)=> c+1}
}
