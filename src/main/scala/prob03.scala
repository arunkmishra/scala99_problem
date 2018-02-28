/*Find the Kth element of a list.
  By convention, the first element in the list is element 0.
Example:

  scala> nth(2, List(1, 1, 2, 3, 5, 8))
res0: Int = 2*/

object prob03 {
  def main(args: Array[String]): Unit = {
    val li = List(1,2,3,4,5,6)

    println(lastNthElement(li,2))
    println(nthElement(li,-1))
  }
  def lastNthElement[A](list: List[A],n : Int):A=
    lastNthElement(list,n,list.size)

  def lastNthElement[A](li: List[A], n: Int, i: Int):A= li match{
    case h :: tail if n==i => h
    case _ :: tail if n<i => lastNthElement(tail,n,i-1)
    case li if n>i => throw new NoSuchElementException
  }


  implicit def toStr[A](i : A): String = i.toString

  def nthElement[A](li: List[A], i: Int):String = (i , li) match{
    case (0, h::_) => h
    case (_, _:: tail) => nthElement(tail, i-1)
    case (_, _) => 0
  }
}
