// P01 (*) Find the last element of a list.
//     Example:
//     scala> last(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 8

// The start of the definition of last should be
//     def last[A](l: List[A]): A = ...
// The `[A]` allows us to handle lists of any type.
/*
object P01 {
  // There are several ways to solve this problem.  If we use builtins, it's very
  // easy.
  def lastBuiltin[A](ls: List[A]): A = ls.last

  // The standard functional approach is to recurse down the list until we hit
  // the end.  Scala's pattern matching makes this easy.
  def lastRecursive[A](ls: List[A]): A = ls match {
    case h :: Nil  => h
    case _ :: tail => lastRecursive(tail)
    case _         => throw new NoSuchElementException
  }
}*/


object prob1 {

  def main(args: Array[String]): Unit = {
    val li = List()
    val lis = List('a','b','c','d','f')
    println(last(li))
    println(last(lis))

  }
  def last[A](list: List[A]):A =list match{
    case h::Nil => h
    case h::tail => last(tail)
    case _ => throw new NoSuchElementException
  }
}
