/*Flatten a nested list structure.
  Example:
  scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
res0: List[Any] = List(1, 1, 2, 3, 5, 8)*/

object prob07 {

  def main(args: Array[String])={
    val ls = List(List(1, 1), 2, List(3, List(5, 8)))
    println("flattened : " + flatten(ls))
  }

  def flatten(ls: List[Any]): List[Any] = ls flatten{
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

}