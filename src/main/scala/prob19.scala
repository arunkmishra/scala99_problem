/*scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)*/

object prob19 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

    println("Rotate list : " + rotateRec(-2,ls))
    println("Rotate list : " + rotate(-2,ls))
  }

  def rotateRec[A](n: Int,ls: List[A]): List[A] = (n,ls) match {
      case (_, Nil) => Nil
      case (0,list) => list
      case (i,h :: tail) if i>0 => rotateRec(n-1,tail :+ h)
      case (i,list) if i<0 => rotateRec(n+1, list.last :: list.dropRight(1))
  }

  def rotate[A](n: Int, ls: List[A]): List[A] = {
    val nBounded = if (ls.isEmpty) 0 else n % ls.length
    if (nBounded < 0) rotate(nBounded + ls.length, ls)
    else ls.drop(nBounded) ::: ls.take(nBounded)
  }
}

