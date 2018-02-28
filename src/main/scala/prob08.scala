import scala.annotation.tailrec

/*compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)*/

object prob08 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

    println("compressByMe : " + compressByMe(ls))
    println("compressDropWhile : "+ compressDropWhile(ls))
    println("compressDropWhileTail : "+ compressDropWhileTail(ls))
    println("compressUsingFold : "+ compressUsingFold(ls))
  }

  def compressByMe[A](symbols: List[A]):List[A]={
    @tailrec
    def compressByMeTail(res: List[A], ls: List[A]):List[A]= ls match{
      case Nil => res
      case h :: n :: tail if (!res.headOption.contains(h)) && h==n => compressByMeTail(h::res, tail)
      case h :: tail if res.headOption.contains(h) => compressByMeTail(res, tail)
      case h :: tail =>compressByMeTail(h :: res, tail)
    }
    compressByMeTail(List(),symbols).foldLeft(List[A]()){(h,r) => r :: h}
  }

  def compressDropWhile[A](ls: List[A]):List[A]=ls match{
    case h :: tail => h :: compressDropWhile(ls.dropWhile(_ == h))
    case Nil => Nil
  }

  def compressDropWhileTail[A](ls: List[A]): List[A] ={
    @tailrec
    def compressDWTailRec(res: List[A], ls: List[A]):List[A]= ls match {
      case Nil => res
      case h:: tail => compressDWTailRec(res :+ h, tail.dropWhile(_ == h))
    }
    compressDWTailRec(List[A](), ls)
  }

  def compressUsingFold[A](ls: List[A]): List[A]=
    ls.foldRight(List[A]()){(h,r) =>
      if(r.headOption.contains(h))
        r
      else
        h :: r
    }
}
