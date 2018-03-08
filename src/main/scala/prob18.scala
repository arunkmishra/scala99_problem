import scala.annotation.tailrec

/*scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g)*/

object prob18 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

    println("Sliced List : " + sliceBuiltIn(3,7,ls))

    println("Slice Recuresive : "+ sliceRec(3,7,ls))

    println("Slice tail recursive : " + sliceTailRec(3,7,ls))

    println("Slice tail rec If : " + sliceTailRecIF(3,7,ls))

    println("Slice functional : " + sliceFunctional(3,7,ls))

    println("Slice using collect : " + sliceCollect(3,7,ls))
  }

  def sliceBuiltIn[A](s:Int, e: Int, ls: List[A]): List[A] = ls.slice(3,7)

  def sliceRec[A](s:Int, e: Int, ls: List[A]): List[A] = (s,e,ls) match {
    case (_,_,Nil) => Nil
    case (_,r,li) if r<=0  => Nil
    case (l,r,h :: tail) if l<=0 => h::sliceRec(l,r-1,tail)
    case (l,r,h :: tail) => sliceRec(l-1,r-1,tail)
  }

  def sliceTailRec[A](s: Int,e: Int, ls: List[A]): List[A] = {
    @tailrec
    def sliceTail(l:Int, r: Int, res: List[A], li: List[A]): List[A] = (l,r,li) match{
      case (_,_,Nil) => res
      case (_,r,lis) if r<=0 => res
      case (l,r,h::tail) if l<=0 => sliceTail(l,r-1,res:+h,tail)
      case (l,r,h::tail) => sliceTail(l-1,r-1,res,tail)
    }
    sliceTail(s,e,List[A](),ls)
  }

  def sliceTailRecIF[A](s: Int,e: Int, ls: List[A]): List[A] ={
    def sliceTail(c: Int, res: List[A], li: List[A]): List[A] =
      if((li.isEmpty || c>=e) && e>li.size)
        res
      else sliceTail(c+1,
        if(c>=s) res:+li.head else res,
        li.tail)
    sliceTail(0,Nil,ls)
  }

  def sliceFunctional[A](s: Int,e: Int, ls: List[A]): List[A] = ls.drop(s).take(e-(s max 0))

  def sliceCollect[A](s: Int,e: Int, ls: List[A]): List[A] = ls.zipWithIndex.collect({case (ele,i) if i>=s&&i<e => ele})
}
