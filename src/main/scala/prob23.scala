import scala.util.Random
import prob20._
/*scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
res0: List[Symbol] = List('e, 'd, 'a)*/

object prob23 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'b, 'c, 'd, 'f, 'g, 'h)

    println("randomSelect  : " + randomSelect(3,ls))
  }

  def randomSelect[A](n: Int, ls: List[A]): List[A] = n match {
    case i if i<=0 => Nil
    case i => removeAt(Random.nextInt(ls.size),ls)._2 :: randomSelect(i-1,ls)
  }
}
