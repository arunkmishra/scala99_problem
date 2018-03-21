import scala.util.Random
import prob23._
/*scala> lotto(6, 49)
res0: List[Int] = List(23, 1, 17, 33, 21, 37)*/

object prob24 {
  def main(args: Array[String]): Unit = {
    println("lotto : " + lotto(6,49))

    println("lottoProb23 : " + lottoProb23(6,49))
  }
  def lotto(n: Int, m: Int): List[Int] = if(n<=0) Nil else 1+Random.nextInt(m) :: lotto(n-1,m)

  def lottoProb23(n: Int, m: Int): List[Int] = randomSelect(n, Range(1,m).toList)
}
