object practice extends App {
  def remove(ls: List[Char]): String = {
    def oneCheck(c: Char, r: List[Char]): List[Char] = r.headOption match {
      case Some(x) if x - c == 1 => oneCheck(x, r.tail)
      case _ => r
    }

    def reT(acc: List[Char], v: List[Char]): List[Char] = {
      v match {
        case h :: s:: tail if (s-h) ==1 => reT(List.empty[Char], acc :::oneCheck(s, tail))
        case h :: s:: tail if (s-h) != 1 => reT(acc:+h, s::tail)
        case h:: Nil => reT(acc :+ h, Nil)
        case _ => acc
      }
    }
    val c = reT(List.empty[Char], ls)
    c.mkString
  }
  val ls: List[Char] = "axefyzijkmoq".toList



  println('b'-'a')
  println("res : " + remove(ls))
}
