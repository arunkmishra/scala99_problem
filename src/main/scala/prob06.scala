import scala.annotation.tailrec
import prob05._
/*Find out whether a list is a palindrome.
Example:
  scala> isPalindrome(List(1, 2, 3, 2, 1))
res0: Boolean = true*/
object findMidList {
  def unapply[A](l : List[A])=l match {       //Note : The unapply() method deconstructs the incoming value returning an Option, either None if the value does not match the format or Some() with a tuple of extracted values.
    case Nil=> None
    case l if l.length == 1 => Some(l.head,l.last,List())
    case l => Some(l.head,l.last, l.init.tail)          //l.init will return list leaving last element
  }
}
object prob6 {

  def main(args: Array[String]): Unit = {
    val ls = List(1,2,3,3,2,1)
    val lstr = List("aa","bb","bb","aa")

    println("isPalindrome : " + isPalindrome(lstr))
    println("isPalRev : " + isPalRev(lstr))
  }
//method 1 : using unapply
  def isPalindrome[A](ls: List[A]): Boolean = {
    @tailrec
    def isPalind(res: Boolean, ls: List[A]):Boolean=ls match{
      case Nil => res
      case findMidList(f,l,mid)=>isPalind(res && f==l, mid)
    }
    isPalind(true,ls)
  }
  //method1 ends:

  //method2 : using reverse
  def isPalRev[A](ls: List[A]):Boolean={
    val revList = reverSe(ls) // or ls.reverse which is List function ; here used reverse from prob5
    if(revList == ls) true
    else false
  }
}