/*
Write a function:

object Solution { def solution(a: Array[Int]): Int }

that, given an array A of N integers, returns the smallest positive integer (greater than 0) that does not occur in A.

For example, given A = [1, 3, 6, 4, 1, 2], the function should return 5.

Given A = [1, 2, 3], the function should return 4.

Given A = [−1, −3], the function should return 1.

Write an efficient algorithm for the following assumptions:

N is an integer within the range [1..100,000];
each element of array A is an integer within the range [−1,000,000..1,000,000].
*/


object Solution {
  def solution(a: Array[Int]): Int = {
  
    val ls = a.toList.filter(i => i > 0).sortWith(_ < _)

    def tailC(acc: Int, ls: List[Int]): Int = {
        if(acc!=1)
  	        acc
        else
  	        ls match {
                case h::s :: tail => if(s-h>1)
                                        tailC(h+1, s::tail)
                                    else
                                        tailC(acc, s::tail)
                case h:: Nil => tailC(h+1, Nil)
                case Nil => acc
            }
  
    }
    if(ls.size > 0)
        if(ls.head !=1)
            1
        else
            tailC(1, ls)
    else
        1
  }
}
