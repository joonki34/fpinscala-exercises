package chapter2

object Exercise2 extends App {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int, last: A): Boolean = {
      if(n >= as.length) true
      else if(ordered(as(n), last) == false) false
      else loop(n+1, as(n))
    }

    loop(1, as(0))
  }

  private val intOrderFunc = (a: Int, b: Int) => a >= b
  println(s"${isSorted(Array(1, 2, 3), intOrderFunc)} == true")
  println(s"${isSorted(Array(1, 3, 2), intOrderFunc)} == false")
  println(s"${isSorted(Array(1, 2, 3, 4), intOrderFunc)} == true")
  println(s"${isSorted(Array(1, 2, 3, 3), intOrderFunc)} == true")
  println(s"${isSorted(Array(5, 4, 3), intOrderFunc)} == false")

  private val stringOrderFunc = (a: String, b: String) => a >= b
  println(s"${isSorted(Array("a", "b", "c"), stringOrderFunc)} == true")
  println(s"${isSorted(Array("d", "b", "c"), stringOrderFunc)} == false")
}
