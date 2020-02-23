package chapter2

import scala.annotation.tailrec

object Exercise1 extends App {
  def fib(n: Int): Int = {
    @tailrec
    def fibInternal(a: Int, acc: Int, acc2: Int): Int = {
      if(a <= 0) acc
      else if(a == 1) acc2
      else {
        fibInternal(a-1, acc2, acc + acc2)
      }
    }

    fibInternal(n, 0, 1)
  }

  // 5, 0, 1
  // 4, 1, 1
  // 3, 1, 2
  // 2, 2, 3
  // 1, 3, 5

  println(s"[fib(0)] expected: 0, actual: ${fib(0)}")
  println(s"[fib(1)] expected: 1, actual: ${fib(1)}")
  println(s"[fib(2)] expected: 1, actual: ${fib(2)}")
  println(s"[fib(3)] expected: 2, actual: ${fib(3)}")
  println(s"[fib(4)] expected: 3, actual: ${fib(4)}")
  println(s"[fib(5)] expected: 5, actual: ${fib(5)}")
  println(s"[fib(6)] expected: 8, actual: ${fib(6)}")
  println(s"[fib(7)] expected: 13, actual: ${fib(7)}")
}
