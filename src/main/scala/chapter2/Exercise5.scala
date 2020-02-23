package chapter2

object Exercise5 extends App {
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  println("Compiled")
}
