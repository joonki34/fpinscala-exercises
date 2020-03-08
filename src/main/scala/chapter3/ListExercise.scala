package chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  // Exercise 3
  def setHead[A](list: List[A], elem: A): List[A] = list match {
    case Nil => Nil
    case Cons(_, tail) => Cons(elem, tail)
  }

  // Exercise 4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n-1)
    }
  }

  // Exercise 5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) if(f(head)) => dropWhile(tail, f)
    case c @ Cons(_, _) => c
  }

  // Exercise 6
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)((x, y) => x * y)

  // Exercise 9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((x, y) => y + 1)

  // Exercise 10
  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  // Exercise 11
  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)((x, y) => x * y)

  // Exercise 12
//  def reverse[A](ns: List[A]): List[A] = {
//    val (head, tail) = ns match {
//      case Cons(head, tail) => (head, tail)
//    }
//
//    // 4, Cons(1, Nil) = Cons(4, Cons(1, Nil))
//    // 3, Cons(4, Cons(1, Nil)) = Cons(4, Cons(3, Cons(1, Nil)))
//    foldRight(tail, Cons(head, Nil): List[A])((x: A, ys: List[A]) => ys match {
//      case Cons(h, t) => Cons(h, Cons(x, t))
//    })
//  }

  // Exercise 14
  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, y) => Cons(x, y))

  // Exercise 16
  def appendOne(ns: List[Int]): List[Int] =
    foldRight(ns, Nil:List[Int])((x, y) => Cons(x+1, y) )

  // Exercise 17
  def doubleToString(ns: List[Double]): List[String] =
    foldRight(ns, Nil:List[String])((x, y) => Cons(x.toString, y) )

  // Exercise 18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((x, y) => Cons(f(x), y))

  // Exercise 19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((x, y) => if(f(x)) Cons(x, y) else y)

  // Exercise 20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    def prepend(l: List[B], r: List[B]): List[B] =
      l match {
        case Nil => r
        case Cons(head, tail) => Cons(head, prepend(tail, r))
      }

    foldRight(as, Nil:List[B])((x, y) => prepend(f(x), y))
  }

  // Exercise 21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if(f(x)) Cons(x, Nil) else Nil)

  // Exercise 22
  def zipPlus(l: List[Int], r: List[Int]): List[Int] =
    (l, r) match {
      case (Nil, Nil) => Nil
      case (Cons(head, tail), Nil) => Cons(head, tail)
      case (Nil, Cons(head, tail)) => Cons(head, tail)
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipPlus(t1, t2))
    }

  // Exericse 23
  def zipWith[A](l: List[A], r: List[A])(f: (A, A) => A): List[A] =
    (l, r) match {
      case (Nil, Nil) => Nil
      case (Cons(head, tail), Nil) => Cons(head, tail)
      case (Nil, Cons(head, tail)) => Cons(head, tail)
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
}

object ListExercise extends App {
  // Exercise 1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // Ans: 3
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  println(x)

  // Exercise 2
  println(List.tail(List(1, 2, 3, 4, 5)))

  // Exercise 3
  println(List.setHead(List(1, 2, 3, 4, 5), 9))

  // Exercise 4
  println(List.drop(List(1, 2, 3), 2))

  // Exercise 5
  println(List.dropWhile(List(2, 2, 3), (a: Int) => a == 2))

  // Exercise 6
  println(List.init(List(1, 2, 3, 4)))

  // Exercise 7
  // 재귀를 멈추지 않는다. 멈추는 stop function이 없다.
  // 평가 단축은 무슨 말???
  println(List.product2(List(0, 1, 2, 3)))

  // Exercise 8
  // 같은 값을 갖는 리스트를 반환한다.
  // foldRight와 List의 자료 생성자 사이의 관계? 모르겠다.
  println(List.foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _)))

  // Exercise 9
  println(List.length(List("a", "b", "c")))

  // Exercise 10
  // 재귀 함수의 결과를 바로 리턴하지 않아서 꼬리 재귀가 아니다.
  println(List.foldLeft(List(1, 2, 3), 0)((x, y) => x + y))

  // Exercise 11
  println(List.sum3(List(1, 2, 3, 4)))
  println(List.product3(List(1, 2, 3, 4)))

  // Exercise 12
  // TODO
  // println(List.reverse(List(1, 2, 3, 4)))

  // Exercise 13
  // TODO

  // Exercise 14
  println(List.append(List(1, 2, 3), List(4, 5, 6)))

  // Exercise 15
  // TODO

  // Exercise 16
  println(List.appendOne(List(1, 2, 3)))

  // Exercise 17
  println(List.doubleToString(List(1.0, 2.0, 3.0)))

  // Exercise 18
  println(List.map(List(1, 2, 3))(x => x + 1))

  // Exercise 19
  println(List.filter(List(1, 2, 3))(x => x % 2 == 0))

  // Exercise 20
  println(List.flatMap(List(1, 2, 3))(i => List(i, i)))

  // Exercise 21
  println(List.filter2(List(1, 2, 3))(x => x % 2 == 0))

  // Exercise 22
  println(List.zipPlus(List(1, 2, 3), List(4, 5, 6)))

  // Exercise 23
  println(List.zipWith(List(1, 2, 3), List(4, 5, 6))((x, y) => x + y))

  // Exercise 24
  // TODO
}
