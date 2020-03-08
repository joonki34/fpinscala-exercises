package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Exercise 25
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  // Exercise 26
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(value) => value
      case Branch(left, right) =>
        maximum(left).max(maximum(right))
    }

  // Exercise 27
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(left, right) =>
        1 + depth(left).max(depth(right))
    }

  // Exercise 28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  // Exercise 29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(value) => f(value)
      case Branch(left, right) =>
        g(fold(left)(f)(g), fold(right)(f)(g))
    }

  def size2[A](t: Tree[A]): Int =
    fold(t)(x => 1)((a, b) => 1 + a + b)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(x => x)((a, b) => a.max(b))

  def depth2[A](t: Tree[A]): Int =
    fold(t)(x => 1)((a, b) => 1 + a.max(b))

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)): Tree[B])((a, b) => Branch(a, b))
}

object TreeExercise extends App {
  // Exercise 25
  println(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))

  // Exercise 26
  println(Tree.maximum(Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(11), Leaf(3)))))

  // Exercise 27
  println(Tree.depth(Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(11), Branch(Leaf(3), Leaf(9))))))

  // Exercise 28
  println(Tree.map(Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(11), Branch(Leaf(3), Leaf(9)))))(x => x.toString + "!!"))

  // Exercise 29
  println(Tree.size2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))

  println(Tree.maximum2(Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(11), Leaf(3)))))

  println(Tree.depth2(Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(11), Branch(Leaf(3), Leaf(9))))))

  println(Tree.map2(Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(11), Branch(Leaf(3), Leaf(9)))))(x => x.toString + "!!"))

  // Q. 이 fold 함수와 List에 대한 왼쪽, 오른쪽 fold 사이의 유사성을 찾아낼 수 있는가?
  // A. Branch 시그니쳐 부분이 비슷하다.
}
