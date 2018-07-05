package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = {
    def iterTree(tt: Tree[Int], max: Int): Int = {
      tt match {
        case Leaf(l) => l.max(max)
        case Branch(left, right) => iterTree(left, max).max(iterTree(right, max))
      }
    }

    iterTree(t, Int.MinValue)
  }

  def depth[A](t: Tree[A]): Int = {
    var d = 0

    def iterTree(tt: Tree[A], depthest: Int): Unit = {
      tt match {
        case Leaf(_) => d = d.max(depthest)
        case Branch(l, r) => {
          iterTree(l, depthest + 1)
          iterTree(r, depthest + 1)
        }
      }
    }

    iterTree(t, 1)
    d
  }

  def fold[A, B](as: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    as match {
      case Leaf(l) => f(l)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def map[A, B](t: Tree[A])(f: A => B) : Tree[B] = fold[A, Tree[B]](t)(a => Leaf(f(a)))((l, r) => Branch(l, r))

  def depthFold[A](t: Tree[A]): Int = fold[A, Int](t)(_ => 1)((a, b) => 1 + a.max(b))

  def maximumFold(t: Tree[Int]): Int = fold[Int, Int](t)(l => l)((a, b) => a.max(b))

  def sizeFold[A](t: Tree[A]): Int = fold[A, Int](t)(_ => 1)((l, r) => 1 + l + r)
}

object TestTree {
  def main(args: Array[String]): Unit = {
    var t = new Branch[Int](new Leaf[Int](1), new Branch[Int](new Leaf[Int](2), new Leaf[Int](1)))
    var b = Tree.map(t)(a => a + 1)
    println(Tree.depthFold(t))
    println(Tree.maximumFold(t))
    println(Tree.size(t))
    println(Tree.sizeFold(t))
  }
}
