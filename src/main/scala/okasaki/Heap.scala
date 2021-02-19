package okasaki

import okasaki.Heap.{Empty, Node, makeNode}

trait Ord[-A] { self =>
  def lteq(lhs: A, rhs: A): Boolean
}

object Ord {
  implicit def orderingToOrd[A](implicit ordering: Ordering[A]): Ord[A] = new Ord[A] {
    override def lteq(lhs: A, rhs: A): Boolean = ordering.lteq(lhs, rhs)
  }
}

sealed trait Heap[+A] { self =>
  def rank: Int

  def isEmpty: Boolean = self match {
    case Empty      => true
    case _: Node[A] => false
  }

  def insert[A1 >: A: Ord](value: A1): Heap[A1] =
    Heap.succeed(value).merge(self)

  def merge[A1 >: A](that: Heap[A1])(implicit ord: Ord[A1]): Heap[A1] =
    (self, that) match {
      case (Empty, node) => node
      case (node, Empty) => node
      case (node1 @ Node(_, a1, left1, right1), node2 @ Node(_, a2, left2, right2)) =>
        if (ord.lteq(a1, a2))
          makeNode(a1, left1, right1.merge(node2))
        else
          makeNode(a2, left2, node1.merge(right2))
    }

  def findMin: Option[A] = self match {
    case Empty                => None
    case Node(_, value, _, _) => Some(value)
  }

  def deleteMin(implicit ord: Ord[A]): Heap[A] = self match {
    case Empty                   => Empty
    case Node(_, _, left, right) => left.merge(right)
  }
}

object Heap {
  def succeed[A](value: A): Heap[A] = Node(1, value, empty, empty)
  def empty: Heap[Nothing]          = Empty

  def makeNode[A](value: A, left: Heap[A], right: Heap[A]): Heap[A] =
    if (left.rank >= right.rank)
      Node(right.rank + 1, value, left, right)
    else
      Node(left.rank + 1, value, right, left)

  case object Empty extends Heap[Nothing] {
    override def rank: Int = 0
  }

  case class Node[+A](rank: Int, value: A, left: Heap[A], right: Heap[A]) extends Heap[A]
}

object HeapExamples {
  def main(args: Array[String]): Unit = {
    val heap: Heap[Int] =
      Heap
        .succeed(10)
        .insert(8)
        .insert(12)
        .insert(13)
        .insert(6)

    println(heap)
    println(heap.findMin)
  }
}
