package okasaki

import okasaki.Tree.{Leaf, Node}

sealed trait Tree[+A] { self =>
  def isNode: Boolean = self match {
    case Tree.Leaf     => false
    case Node(_, _, _) => true
  }

  def insert[A1 >: A](x: A1)(implicit ord: Ordering[A1]): Tree[A1] =
    self match {
      case Leaf =>
        Node(x)
      case Node(y, left, right) if ord.lt(x, y) =>
        Node(y, left.insert(x), right)
      case Node(y, left, right) if ord.gt(x, y) =>
        Node(y, left, right.insert(x))
      case node =>
        node
    }

  def member[A1 >: A](x: A1)(implicit ord: Ordering[A1]): Boolean =
    self match {
      case Leaf =>
        false
      case Node(y, left, _) if ord.lt(x, y) =>
        left.member(x)
      case Node(y, _, right) if ord.gt(x, y) =>
        right.member(x)
      case Node(y, _, _) if ord.eq(x, y) =>
        true
    }

  def render(padding: String = ""): String =
    self match {
      case Tree.Leaf => ""
      case Node(value, left, right) =>
        val children    = Seq(left, right).filter(_.isNode)
        val numChildren = children.length

        val rendered = children.zipWithIndex.map { case (node, i) =>
          val isLeft       = i < numChildren - 1
          val char         = if (isLeft) "├╴" else "└╴"
          val nodeString   = node.render(padding + " ")
          val head :: tail = nodeString.linesIterator.toList
          val start        = padding + char + head

          val prefixChar   = if (isLeft) "│" else " "
          val prefixedTail = tail.map(prefixChar + _)
          (start :: prefixedTail).mkString("\n")
        }

        s"""
$padding$value
${rendered.mkString("\n")}
           """.trim
    }
}

object Tree {
  def empty: Tree[Nothing] = Leaf

  case object Leaf                                                           extends Tree[Nothing]
  case class Node[+A](value: A, left: Tree[A] = Leaf, right: Tree[A] = Leaf) extends Tree[A]
}

object TreeExample {
  val tree: Tree[Int] = Tree.empty
    .insert(1)
    .insert(2)
    .insert(3)
    .insert(4)
    .insert(0)
    .insert(-3)
    .insert(-3)
    .insert(-2)
    .insert(-5)
    .insert(3)
    .insert(8)
    .insert(5)

  def main(args: Array[String]): Unit = {
    println(tree.render())
  }
}
