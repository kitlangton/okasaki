package okasaki

import okasaki.List._

sealed trait List[+A] { self =>

  def head: A =
    self match {
      case List.Empty       => throw new Error("OH NO")
      case Cons(head, tail) => head
    }

  def ++[A1 >: A](that: List[A1]): List[A1] =
    self match {
      case Cons(head, tail) =>
        Cons(head, tail ++ that)
      case Empty =>
        that
    }

  def update[A1 >: A](index: Int, value: A1): List[A1] =
    self match {
      case Cons(head, tail) if index > 0 =>
        Cons(head, tail.update(index - 1, value))
      case Cons(_, tail) =>
        Cons(value, tail)
      case Empty =>
        Empty
    }

  def render: String =
    self match {
      case Cons(head, tail) =>
        head.toString + " :: " + tail.render
      case List.Empty =>
        "Nil"
    }
}

// data List a = Nil | Cons a (List a)
object List {
  case object Empty                        extends List[Nothing]
  case class Cons[+A](a: A, tail: List[A]) extends List[A]
}

object ListExamples {
  val list1: Cons[Int]    = Cons(0, Cons(1, Cons(2, Empty)))
  val list2: Cons[Int]    = Cons(3, Cons(4, Cons(5, Empty)))
  val combined: List[Int] = list1 ++ list2

  def main(args: Array[String]): Unit = {
    println(combined)
  }
}
