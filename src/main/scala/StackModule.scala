trait StackModule {
  type Stack[+_]

  def empty[A]: Stack[A]
  def isEmpty[A](stack: Stack[A]): Boolean

  def cons[A](a: A, stack: Stack[A]): Stack[A]
  def head[A](stack: Stack[A]): A
  def tail[A](stack: Stack[A]): Stack[A]
}

object StackModule {
  type Aux[F[+_]] = StackModule { type Stack[A] = F[A] }

  val listStack: StackModule.Aux[List] =
    new StackModule {
      override type Stack[+A] = List[A]

      override def empty[A]: List[A] = List.empty

      override def isEmpty[A](stack: List[A]): Boolean = stack.isEmpty

      override def cons[A](a: A, stack: List[A]): List[A] = a +: stack

      override def head[A](stack: List[A]): A = stack.head

      override def tail[A](stack: List[A]): List[A] = stack.tail
    }
}
