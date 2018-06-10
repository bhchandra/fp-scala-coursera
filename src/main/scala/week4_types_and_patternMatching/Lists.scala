package week4_types_and_patternMatching

object Lists {

  trait List[T] {

    def head: T

    def tail: List[T]

    def isEmpty: Boolean

    override def toString: String = "[]"

  }

  class Nil[T] extends List[T] {

    def head = throw new java.util.NoSuchElementException("head of EmptyList")

    def tail = throw new java.util.NoSuchElementException("tail of EmptyList")

    def isEmpty = true

    override def toString: String = "[]"

  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false

    override def toString: String =
      head.toString + " : " + tail.toString
  }

  def apply[T]: List[T] = new Nil[T]

  def apply[T](i: T): List[T] = new Cons[T](i, new Nil)

  def apply[T](i: T, j: T): List[T] = new Cons[T](i, new Cons[T](j, new Nil))


}

object TestLists extends App {
  println(Lists())
  println(Lists(1))
  println(Lists(1, 2))
}
