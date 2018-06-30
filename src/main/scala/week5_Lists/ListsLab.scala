package week5_Lists

object ListsLab {
  /**
    * gives the last element of the list
    */
  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
  }


  /**
    * all the elements the the list except the last one.
    */
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  /**
    * concat all elements of xs and ys. complexity is length of xs.
    */
  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }


  /**
    * reverse the list
    */
  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => List()
    case z :: zs => zs.last :: reverse(z :: zs.init)
  }

  def flatten(xs: List[Any]): List[Any] = ???


  def msort[T](xs: List[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge[T](xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys1) => ys1
        case (xs1, Nil) => xs1
        case (x :: xs1, y :: ys1) =>
          if (x == y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  /**
    * packs consecutive duplicates of lists of elements into sublist.
    */
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (l1, l2) = xs1 span (y => y == x)
      l1 :: pack(l2)
  }

  /**
    * run length encoding
    */
  def encode[T](xs: List[T]): List[(T, Int)] = {
    pack(xs)
      .filter(x => x.isEmpty)
      .map(x => (x.head, x.length))
  }


  def sum(xs: List[Int]): Int = (0 :: xs) reduceLeft (_ + _)

  def product(xs: List[Int]): Int = (1 :: xs) reduceLeft (_ * _)

  def sum2(xs: List[Int]): Int = (xs foldLeft 0) (_ + _)

  def product2(xs: List[Int]): Int = (xs foldLeft 1) (_ * _)


  def reduceLeft[T](xs: List[T], op: (T, T) => T): T = xs match {
    case Nil => throw new Error("Nil.reduceLeft")
    case x :: xs1 => (xs1 foldLeft x) (op)
  }

}

import week5_Lists.ListsLab.pack

object TestLists extends App {

  val l = List(1, 2, 3, 4, 6)

  println(
    //    reverse(l)   // TODO : exception here
  )

  //  flatten(List(List(1, 1), 2, List(3, List(5, 8))))

  val x = pack(List("a", "a", "a", "b", "c", "c", "a")) // -> List(List("a", "a", "a"), List("b"). List("c", "c"), List("a"))

  println(x)

}
