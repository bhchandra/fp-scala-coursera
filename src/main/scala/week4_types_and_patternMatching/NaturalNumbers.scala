package week4_types_and_patternMatching

import week4_types_and_patternMatching.NaturalNumbers.{Succ, Zero}

object NaturalNumbers {

  abstract class Nat {

    def isZero: Boolean

    def predecessor: Nat

    def successor: Nat

    def +(that: Nat): Nat

    def -(that: Nat): Nat

    def value: Int
  }

  object Zero extends Nat {
    override def isZero: Boolean = true

    override def predecessor: Nat = throw new NoSuchElementException

    override def successor: Nat = new Succ(Zero)

    override def +(that: Nat): Nat = that

    override def -(that: Nat): Nat = throw new UnsupportedOperationException

    //value and toString are added only to verify the correctness of the program visually
    override def value: Int = 0

    override def toString: String = value.toString
  }

  class Succ(n: Nat) extends Nat {
    override def isZero: Boolean = false

    override def predecessor: Nat = n

    override def successor: Nat = this + new Succ(Zero)

    /**
      * To add two Nats, we are adding both the numbers to zero in a loop until both the Nats are reduced to zero.
      */
    override def +(that: Nat): Nat = {

      def add(n1: Nat, n2: Nat, acc: Nat): Nat = {
        if (n1.isZero && n2.isZero) acc
        else if (n1.isZero) add(Zero, n2.predecessor, new Succ(acc))
        else if (n2.isZero) add(n1.predecessor, Zero, new Succ(acc))
        else add(n1.predecessor, n2.predecessor, new Succ(new Succ(acc)))
      }

      add(this, that, Zero)
    }

    /**
      * subtraction is a little tricky, keep reducing both the numbers to zero and but do not add 1 to acc, until n2 is zero.
      */
    override def -(that: Nat): Nat = {
      def subtract(n1: Nat, n2: Nat, acc: Nat): Nat = {
        if (n1.isZero && n2.isZero) acc
        else if (n1.isZero) Zero - n2
        else if (n2.isZero) subtract(n1.predecessor, Zero, new Succ(acc))
        else subtract(n1.predecessor, n2.predecessor, acc)
      }

      subtract(this, that, Zero)
    }

    /**
      * @return int representation of the Nat
      */
    override def value: Int = {
      def loop(n: Nat, acc: Int): Int = {
        if (n.isZero) acc
        else loop(n.predecessor, acc + 1)
      }

      loop(this, 0)
    }

    override def toString: String = value.toString

  }

}

object Test extends App {

  val one = new Succ(Zero)
  val two = new Succ(one)
  val three = new Succ(two)
  val four = new Succ(three)
  val five = new Succ(four)

  println(five) //prints 5
  println(five.predecessor) //prints 4
  println(five.successor) //prints 6
  println(five + four) //prints 9
  println(five - four) //prints 1
  println(five - three) //prints 2
  println(five - five.successor) //exception!

}
