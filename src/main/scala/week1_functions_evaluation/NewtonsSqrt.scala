package week1_functions_evaluation

object NewtonsSqrt extends App {


  /**
    * Start with a guess x1 > 0 and compute a sequence of improved guesses.
    * Improve the estimate by taking the mean of guess, and num/guess.
    */


  def sqrt(x: Double): Double = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double): Boolean = {
      val diff = guess * guess - x
      Math.abs(diff) / x < 0.001
    }

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }


  println(sqrt(2)) // prints 1.4142156862745097

  println(sqrt(4)) // prints 2.0000000929222947

  println(sqrt(6)) //prints 2.4494943716069653


  //Q. Why is isGoodEnough bad for small numbers and can lead to non-termination of very large numbers.
  //A. 1. Because the epsilon(0.001) used is big for small numbers (< 1).
  //   2. Because for large numbers we may never be able to achieve the abs diff(guess^2 -x) less than the epsilon(0.001)

  //Q. Define a different version of isGoodEnough that does not have these problems.


}
