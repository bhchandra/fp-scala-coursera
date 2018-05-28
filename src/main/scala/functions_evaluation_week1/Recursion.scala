package functions_evaluation_week1

object Recursion extends App {

  /**
    *     1
    *    1 1
    *   1 2 1
    *  1 3 3 1
    * 1 4 6 4 1
    * ...
    * Computes the elements of Pascal’s triangle by means of a recursive process.
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }



}
