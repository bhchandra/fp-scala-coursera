package week1_functions_evaluation

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


  /**
    * Verifies if a string has balanced parentheses
    */
  def balance(chars: List[Char]): Boolean = {

    def countOpen(c: Char): Int = if (c.equals('(')) 1 else 0

    def countClose(c: Char): Int = if (c.equals(')')) -1 else 0

    def loop(chars: List[Char], open: Int, close: Int): Boolean = {
      if (chars.isEmpty && (open + close) == 0) true
      else if (chars.isEmpty || (open + close) < 0) false
      else loop(chars.tail, open + countOpen(chars.head), close + countClose(chars.head))
    }

    loop(chars, 0, 0)
  }


}
