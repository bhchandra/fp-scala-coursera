package week4_types_and_patternMatching

class IdealizedBoolean {

  abstract class Boolean {

    def ifThenElse[T](thenClause: => T, elseClause: => T): T

    def &&(b: => Boolean): Boolean = ifThenElse(b, False)

    def ||(b: => Boolean): Boolean = ifThenElse(True, b)

    def unary_! : Boolean = ifThenElse(False, True)

    def ==(b: => Boolean): Boolean = ifThenElse(b, !b)

    def !=(b: => Boolean): Boolean = ifThenElse(!b, b)

    //asuume False < True
    def <(b: => Boolean): Boolean = ifThenElse(False, b)

  }

  object True extends Boolean {
    override def ifThenElse[T](thenClause: => T, elseClause: => T): T = thenClause
  }

  object False extends Boolean {
    override def ifThenElse[T](thenClause: => T, elseClause: => T): T = elseClause
  }

}
