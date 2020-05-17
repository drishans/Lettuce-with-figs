package edu.colorado.csci3155.project2

import scala.util.Failure

/* Define the values used in our interpreter */
sealed trait Value
case class NumValue(f: Double) extends Value
case class FigValue(c: MyCanvas) extends Value
case class Closure(x: String, e: Expr, env: Environment) extends Value
case class BoolValue(b: Boolean) extends Value

/* Create some utility functions to operate on values.  */
object ValueOps {

    def plus(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => NumValue(f1 + f2)
        case (FigValue(f1), FigValue(f2)) => FigValue(f1.overlap(f2))
        case _ => throw new IllegalArgumentException("Cannot add booleans or closures")
    }

    def minus(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => NumValue(f1 - f2)
        case _ => throw new IllegalArgumentException("Cannot subtract figures, booleans or closures")
    }

    def mult(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => NumValue(f1 * f2)
        case (FigValue(f1), FigValue(f2)) => FigValue(f1.placeRight(f2))
        case _ => throw new IllegalArgumentException("Cannot multiply booleans or closures")
    }

    def divide(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => NumValue(f1 / f2)
        case (FigValue(f1), FigValue(f2)) => FigValue(f2.placeTop(f1))
        case (FigValue(f1), NumValue(rad)) => FigValue(f1.rotate(rad))
        case (NumValue(rad), FigValue(f1)) => FigValue(f1.rotate(rad))
        case _ => throw new IllegalArgumentException("Cannot divide booleans or closures")
    }

    def geq(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => BoolValue(f1 >= f2)
        case _ => throw new IllegalArgumentException("Cannot compare non numeric expressions with the geq comparator")
    }

    def gt(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => BoolValue(f1 > f2)
        case _ => throw new IllegalArgumentException("Cannot compare non numeric expressions with the eq comparator")
    }

    def equal(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => BoolValue(f1 == f2)
        case (BoolValue(b1), BoolValue(b2)) => BoolValue(b1 == b2)
        case _ =>  throw new IllegalArgumentException("Cannot compare non numeric/boolean expressions with the eq comparator")
    }

    def notEqual(v1: Value, v2: Value): Value = {
        val v = equal(v1, v2)
        v match {
            case BoolValue(b) => BoolValue(!b)
            case _ => throw new IllegalArgumentException("Internal error: something is really wrong") // This should never happen
        }
    }



}