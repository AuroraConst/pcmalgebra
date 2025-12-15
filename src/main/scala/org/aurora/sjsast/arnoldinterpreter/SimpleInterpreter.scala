package org.aurora.sjsast.arnoldinterpreter


object SimpleInterpreter :

  // The interpreter function that evaluates an expression
  def evaluate(expr: Expr): Value = expr match {
    case Number(value) => IntValue(value)

    // Handle arithmetic operations
    case Add(left, right) => (evaluate(left), evaluate(right)) match {
        case (IntValue(l), IntValue(r)) => IntValue(l + r)
        case _ => throw new IllegalArgumentException("Cannot add non-integer values")
    }
    case Subtract(left, right) => (evaluate(left), evaluate(right)) match {
        case (IntValue(l), IntValue(r)) => IntValue(l - r)
        case _ => throw new IllegalArgumentException("Cannot subtract non-integer values")
    }
    case Multiply(left, right) => (evaluate(left), evaluate(right)) match {
        case (IntValue(l), IntValue(r)) => IntValue(l * r)
        case _ => throw new IllegalArgumentException("Cannot multiply non-integer values")
    }
    case Divide(left, right) => (evaluate(left), evaluate(right)) match {
        case (IntValue(l), IntValue(r)) =>
        if (r == 0) throw new ArithmeticException("Division by zero")
        IntValue(l / r)
        case _ => throw new IllegalArgumentException("Cannot divide non-integer values")
    }

    // Handle inequality operations
    case GreaterThan(left, right) => (evaluate(left), evaluate(right)) match {
        case (IntValue(l), IntValue(r)) => BoolValue(l > r)
        case _ => throw new IllegalArgumentException("Cannot compare non-integer values")
    }
    case LessThan(left, right) => (evaluate(left), evaluate(right)) match {
        case (IntValue(l), IntValue(r)) => BoolValue(l < r)
        case _ => throw new IllegalArgumentException("Cannot compare non-integer values")
    }
    case EqualTo(left, right) => (evaluate(left), evaluate(right)) match {
        case (IntValue(l), IntValue(r)) => BoolValue(l == r)
        case (BoolValue(l), BoolValue(r)) => BoolValue(l == r)
        case _ => throw new IllegalArgumentException("Cannot compare dissimilar types")
    }
  }
