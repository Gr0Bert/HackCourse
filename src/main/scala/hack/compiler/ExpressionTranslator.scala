package hack.compiler

object ExpressionTranslator {
  import Compiler._
  import hack.vm.StackMachine._

  def translate(expression: Expression, clSt: SymbolTable, subSt: SymbolTable): Seq[Command] = {
    expression match {
      case Expression.KeywordConstant(value) =>
        value match {
          case "true" => ???
          case "false" => ???
          case "null" => ???
          case "this" => ???
        }
      case Expression.Braces(expr) => translate(expr, clSt, subSt)
      case Expression.Op(value) =>
        value match {
          case "+" => Seq(Arithmetic.Add)
          case "-" => Seq(Arithmetic.Sub)
          case "*" => Seq(Function.Call("Math.multiply", 2))
          case "/" => Seq(Function.Call("Math.divide", 2))
          case "&" => Seq(Arithmetic.And)
          case "|" => Seq(Arithmetic.Or)
          case "<" => Seq(Arithmetic.Lt)
          case ">" => Seq(Arithmetic.Gt)
          case "=" => Seq(Arithmetic.Eq)
        }

      case Expression.BinaryOp(first, op, second) =>
        translate(first, clSt, subSt) ++ translate(second, clSt, subSt) ++ translate(op, clSt, subSt)
      case Expression.VarName(name) =>
        val SymbolTable.Entry(kind, tp, index) = subSt.get(name)
        val ms = kind match {
          case "local" => MemoryAccess.Segment.Local
          case "field" => MemoryAccess.Segment.This
          case "static" => MemoryAccess.Segment.Static
          case "argument" => MemoryAccess.Segment.Argument
        }
        Seq(MemoryAccess.Push(ms, index))
      case Expression.VarAccess(name, expression) => ???
      case Expression.UnaryOpApply(op, term) =>
        val operation = op match {
          case "-" => Arithmetic.Neg
          case "~" => Arithmetic.Not
        }
        translate(term, clSt, subSt) :+ operation
      case Expression.IntegerConstant(value) =>
        Seq(MemoryAccess.Push(MemoryAccess.Segment.Constant, value))
      case Expression.StringConstant(value) => ???
      case Expression.SubroutineCall.PlainSubroutineCall(name, expressions) =>
        expressions.flatMap(e => translate(e, clSt, subSt)) :+ Function
          .Call(name.value, expressions.size)
      case Expression.SubroutineCall.ClassSubroutineCall(className, inner) =>
        translate(
          expression = inner.copy(subroutineName =
            Token.Identifier(s"${className.value}.${inner.subroutineName.value}")
          ),
          clSt = clSt,
          subSt = subSt
        )
    }
  }
}
