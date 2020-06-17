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
      case Expression.Braces(expr) => ???
      case Expression.Op(value) =>
        value match {
          case "+" => ???
          case "-" => ???
          case "*" => ???
          case "/" => ???
          case "&" => ???
          case "|" => ???
          case "<" => ???
          case ">" => ???
          case "=" => ???
        }

      case Expression.BinaryOp(first, op, second) => ???
      case Expression.VarName(name) => ???
      case Expression.VarAccess(name, expression) => ???
      case Expression.UnaryOpApply(op, term) => ???
      case Expression.IntegerConstant(value) => ???
      case Expression.StringConstant(value) => ???
      case Expression.SubroutineCall.PlainSubroutineCall(name, expressions) => ???
      case Expression.SubroutineCall.ClassSubroutineCall(
          className,
          Expression.SubroutineCall.PlainSubroutineCall(name, expressions)
          ) => ???
    }
  }
}
