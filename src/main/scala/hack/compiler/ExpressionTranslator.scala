package hack.compiler

import Compiler._
import hack.compiler.Compiler.Structure.Type
import hack.vm.StackMachine._

final class ExpressionTranslator(st: ComposedSymbolTable) {

  def translate(expression: Expression): Seq[Command] = {
    expression match {
      case Expression.KeywordConstant(value) =>
        value match {
          case "true" => Seq(MemoryAccess.Push(MemoryAccess.Segment.Constant, 0))
          case "false" => Seq(MemoryAccess.Push(MemoryAccess.Segment.Constant, 1), Arithmetic.Neg)
          case "null" => Seq(MemoryAccess.Push(MemoryAccess.Segment.Constant, 0))
          case "this" =>  Seq(MemoryAccess.Pop(MemoryAccess.Segment.Pointer, 0))
        }

      case Expression.Braces(expr) => translate(expr)

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

      case Expression.BinaryOp(first, op, second) => // todo: braces for priority
        translate(first) ++ translate(second) ++ translate(op)

      case Expression.VarName(name) =>
        val SymbolTable.Entry(kind, tp, index) = st.get(name)
        val ms = Common.kindToSegment(kind)
        Seq(MemoryAccess.Push(ms, index))

      case Expression.VarAccess(name, expression) =>
        val SymbolTable.Entry(kind, _, index) = st.get(name.name)
        val segment = Common.kindToSegment(kind)
        Seq(MemoryAccess.Push(segment, index)) ++
          translate(expression) ++
          Seq(
            Arithmetic.Add,
            MemoryAccess.Pop(MemoryAccess.Segment.Pointer, 1),
            MemoryAccess.Push(MemoryAccess.Segment.That, 0)
          )

      case Expression.UnaryOpApply(op, term) =>
        val operation = op.value match {
          case "-" => Arithmetic.Neg
          case "~" => Arithmetic.Not
        }
        translate(term) :+ operation

      case Expression.IntegerConstant(value) =>
        Seq(MemoryAccess.Push(MemoryAccess.Segment.Constant, value))

      case Expression.StringConstant(value) => ???

      case Expression.SubroutineCall.PlainSubroutineCall(name, expressions) =>
        expressions.flatMap(e => translate(e)) :+ Function.Call(name.value, expressions.size)

      case Expression.SubroutineCall.ClassSubroutineCall(receiverName, inner) =>
        val receiver = st.get(receiverName)
        val receiverIndex = receiver.index
        val receiverType = receiver.`type` match {
          case Type.Primitive(value) =>
            throw new RuntimeException(
              s"Attempt to perform method call `$receiverName.${inner.subroutineName.value}` on primitive: $value"
            )
          case Type.UserDefined(value) => value
        }
        val receiverMs = Common.kindToSegment(receiver.kind)
        Seq(
          MemoryAccess.Push(receiverMs, receiverIndex)
        ) ++
          translate(
            inner.copy(subroutineName =
              Token.Identifier(s"$receiverType.${inner.subroutineName.value}")
            )
          )
    }
  }
}
