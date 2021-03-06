package hack.compiler

import Compiler._
import hack.vm.StackMachine._

final class StatementTranslator(st: ComposedSymbolTable, fSt: FunctionSymbolTable, className: String) {
  val et = new ExpressionTranslator(st, fSt, className)

  var labelCount = 0
  def translate(statement: Statement): Seq[Command] = {
    statement match {
      case Statement.Let(varName, expression, eqExpression) =>
        expression match {
          case Some(arrayExpression) =>
            val SymbolTable.Entry(kind, _, index) = st.get(varName)
            val segment = Common.kindToSegment(kind)
            Seq(MemoryAccess.Push(segment, index)) ++
            et.translate(arrayExpression) ++
            Seq(Arithmetic.Add) ++
            et.translate(eqExpression) ++
            Seq(
              MemoryAccess.Pop(MemoryAccess.Segment.Temp, 0),
              MemoryAccess.Pop(MemoryAccess.Segment.Pointer, 1),
              MemoryAccess.Push(MemoryAccess.Segment.Temp, 0),
              MemoryAccess.Pop(MemoryAccess.Segment.That, 0)
            )

          case None =>
            val SymbolTable.Entry(kind, _, index) = st.get(varName)
            val segment = Common.kindToSegment(kind)
            et.translate(eqExpression) :+ MemoryAccess.Pop(segment, index)
        }
      case Statement.If(expression, trueCond, falseCond) =>
        val falseLabel = s"if_${className}_${labelCount}_false"
        val trueLabel = s"if_${className}_${labelCount}_true"
        labelCount += 1
        et.translate(expression) ++
          Seq(Arithmetic.Not, Branching.IfGoTo(falseLabel)) ++
          trueCond.flatMap(translate) ++
          Seq(Branching.GoTo(trueLabel), Branching.Label(falseLabel)) ++
          falseCond.toList.flatten.flatMap(translate) ++
          Seq(Branching.Label(trueLabel))

      case Statement.While(expression, statements) =>
        val loopLabel = s"while_${className}_${labelCount}_loop"
        val endLabel = s"while_${className}_${labelCount}_end"
        labelCount += 1
        Seq(Branching.Label(loopLabel)) ++
        et.translate(expression) ++
        Seq(Arithmetic.Not, Branching.IfGoTo(endLabel)) ++
        statements.flatMap(translate) ++
        Seq(Branching.GoTo(loopLabel), Branching.Label(endLabel))

      case Statement.Do(subroutineCall) =>
        et.translate(subroutineCall) ++ Seq(MemoryAccess.Pop(MemoryAccess.Segment.Temp, 0))

      case r@Statement.Return(expression) =>
        expression match {
        case Some(value) => et.translate(value) ++ Seq(Function.Return)
        case None => Seq(Function.Return)
      }
    }
  }
}
