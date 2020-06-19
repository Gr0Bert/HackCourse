package hack.compiler

object StatementTranslator {
  import Compiler._
  import hack.vm.StackMachine._

  var labelCount = 0
  def translate(statement: Statement, className: String, clSt: SymbolTable, subSt: SymbolTable): Seq[Command] = {
    statement match {
      case Statement.Let(varName, expression, eqExpression) =>
        expression match {
          case Some(arrayExpression) => ??? // todo: add array expr support
          case None =>
            val SymbolTable.Entry(kind, tp, index) =
              subSt
                .get(varName)
                .orElse(clSt.get(varName))
                .getOrElse(throw new RuntimeException(s"Undefined variable $varName"))
            val segment = Common.kindToSegment(kind)
            ExpressionTranslator.translate(eqExpression, clSt, subSt) :+ MemoryAccess.Pop(segment, index)
        }
      case Statement.If(expression, trueCond, falseCond) =>
        val falseLabel = s"if_${className}_${labelCount}_false"
        val trueLabel = s"if_${className}_${labelCount}_true"
        labelCount += 1
        ExpressionTranslator.translate(expression, clSt, subSt) ++
          Seq(Arithmetic.Not, Branching.IfGoTo(falseLabel)) ++
          trueCond.flatMap(translate(_, className, clSt, subSt)) ++
          Seq(Branching.GoTo(trueLabel), Branching.Label(falseLabel)) ++
          falseCond.toList.flatten.flatMap(translate(_, className, clSt, subSt)) ++
          Seq(Branching.Label(trueLabel))

      case Statement.While(expression, statements) =>
        val loopLabel = s"while_${className}_${labelCount}_loop"
        val endLabel = s"while_${className}_${labelCount}_end"
        labelCount += 1
        Seq(Branching.Label(loopLabel))
        ExpressionTranslator.translate(expression, clSt, subSt) ++
        Seq(Arithmetic.Not, Branching.IfGoTo(endLabel)) ++
        statements.map(translate(_, className, clSt, subSt)) ++
        Seq(Branching.GoTo(loopLabel), Branching.Label(endLabel))

      case Statement.Do(subroutineCall) => ???
      case Statement.Return(expression) => ???
    }
  }
}
