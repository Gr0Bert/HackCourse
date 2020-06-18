package hack.compiler

object StatementTranslator {
  import Compiler._
  import hack.vm.StackMachine._

  def translate(statement: Statement, clSt: SymbolTable, subSt: SymbolTable): Seq[Command] = {
    statement match {
      case Statement.Let(varName, expression, eqExpression) => ???
      case Statement.If(expression, trueCond, falseCond) => ???
      case Statement.While(expression, statements) => ???
      case Statement.Do(subroutineCall) => ???
      case Statement.Return(expression) => ???
    }
  }
}
