package hack

package object compiler {
  sealed trait Term

  sealed trait Token
  object Token {
    final case class Keyword(value: String) extends Token
    final case class Symbol(value: String) extends Token
    final case class IntegerConstant(value: String) extends Token with Term
    final case class StringConstant(value: String) extends Token with Term
    final case class Identifier(value: String) extends Token with Term
  }

  sealed trait Structure
  object Structure {
    final case class ClassStruct(
      keyword: String,
      className: Token.Identifier,
      classVarDec: List[ClassVarDec],
      subroutineDec: List[SubroutineDec]
    ) extends Structure
    final case class ClassVarDec(
      keyword: String,
      `type`: Type,
      identifier: List[String],
    ) extends Structure
    final case class Type(value: String) extends Structure
    final case class SubroutineDec(
      keyword: String,
      `type`: Type,
      subroutineName: Token.Identifier,
      parameterList: List[SubroutineParameter],
      subroutineVars: List[VarDec],
      subroutineStatements: List[Statement],
    )
    final case class SubroutineParameter(
      `type`: Type,
      varName: Token.Identifier
    ) extends Structure
    final case class VarDec(keyword: String, `type`: Type, varNames: List[Token.Identifier])
  }
  sealed trait Statement
  object Statement {
    final case class Let(
      keyword: String,
      varName: Token.Identifier,
      expression: Option[Expression],
      eqExpression: Expression
    ) extends Statement
    final case class If(
      expression: Expression,
      trueCond: List[Statement],
      falseCond: List[Statement],
    ) extends Statement
    final case class While(
      expression: Expression,
      statements: List[Statement],
    ) extends Statement
    final case class Do(keyword: String, subroutineCall: Expression.SubroutineCall)
        extends Statement
    final case class Return(keyword: String, expression: Option[Expression]) extends Statement
  }
  sealed trait Expression extends Term
  object Expression {
    final case class KeywordConstant(value: String) extends Expression
    final case class UnaryOp(value: String) extends Expression
    final case class Op(value: String) extends Expression
    final case class ExpressionDec(term: Term, terms: List[(Op, Term)]) extends Expression
    // varName[expression]
    final case class VarAccess(varName: Token.Identifier, expression: Option[Expression]) extends Expression
    sealed trait SubroutineCall
    object SubroutineCall {
      final case class PlainSubroutineCall(
        subroutineName: Token.Identifier,
        expressionsList: List[ExpressionDec]
      ) extends Expression
      final case class ClassSubroutineCall(
        name: Token.Identifier,
        plainSubroutineCall: PlainSubroutineCall
      ) extends Expression
    }
  }
}
