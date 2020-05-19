package hack

package object compiler {
  object Compiler {

    sealed trait AST

    sealed trait Term extends AST

    sealed trait Token extends AST

    object Token {
      final case class Keyword(value: String) extends Token
      final case class Symbol(value: String) extends Token
      final case class IntegerConstant(value: Int) extends Token with Term
      final case class StringConstant(value: String) extends Token with Term
      final case class Identifier(value: String) extends Token with Term
    }

    sealed trait Structure extends AST

    object Structure {
      final case class ClassStruct(
        className: Token.Identifier,
        classVarDec: List[ClassVarDec],
        subroutineDec: List[SubroutineDec]
      ) extends Structure

      final case class ClassVarDec(
        keyword: Token.Keyword,
        varDec: VarDec,
      ) extends Structure

      sealed trait Type extends Structure
      object Type {
        final case class Primitive(value: String) extends Type
        final case class UserDefined(value: Token.Identifier) extends Type
      }

      final case class SubroutineDec(
        keyword: Token.Keyword,
        `type`: Type,
        subroutineName: Token.Identifier,
        parameterList: Seq[SubroutineParameter],
        subroutineVars: List[VarDec],
        subroutineStatements: List[Statement],
      ) extends Structure

      final case class SubroutineParameter(
        `type`: Type,
        varName: Token.Identifier
      ) extends Structure

      final case class VarDec(
        `type`: Type,
        varNames: Seq[Token.Identifier]
      ) extends Structure
    }

    sealed trait Statement extends AST

    object Statement {

      final case class Let(
        varName: Token.Identifier,
        expression: Option[Expression],
        eqExpression: Expression
      ) extends Statement

      final case class If(
        expression: Expression,
        trueCond: Seq[Statement],
        falseCond: Seq[Statement],
      ) extends Statement

      final case class While(
        expression: Expression,
        statements: Seq[Statement],
      ) extends Statement

      final case class Do(subroutineCall: Expression.SubroutineCall) extends Statement

      final case class Return(expression: Option[Expression]) extends Statement

    }

    sealed trait Expression extends AST

    object Expression {

      final case class KeywordConstant(value: String) extends Expression with Term

      final case class UnaryOp(value: String) extends Expression

      final case class Op(value: String) extends Expression

      final case class ExpressionDec(term: Term, terms: Seq[(Op, Term)]) extends Expression with Term

      final case class VarName(name: Token.Identifier) extends Expression with Term

      final case class VarAccess(name: VarName, expression: Expression) extends Expression with Term

      final case class UnaryOpApply(op: UnaryOp, term: Term) extends Expression with Term

      sealed trait SubroutineCall extends Expression with Term

      object SubroutineCall {

        final case class PlainSubroutineCall(
          subroutineName: Token.Identifier,
          expressionsList: Seq[ExpressionDec]
        ) extends SubroutineCall

        final case class ClassSubroutineCall(
          name: Token.Identifier,
          plainSubroutineCall: PlainSubroutineCall
        ) extends SubroutineCall
      }

    }
  }
}
