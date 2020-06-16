package hack

package object compiler {
  object Compiler {
    final case class SymbolTable(
      entries: Map[Token.Keyword, Map[Token.Identifier, SymbolTable.Entry]]
    ) {
      def update(
        kind: Token.Keyword,
        name: Token.Identifier,
        `type`: Structure.Type
      ): SymbolTable = {
        val symbols = entries.getOrElse(kind, Map.empty)
        val newEntry = SymbolTable.Entry(`type`, symbols.size)
        if (symbols.get(name).isDefined) {
          throw new RuntimeException(s"Variable $name is already defined")
        } else {
          val updatedSymbols = symbols.updated(name, newEntry)
          new SymbolTable(entries.updated(kind, updatedSymbols))
        }
      }

      def get(kind: Token.Keyword, name: Token.Identifier): SymbolTable.Entry = {
        entries(kind)(name)
      }
    }

    object SymbolTable {
      final case class Entry(
        `type`: Structure.Type,
        index: Int
      )

      def empty: SymbolTable = SymbolTable(Map.empty)
    }

    sealed trait AST {
      def toXml: String
    }

    sealed trait Token extends AST

    object Token {
      final case class Keyword(value: String) extends Token {
        override def toXml: String = s"<keyword> $value </keyword>"
      }
      final case class Identifier(value: String) extends Token {
        override def toXml: String = s"<identifier> $value </identifier>"
      }
    }

    sealed trait Structure extends AST

    object Structure {
      final case class ClassStruct(
        className: Token.Identifier,
        classVarDec: Seq[ClassVarDec],
        subroutineDec: Seq[SubroutineDec],
        symbolTable: Option[SymbolTable] = None,
      ) extends Structure {
        override def toXml: String = {
          s"""<class>
             |<keyword> class </keyword>
             |${className.toXml}
             |<symbol> { </symbol>
             |${classVarDec.map(_.toXml).mkString(System.lineSeparator())}
             |${subroutineDec.map(_.toXml).mkString(System.lineSeparator())}
             |<symbol> } </symbol>
             |</class>""".stripMargin
        }
      }

      final case class ClassVarDec(
        keyword: Token.Keyword,
        `type`: Type,
        varNames: Seq[Token.Identifier],
      ) extends Structure {
        override def toXml: String = {
          s"""<classVarDec>
             |${keyword.toXml}
             |${`type`.toXml}
             |${varNames.map(_.toXml).mkString(s"${System.lineSeparator()}<symbol> , </symbol>")}
             |<symbol> ; </symbol>
             |</classVarDec>""".stripMargin
        }
      }

      sealed trait Type extends Structure
      object Type {
        final case class Primitive(value: String) extends Type {
          override def toXml: String = s"<keyword> $value </keyword>"
        }
        final case class UserDefined(value: Token.Identifier) extends Type {
          override def toXml: String = s"${value.toXml}"
        }
      }

      final case class SubroutineDec(
        keyword: Token.Keyword,
        `type`: Type,
        subroutineName: Token.Identifier,
        parameterList: Seq[SubroutineParameter],
        subroutineVars: Seq[VarDec],
        subroutineStatements: Seq[Statement],
        symbolTable: Option[SymbolTable] = None,
      ) extends Structure {
        override def toXml: String = {
          s"""<subroutineDec>
             |${keyword.toXml}
             |${`type`.toXml}
             |${subroutineName.toXml}
             |<symbol> ( </symbol>
             |<parameterList>
             |${parameterList
               .map(_.toXml)
               .mkString(s"${System.lineSeparator()}<symbol> , </symbol>")}
             |</parameterList>
             |<symbol> ) </symbol>
             |<subroutineBody>
             |<symbol> { </symbol>
             |${subroutineVars.map(_.toXml).mkString(System.lineSeparator())}
             |<statements>
             |${subroutineStatements.map(_.toXml).mkString(System.lineSeparator())}
             |</statements>
             |<symbol> } </symbol>
             |</subroutineBody>
             |</subroutineDec>""".stripMargin
        }
      }

      final case class SubroutineParameter(
        `type`: Type,
        varName: Token.Identifier
      ) extends Structure {
        override def toXml: String = {
          s"""${`type`.toXml}
             |${varName.toXml}
             |""".stripMargin
        }
      }

      final case class VarDec(
        `type`: Type,
        varNames: Seq[Token.Identifier]
      ) extends Structure {
        override def toXml: String = {
          s"""<varDec>
             |<keyword> var </keyword>
             |${`type`.toXml}
             |${varNames.map(_.toXml).mkString(s"${System.lineSeparator()}<symbol> , </symbol>")}
             |<symbol> ; </symbol>
             |</varDec>
             |""".stripMargin
        }
      }
    }

    sealed trait Statement extends AST

    object Statement {

      final case class Let(
        varName: Token.Identifier,
        expression: Option[Expression],
        eqExpression: Expression
      ) extends Statement {
        private def exprAccess = expression.map { e =>
          s"""<symbol> [ </symbol>
             |<expression>
             |${e.toXml}
             |</expression>
             |<symbol> ] </symbol>
             |""".stripMargin
        }
        override def toXml: String = {
          s"""<letStatement>
             |  <keyword> let </keyword>
             |  ${varName.toXml}
             |  ${exprAccess.getOrElse("")}
             |  <symbol> = </symbol>
             |  <expression>
             |  ${eqExpression.toXml}
             |  </expression>
             |  <symbol> ; </symbol>
             |</letStatement>
             |""".stripMargin
        }
      }

      final case class If(
        expression: Expression,
        trueCond: Seq[Statement],
        falseCond: Option[Seq[Statement]],
      ) extends Statement {
        private def falseToXml = {
          falseCond.map { fc =>
            s"""<keyword> else </keyword>
              |<symbol> { </symbol>
              |<statements>
              |${fc.map(_.toXml).mkString(System.lineSeparator())}
              |</statements>
              | <symbol> } </symbol>
              | """.stripMargin
          }.getOrElse("")
        }

        override def toXml: String = {
          s"""<ifStatement>
             |<keyword> if </keyword>
             |<symbol> ( </symbol>
             |<expression>
             |${expression.toXml}
             |</expression>
             |<symbol> ) </symbol>
             |<symbol> { </symbol>
             |<statements>
             |${trueCond.map(_.toXml).mkString(System.lineSeparator())}
             |</statements>
             |<symbol> } </symbol>
             |$falseToXml
             |</ifStatement>
             |""".stripMargin
        }
      }

      final case class While(
        expression: Expression,
        statements: Seq[Statement],
      ) extends Statement {
        override def toXml: String = {
          s"""<whileStatement>
             |<keyword> while </keyword>
             |<symbol> ( </symbol>
             |<expression>
             |${expression.toXml}
             |</expression>
             |<symbol> ) </symbol>
             |<symbol> { </symbol>
             |${statements.map(_.toXml).mkString(System.lineSeparator())}
             |<symbol> } </symbol>
             |</whileStatement>
             |""".stripMargin
        }
      }

      final case class Do(subroutineCall: Expression.SubroutineCall) extends Statement {
        override def toXml: String = {
          s"""<doStatement>
             |<keyword> do </keyword>
             |${subroutineCall.toXml
               .split(System.lineSeparator())
               .tail
               .reverse
               .tail
               .reverse
               .mkString(System.lineSeparator())}
             |<symbol> ; </symbol>
             |</doStatement>""".stripMargin
        }
      }

      final case class Return(expression: Option[Expression]) extends Statement {
        override def toXml: String = {
          s"""<returnStatement>
             |<keyword> return </keyword>
             |${expression
               .map(_.toXml)
               .map { e =>
                 s"""<expression>
               |$e
               |</expression>""".stripMargin
               }
               .getOrElse("")}
             |<symbol> ; </symbol>
             |</returnStatement>
             |""".stripMargin
        }
      }

    }

    sealed trait Expression extends AST

    object Expression {

      final case class KeywordConstant(value: String) extends Expression {
        override def toXml: String =
          s"""<term>
             |<keyword> $value </keyword>
             |</term>
             |""".stripMargin
      }

      final case class Braces(expr: Expression) extends Expression {
        override def toXml: String = {
          s"""<term>
             |<symbol> ( </symbol>
             |<expression>
             |${expr.toXml}
             |</expression>
             |<symbol> ) </symbol>
             |</term>""".stripMargin
        }
      }

      final case class Op(value: String) extends Expression {
        override def toXml: String = {
          s"""<symbol> $value </symbol>""".stripMargin
        }
      }

      final case class BinaryOp(first: Expression, op: Op, second: Expression) extends Expression {
        override def toXml: String = {
          s"""${first.toXml}
             |${op.toXml}
             |${second.toXml}
             |""".stripMargin
        }
      }

      final case class VarName(name: Token.Identifier) extends Expression {
        override def toXml: String =
          s"""<term>
             |${name.toXml}
             |</term>""".stripMargin
      }

      final case class VarAccess(name: VarName, expression: Expression) extends Expression {
        override def toXml: String =
          s"""<term>
             |${name.name.toXml}
             |<symbol> [ </symbol>
             |<expression>
             |${expression.toXml}
             |</expression>
             |<symbol> ] </symbol>
             |</term>
             |""".stripMargin
      }

      final case class UnaryOp(value: String)

      final case class UnaryOpApply(op: UnaryOp, term: Expression) extends Expression {
        override def toXml: String =
          s"""<term>
             |<symbol> ${op.value} </symbol>
             |${term.toXml}
             |</term>
             |""".stripMargin
      }

      final case class IntegerConstant(value: Int) extends Expression {
        override def toXml: String = {
          s"""<term>
             |<integerConstant> $value </integerConstant>
             |</term>
             |""".stripMargin
        }
      }

      final case class StringConstant(value: String) extends Expression {
        override def toXml: String = {
          s"""<term>
              <stringConstant> $value </stringConstant>
              </term>
             """.stripMargin
        }
      }

      sealed trait SubroutineCall extends Expression

      object SubroutineCall {

        final case class PlainSubroutineCall(
          subroutineName: Token.Identifier,
          expressionsList: Seq[Expression]
        ) extends SubroutineCall {
          override def toXml: String =
            s"""<term>
               |${subroutineName.toXml}
               |<symbol> ( </symbol>
               |<expressionList>
               |${expressionsList
                 .map(_.toXml)
                 .map(e => s"<expression> $e </expression>")
                 .mkString(s"${System.lineSeparator()}<symbol> , </symbol>")}
               |</expressionList>
               |<symbol> ) </symbol>
               |</term>
               |""".stripMargin
        }

        final case class ClassSubroutineCall(
          name: Token.Identifier,
          plainSubroutineCall: PlainSubroutineCall
        ) extends SubroutineCall {
          override def toXml: String = {
            s"""<term>
               |${name.toXml}
               |<symbol> . </symbol>
               |${plainSubroutineCall.subroutineName.toXml}
               |<symbol> ( </symbol>
               |<expressionList>
               |${plainSubroutineCall.expressionsList
                 .map(_.toXml)
                 .map(e => s"<expression> $e </expression>")
                 .mkString(s"${System.lineSeparator()}<symbol> , </symbol>")}
               |</expressionList>
               |<symbol> ) </symbol>
               |</term>
               |""".stripMargin
          }
        }
      }
    }
  }
}
