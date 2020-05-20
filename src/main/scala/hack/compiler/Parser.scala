package hack.compiler

import fastparse._

object Parser {
  import NoWhitespace._
  import Compiler._

  private def comment[_: P] = P(("//" ~ CharsWhile(c => (c != '\r' && c != '\n')).rep)) //.log
  private def tabOrSpace[_: P] = P(" " | "\t") //.log
  private def lineSeparator[_: P] = P(System.lineSeparator()) //.log

  private def ss[_: P] = (tabOrSpace | lineSeparator | comment).rep.?
  private def s[_: P] = tabOrSpace.rep.?

  private def alpha[_: P] = P(CharIn("a-zA-Z"))
  private def underscore[_: P] = P("_")
  private def number[_: P] = P(CharIn("0-9"))

  private def identifier[_: P] =
    P((!number) ~ ((alpha | number | underscore).rep(1)).!).map(Token.Identifier)

  object StructureParsers {

    private def typeDeclaration[_: P] = {
      def primitiveTypeDeclaration =
        StringIn("int", "char", "boolean", "void").!.map(Structure.Type.Primitive)
      def userTypeDeclaration = identifier.map(Structure.Type.UserDefined)
      P(primitiveTypeDeclaration | userTypeDeclaration)
    }

    private def varDeclaration[_: P] =
      P("var" ~ " " ~/ typeDeclaration ~ " " ~/ identifier.rep(sep = P("," ~ " ".?)) ~ ";").map{
        case (t, names) => Structure.VarDec(t, names)
      }.log

    private def classVarDeclaration[_: P] =
      P(StringIn("static", "field").! ~ " " ~ varDeclaration).map{
        case (modifier, varDec) => Structure.ClassVarDec(Token.Keyword(modifier), varDec)
      }

    private def parameterList[_: P] =
      P("(" ~ (typeDeclaration ~ " " ~ identifier).rep(sep = P("," ~ " ".?)) ~ ")").map { params =>
        params.map{ case (t, n) => Structure.SubroutineParameter(t, n) }
      }

    private def subroutineDeclaration[_: P] =
      P(
        StringIn("constructor", "function", "method").! ~ s ~
          typeDeclaration ~ s ~
          identifier ~ parameterList ~ ss ~
          "{" ~ ss ~ varDeclaration.rep(sep = ss) ~ ss ~ StatementParsers.statements ~ ss ~ "}").map {
        case (modifier, t, n, params, vars, body) =>
          Structure.SubroutineDec(Token.Keyword(modifier), t, n, params, vars, body)
      }

    def classDeclaration[_: P] =
      P(
        "class" ~ s ~ identifier ~ ss ~
          "{" ~ ss ~ classVarDeclaration.rep(sep = ss) ~ ss ~ subroutineDeclaration.rep(sep = ss) ~ ss ~ "}"
      ).map {
        case (name, vars, funcs) => Structure.ClassStruct(name, vars, funcs)
      }
  }

  object StatementParsers {
    private def codeBlock[_: P] = P("{" ~ ss ~ statements ~ ss ~ "}")

    private def ifStatement[_: P] =
      P(ss ~ "if" ~ ss ~ "(" ~ ExpressionParsers.expression ~ ")" ~ ss ~ codeBlock ~ ss ~ "else" ~ ss ~ codeBlock).map {
        case (exp, t, f) => Statement.If(exp, t, f)
      }.log

    private def letStatement[_: P] =
      P("let" ~ s ~ identifier ~ ("[" ~ ExpressionParsers.expression ~ "]").? ~ s ~ "=" ~ s ~ ExpressionParsers.expression ~ ";").map {
        case (name, accessOpt, exp) => Statement.Let(name, accessOpt, exp)
      }

    private def whileStatement[_: P] =
      P("while" ~ s ~ "(" ~ s ~ ExpressionParsers.expression ~ s ~ ")" ~ ss ~ codeBlock).map {
        case (e, s) => Statement.While(e, s)
      }.log

    private def doStatement[_: P] = P("do" ~ s ~ ExpressionParsers.subroutineCall ~ s ~ ";").map(Statement.Do)

    private def returnStatement[_: P] = P("return" ~ s ~ ExpressionParsers.expression.? ~ s ~ ";").map(Statement.Return).log

    def statement[_: P]: P[Statement] = (ifStatement | letStatement | whileStatement | doStatement | returnStatement)

    def statements[_: P]: P[Seq[Statement]] = statement.rep(sep = ss)
  }


  object ExpressionParsers {

    private def expressionList[_: P] = expression.rep(sep = (s.? ~ "," ~ s.?))

    private def keywordConstant[_: P]: P[Expression.KeywordConstant] = StringIn("true", "false", "null", "this").!.map(Expression.KeywordConstant)

    private def op[_: P] = StringIn("+", "-", "*", "/", "&", "|", "<", ">", "=").!.map(Expression.Op)

    private def unaryOp[_: P] = StringIn("-", "~").!.map(Expression.UnaryOp)

    private def varName[_: P]: P[Expression.VarName] = identifier.map(Expression.VarName)

    private def varAccess[_: P]: P[Expression.VarAccess] = P(varName ~ "[" ~ s ~ expression ~ s ~"]").map {
      case (name, exp) => Expression.VarAccess(name, exp)
    }

    def stringConstant[_: P]: P[Expression.StringConstant] =
      P("\"" ~ CharsWhile(c => c != '"').!).map(Expression.StringConstant).log

    private def integerConstant[_: P]: P[Expression.IntegerConstant] =
      P(number.rep(min = 1, max = 5).!).map(_.toInt).map(Expression.IntegerConstant)

    def subroutineCall[_: P]: P[Expression.SubroutineCall] =
      P((identifier ~ ".").? ~ identifier ~ "(" ~ s ~ expressionList ~ s ~ ")").map {
        case (classNameOpt, subName, args) => classNameOpt match {
          case Some(clName) =>
            val sub = Expression.SubroutineCall.PlainSubroutineCall(subName, args)
            Expression.SubroutineCall.ClassSubroutineCall(clName, sub)
          case None =>
            Expression.SubroutineCall.PlainSubroutineCall(subName, args)
        }
      }

    private def expressionInBraces[_: P]: P[Expression] = P("(" ~ s ~ expression ~ s ~ ")")

    private def binaryOp[_: P]: P[Expression.BinaryOp] = (expression ~ s ~ op ~ s ~ expression).map {
      case (f, o, s) => Expression.BinaryOp(f, o, s)
    }

    private def unaryOpApply[_: P] = (unaryOp ~ s ~ expression).map {
      case (o, t) => Expression.UnaryOpApply(o, t)
    }

    def expression[_: P]: P[Expression] =
      (
        stringConstant |
        integerConstant |
        keywordConstant |
        varName |
        varAccess |
        subroutineCall |
        expressionInBraces |
        unaryOpApply
  //      binaryOp // buggy
        )
  }


  private def parser[_: P] = P((ss ~ (StructureParsers.classDeclaration | StatementParsers.statement | ExpressionParsers.expression))).log

  private def onFailure(x: String, y: Int, z: Parsed.Extra) = Left((x, y, z))
  private def onSuccess(exp: AST, index: Int) = Right((index, exp))

  type FailureString = String
  type Index = Int
  def parseRaw(raw: String): Either[(FailureString, Index, Parsed.Extra), (Index, AST)] = {
    parse(raw, parser(_)).fold(onFailure, onSuccess)
  }
}

object ParserTest extends App {
//  assert(Parser.parseRaw("var int test,test1,test2;").isRight, "var dec is broken")
//  assert(Parser.parseRaw("static var int test,test1,test2;").isRight, "var dec is broken")
//  assert(Parser.parseRaw("function int test(int t1, boolean t2)").isRight, "subroutine dec is broken")
//  println {
//    Parser.parseRaw {
//      """   if () {
//        |let test[] = ; // some comment goes here
//        |} else {while () {
//        |  let test[] = ;
//        |  do testFunc();
//        |  return;
//        |}}
//        |let test[] = ;""".stripMargin
//    }
//  }
  println {
    Parser.parseRaw {
      """return "asc";"""
    }
  }
}
