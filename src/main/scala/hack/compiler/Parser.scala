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
    P(!number ~ (alpha | number | underscore).rep.!).map(Token.Identifier)
  private def stringConstant[_: P]: P[Term] =
    P("\"" ~ AnyChar.rep.! ~ "\"").map(Token.StringConstant)
  private def integerConstant[_: P]: P[Term] =
    P(number.rep(min = 1, max = 5).!).map(_.toInt).map(Token.IntegerConstant)

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
    P(StringIn("constructor", "function", "method").! ~ " " ~ typeDeclaration ~ " " ~ identifier ~ parameterList).map {
      case (modifier, t, n, params) => Structure.SubroutineDec(Token.Keyword(modifier), t, n, params, Nil, Nil)
    }

  private def codeBlock[_: P] = P("{" ~ ss ~ statements ~ ss ~ "}")
  private def ifStatement[_: P] =
    P(ss ~ "if" ~ ss ~ "(" ~ expression ~ ")" ~ ss ~ codeBlock ~ ss ~ "else" ~ ss ~ codeBlock).map {
      case (exp, t, f) => Statement.If(exp, t, f)
    }.log

  private def letStatement[_: P] =
    P("let" ~ s ~ identifier ~ ("[" ~ expression ~ "]").? ~ s ~ "=" ~ s ~ expression ~ ";").map {
      case (name, accessOpt, exp) => Statement.Let(name, accessOpt, exp)
    }

  private def whileStatement[_: P] =
    P("while" ~ s ~ "(" ~ s ~ expression ~ s ~ ")" ~ ss ~ codeBlock).map {
      case (e, s) => Statement.While(e, s)
    }.log

  private def doStatement[_: P] = P("do" ~ s ~ subroutineCall ~ s ~ ";").map(Statement.Do)

  private def returnStatement[_: P] = P("return" ~ s ~ expression.? ~ s ~ ";").map(Statement.Return)

  private def expressionList[_: P] = expression.rep(sep = (s.? ~ "," ~ s.?))

  private def keywordConstant[_: P]: P[Term] = StringIn("true", "false", "null", "this").!.map(Expression.KeywordConstant)

  private def op[_: P] = StringIn("+", "-", "*", "/", "&", "|", "<", ">", "=").!.map(Expression.Op)

  private def unaryOp[_: P] = StringIn("-", "~").!.map(Expression.UnaryOp)

  private def varName[_: P]: P[Expression.VarName] = identifier.map(Expression.VarName)
  private def varAccess[_: P]: P[Expression.VarAccess] = P(varName ~ "[" ~ s ~ expression ~ s ~"]").map {
    case (name, exp) => Expression.VarAccess(name, exp)
  }

  private def subroutineCall[_: P]: P[Expression.SubroutineCall] =
    P((identifier ~ ".").? ~ identifier ~ "(" ~ s ~ expressionList ~ s ~ ")").map {
      case (classNameOpt, subName, args) => classNameOpt match {
        case Some(clName) =>
          val sub = Expression.SubroutineCall.PlainSubroutineCall(subName, args)
          Expression.SubroutineCall.ClassSubroutineCall(clName, sub)
        case None =>
          Expression.SubroutineCall.PlainSubroutineCall(subName, args)
      }
    }

  private def expressionInBraces[_: P]: P[Term] = P("(" ~ s ~ expression ~ s ~ ")")

  private def expression[_: P]: P[Expression.ExpressionDec] = (term ~ (op ~ s ~ term ~ s).rep).map {
    case (t, other) => Expression.ExpressionDec(t, other)
  }

  private def unaryOpApply[_: P] = (unaryOp ~ s ~ term).map {
    case (o, t) => Expression.UnaryOpApply(o, t)
  }

  private def term[_: P]: P[Term] =
    (integerConstant | stringConstant | keywordConstant | varName | varAccess | subroutineCall | expressionInBraces | unaryOpApply)

  private def statement[_: P]: P[Statement] = (ifStatement | letStatement | whileStatement | doStatement | returnStatement)
  private def statements[_: P]: P[Seq[Statement]] = statement.rep(sep = ss)

  private def parser[_: P] = P((ss ~ (statement | term)).rep(sep = lineSeparator) ~ End) //.log

  private def onFailure(x: String, y: Int, z: Parsed.Extra) = Left((x, y, z))
  private def onSuccess(exp: Seq[AST], index: Int) = Right((index, exp))

  type FailureString = String
  type Index = Int
  def parseRaw(raw: String): Either[(FailureString, Index, Parsed.Extra), (Index, Seq[AST])] = {
    parse(raw, parser(_)).fold(onFailure, onSuccess)
  }
}

object ParserTest extends App {
//  assert(Parser.parseRaw("var int test,test1,test2;").isRight, "var dec is broken")
//  assert(Parser.parseRaw("static var int test,test1,test2;").isRight, "var dec is broken")
//  assert(Parser.parseRaw("function int test(int t1, boolean t2)").isRight, "subroutine dec is broken")
  println {
    Parser.parseRaw {
      """   if () {
        |let test[] = ; // some comment goes here
        |} else {while () {
        |  let test[] = ;
        |  do ;
        |  return;
        |}}
        |let test[] = ;""".stripMargin
    }
  }
//  println {
//    Parser.parseRaw {
//      """
//        |  return ;""".stripMargin
//    }
//  }
}
