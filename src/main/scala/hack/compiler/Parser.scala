package hack.compiler

import fastparse._

import scala.language.postfixOps

object Parser {
  import NoWhitespace._
  import Compiler._

  def singleLineComment[_: P] = P(("//" ~ CharsWhile(c => (c != '\r' && c != '\n')).rep))
  def toStar[_: P] = CharsWhile(c => c != '*')
  def toMultilineEnd[_: P]: P[Unit] = P( toStar ~ "*" ~ ("/" | toMultilineEnd) )
  def multilineComment[_: P] = P("/" ~ "*".rep(1) ~ toMultilineEnd)
  def comment[_: P] = singleLineComment | multilineComment

  private def tabOrSpace[_: P] = P(" " | "\t") //
  private def lineSeparator[_: P] = P(System.lineSeparator()) //

  private def ss[_: P] = (tabOrSpace | lineSeparator | comment).rep.?
  private def s[_: P] = tabOrSpace.rep.?
  private def sm[_: P] = tabOrSpace.rep(1)

  private def alpha[_: P] = P(CharIn("a-zA-Z"))
  private def underscore[_: P] = P("_")
  private def number[_: P] = P(CharIn("0-9"))

  private def keywords[_: P] = List(
    "let",
    "class",
    "constructor",
    "function",
    "method",
    "field",
    "static",
    "var",
    "int",
    "char",
    "boolean",
    "void",
    "true",
    "false",
    "null",
    "this",
    "do",
    "if",
    "else",
    "while",
    "return"
  )

  private def identifier[_: P] =
    P((!number) ~ ((alpha | number | underscore).rep(1)).!)
      .filter(parsed => !keywords.contains(parsed))
      .map(Token.Identifier)

  object StructureParsers {

    private def typeDeclaration[_: P] = {
      def primitiveTypeDeclaration =
        StringIn("int", "char", "boolean", "void").!.map(Structure.Type.Primitive)
      def userTypeDeclaration = identifier.map(Structure.Type.UserDefined)
      P(primitiveTypeDeclaration | userTypeDeclaration)
    }

    private def varDeclaration[_: P] =
      P("var" ~ s ~ typeDeclaration ~ s ~ identifier.rep(sep = P("," ~ s)) ~ s ~ ";").map {
        case (t, names) => Structure.VarDec(t, names)
      }

    private def classVarDeclaration[_: P] =
      P(
        StringIn("static", "field").! ~ s ~ typeDeclaration ~ s ~ identifier
          .rep(sep = P("," ~ s)) ~ s ~ ";"
      ).map {
        case (modifier, tp, names) =>
          Structure.ClassVarDec(Token.Keyword(modifier), Structure.VarDec(tp, names))
      }

    private def parameterList[_: P] =
      P("(" ~ (typeDeclaration ~ " " ~ identifier).rep(sep = P("," ~ " ".?)) ~ ")").map { params =>
        params.map { case (t, n) => Structure.SubroutineParameter(t, n) }
      }

    private def subroutineDeclaration[_: P] =
      P(
        ss ~ StringIn("constructor", "function", "method").! ~ s ~
          typeDeclaration ~ s ~
          identifier ~ parameterList ~ ss ~
          "{" ~ ss ~ varDeclaration.rep(sep = ss) ~ ss ~ StatementParsers.statements ~ ss ~ "}"
      ).map {
        case (modifier, t, n, params, vars, body) =>
          Structure.SubroutineDec(Token.Keyword(modifier), t, n, params, vars, body)
      }

    def classDeclaration[_: P] =
      P(
        ss ~ "class" ~ s ~ identifier ~ ss ~
          "{" ~ ss ~ classVarDeclaration.rep(sep = ss) ~ ss ~ subroutineDeclaration
          .rep(sep = ss) ~ ss ~ "}"
      ).map {
        case (name, vars, funcs) => Structure.ClassStruct(name, vars, funcs)
      }
  }

  object StatementParsers {
    private def codeBlock[_: P] = P("{" ~ ss ~ statements ~ ss ~ "}")

    private def ifStatement[_: P] =
      P(
        "if" ~ s ~ "(" ~ s ~ ExpressionParsers.expression ~ s ~ ")" ~ ss ~ codeBlock ~ (ss ~ "else" ~ ss ~ codeBlock).?
      ).map {
        case (exp, t, f) => Statement.If(exp, t, f)
      }

    private def letStatement[_: P] =
      P(
        "let" ~ s ~ identifier ~ ("[" ~ ExpressionParsers.expression ~ "]").? ~ s ~ "=" ~ s ~ ExpressionParsers.expression ~ ";"
      ).map {
        case (name, accessOpt, exp) => Statement.Let(name, accessOpt, exp)
      }

    private def whileStatement[_: P] =
      P("while" ~ s ~ "(" ~ s ~ ExpressionParsers.expression ~ s ~ ")" ~ ss ~ codeBlock).map {
        case (e, s) => Statement.While(e, s)
      }

    private def doStatement[_: P] =
      P("do" ~ s ~ ExpressionParsers.subroutineCall ~ s ~ ";").map(Statement.Do)

    private def returnStatement[_: P] =
      P("return" ~ s ~ ExpressionParsers.expression.? ~ s ~ ";").map(Statement.Return)

    def statement[_: P]: P[Statement] =
      (ss ~ (ifStatement | letStatement | whileStatement | doStatement | returnStatement) ~ ss)

    def statements[_: P]: P[Seq[Statement]] = statement.rep(sep = ss)
  }

  object ExpressionParsers {

    private def expressionList[_: P] = expression.rep(sep = (s.? ~ "," ~ s.?))

    private def keywordConstant[_: P]: P[Expression.KeywordConstant] =
      StringIn("true", "false", "null", "this").!.map(Expression.KeywordConstant)

    private def unaryOp[_: P] = StringIn("-", "~").!.map(Expression.UnaryOp)

    private def varName[_: P]: P[Expression.VarName] = identifier.map(Expression.VarName)

    private def varAccess[_: P]: P[Expression.VarAccess] =
      P(varName ~ "[" ~ s ~ expression ~ s ~ "]").map {
        case (name, exp) => Expression.VarAccess(name, exp)
      }

    private def stringConstant[_: P]: P[Expression.StringConstant] =
      P("\"" ~ CharsWhile(c => c != '"').! ~ "\"").map(Expression.StringConstant)

    private def integerConstant[_: P]: P[Expression.IntegerConstant] =
      P(number.rep(min = 1, max = 5).!).map(_.toInt).map(Expression.IntegerConstant)

      def subroutineCall[_: P]: P[Expression.SubroutineCall] =
      P((identifier ~ ".").? ~ identifier ~ "(" ~ s ~ expressionList ~ s ~ ")").map {
        case (classNameOpt, subName, args) =>
          classNameOpt match {
            case Some(clName) =>
              val sub = Expression.SubroutineCall.PlainSubroutineCall(subName, args)
              Expression.SubroutineCall.ClassSubroutineCall(clName, sub)
            case None =>
              Expression.SubroutineCall.PlainSubroutineCall(subName, args)
          }
      }

    private def expressionInBraces[_: P]: P[Expression] = P("(" ~ s ~ expression ~ s ~ ")").map(Expression.Braces)

    private def binaryOp[_: P]: P[Expression.Op] =
      StringIn("+", "-", "*", "/", "&", "|", "<", ">", "=").!.map(Expression.Op)

    private def unaryOpApply[_: P] = (unaryOp ~ s ~ expression).map {
      case (o, t) => Expression.UnaryOpApply(o, t)
    }

    private def atom[_: P] = {
      (
      subroutineCall
        | varAccess
        | unaryOpApply
        | keywordConstant
        | integerConstant
        |stringConstant
        | varName
        | expressionInBraces
      )
    }

    def expression[_: P]: P[Expression] = P( atom ~ (s ~ binaryOp ~ s ~ atom).rep).map{
      case (lhs, chunks) =>
        chunks.foldLeft(lhs) {
          case (lhs, (op, rhs)) => Expression.BinaryOp(lhs, op, rhs)
        }
    }
  }

  private def parser[_: P] = P(
    (ss ~ (StructureParsers.classDeclaration | StatementParsers.statement | ExpressionParsers.expression))
  )

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
//      """   if (true) {
//        |let test = 1; // some comment goes here
//        |} else {while (true) {
//        |  let test[7] = 5;
//        |  do testFunc();
//        |  return;
//        |}}""".stripMargin
//    }
//  }
    Parser.parseRaw {
      """// This file is part of www.nand2tetris.org
        |// and the book "The Elements of Computing Systems"
        |// by Nisan and Schocken, MIT Press.
        |// File name: projects/10/Square/SquareGame.jack
        |
        |// (same as projects/09/Square/SquareGame.jack)
        |
        |/**
        | * Implements the Square Dance game.
        | * This simple game allows the user to move a black square around
        | * the screen, and change the square's size during the movement.
        | * When the game starts, a square of 30 by 30 pixels is shown at the
        | * top-left corner of the screen. The user controls the square as follows.
        | * The 4 arrow keys are used to move the square up, down, left, and right.
        | * The 'z' and 'x' keys are used, respectively, to decrement and increment
        | * the square's size. The 'q' key is used to quit the game.
        | */
        |
        |class SquareGame {
        |   field Square square; // the square of this game
        |   field int direction; // the square's current direction:
        |                        // 0=none, 1=up, 2=down, 3=left, 4=right
        |
        |   /** Constructs a new Square Game. */
        |   constructor SquareGame new() {
        |      // Creates a 30 by 30 pixels square and positions it at the top-left
        |      // of the screen.
        |      let square = Square.new(0, 0, 30);
        |      let direction = 0;  // initial state is no movement
        |      return this;
        |   }
        |
        |   /** Disposes this game. */
        |   method void dispose() {
        |      do square.dispose();
        |      do Memory.deAlloc(this);
        |      return;
        |   }
        |
        |   /** Moves the square in the current direction. */
        |   method void moveSquare() {
        |      if (direction = 1) { do square.moveUp(); }
        |      if (direction = 2) { do square.moveDown(); }
        |      if (direction = 3) { do square.moveLeft(); }
        |      if (direction = 4) { do square.moveRight(); }
        |      do Sys.wait(5);  // delays the next movement
        |      return;
        |   }
        |
        |   /** Runs the game: handles the user's inputs and moves the square accordingly */
        |   method void run() {
        |      var char key;  // the key currently pressed by the user
        |      var boolean exit;
        |      let exit = false;
        |
        |      while (~exit) {
        |         // waits for a key to be pressed
        |         while (key = 0) {
        |            let key = Keyboard.keyPressed();
        |            do moveSquare();
        |         }
        |         if (key = 81)  { let exit = true; }     // q key
        |         if (key = 90)  { do square.decSize(); } // z key
        |         if (key = 88)  { do square.incSize(); } // x key
        |         if (key = 131) { let direction = 1; }   // up arrow
        |         if (key = 133) { let direction = 2; }   // down arrow
        |         if (key = 130) { let direction = 3; }   // left arrow
        |         if (key = 132) { let direction = 4; }   // right arrow
        |
        |         // waits for the key to be released
        |         while (~(key = 0)) {
        |            let key = Keyboard.keyPressed();
        |            do moveSquare();
        |         }
        |     } // while
        |     return;
        |   }
        |}
        |
        |
        |
        |""".stripMargin
    } match {
      case Left(value) =>
        println(value._3.trace(true))
      case Right(value) =>
        println(value)
    }
//  println {
//    Parser.parseRaw {
//      """/* asdasd
//        |* sadasd
//        |*/
//        |let square = new(0, 0, 30);""".stripMargin
//    }
//  }
}
