package hack.vm

import fastparse._
import StackMachine._

object Parser {
  import NoWhitespace._

  private def alpha[_: P] = P(CharIn("a-zA-Z"))
  private def underscoreOrDash[_: P] = P("_" | "-")
  private def dot[_: P] = P(".")
  private def dollar[_: P] = P("$")
  private def number[_: P] = P(CharIn("0-9"))
  private def alphaNumeric[_: P] = P(alpha | number)
  private def labelText[_: P] = (alphaNumeric | underscoreOrDash | dot | dollar).rep.!

  object ArithmeticParser {
    private def add[_: P] = P("add").map(_ => Arithmetic.Add)
    private def sub[_: P] = P("sub").map(_ => Arithmetic.Sub)
    private def neg[_: P] = P("neg").map(_ => Arithmetic.Neg)
    private def eq[_: P] = P("eq").map(_ => Arithmetic.Eq)
    private def gt[_: P] = P("gt").map(_ => Arithmetic.Gt)
    private def lt[_: P] = P("lt").map(_ => Arithmetic.Lt)
    private def and[_: P] = P("and").map(_ => Arithmetic.And)
    private def or[_: P] = P("or").map(_ => Arithmetic.Or)
    private def not[_: P] = P("not").map(_ => Arithmetic.Not)
    def arithmetic[_: P] = P(add | sub | neg | eq | gt | lt | and | or | not)
  }

  object MemoryAccessParser {
    import MemoryAccess._
    private def constant[_: P] = P("constant").map(_ => Segment.Constant)
    private def local[_: P] = P("local").map(_ => Segment.Local)
    private def argument[_: P] = P("argument").map(_ => Segment.Argument)
    private def `this`[_: P] = P("this").map(_ => Segment.This)
    private def that[_: P] = P("that").map(_ => Segment.That)
    private def static[_: P] = P("static").map(_ => Segment.Static)
    private def temp[_: P] = P("temp").map(_ => Segment.Temp)
    private def pointer[_: P] = P("pointer").map(_ => Segment.Pointer)
    private def segment[_: P] = P(constant | local | argument | `this` | that | static | temp | pointer)
    private def pop[_: P] = P("pop" ~ " " ~ segment ~ " " ~ number.rep.!).map{
      case (segment, address) => Pop(segment, address.toInt)
    }
    private def push[_: P] = P("push" ~ " " ~ segment ~ " " ~ number.rep.!).map{
      case (segment, address) => Push(segment, address.toInt)
    }
    def memoryAccess[_: P] = P(pop | push)
  }

  object BranchingParser {
    import Branching._
    private def label[_: P] = P("label" ~ " " ~ labelText).map(Label)//.log
    private def goTo[_: P] = P("goto" ~ " " ~ labelText).map(GoTo)//.log
    private def ifGoTo[_: P] = P("if-goto" ~ " " ~ labelText).map(IfGoTo)//.log
    def branching[_: P] = P(label | goTo | ifGoTo)
  }

  object FunctionParser {
    import Function._
    private def definition[_: P] = P("function" ~ " " ~ labelText ~ " " ~ number.!).map {
      case (name, localArgsCount) => Def(name, localArgsCount.toInt)
    }
    private def call[_: P] = P("call" ~ " " ~ labelText ~ " " ~ number.!).map {
      case (name, argsCount) => Call(name, argsCount.toInt)
    }
    private def funcReturn[_: P] = P("return").map(_ => Return)
    def function[_: P] = P(definition | call | funcReturn)
  }

  private def comment[_: P] = P( ("//" ~ CharsWhile(c => (c != '\r' && c != '\n')).rep))//.log
  private def tabOrSpace[_: P] = P(" " | "\t")//.log
  private def lineSeparator[_: P] = P(System.lineSeparator())//.log

  private def expr[_: P] = P( (MemoryAccessParser.memoryAccess | ArithmeticParser.arithmetic | BranchingParser.branching | FunctionParser.function) ~ tabOrSpace.rep ~ comment.?)//.log
  private def exprList[_: P] = P(expr.rep(sep = lineSeparator) ~ End)//.log
  private def onFailure(x: String, y: Int, z: Parsed.Extra) = Left((x, y, z))
  private def onSuccess(exp: Seq[Command], index: Int) = Right((index, exp))

  type FailureString = String
  type Index = Int
  def parseRaw(raw: String): Either[(FailureString, Index, Parsed.Extra), (Index, Seq[Command])] = {
    val normalisedString =
      raw
        .split(System.lineSeparator())
        .map(str => str.replace("\n", "").replace("\r", "").strip)
        .filterNot(str => str.isEmpty || str.startsWith("//") || str.isBlank)
    val normalised = normalisedString.mkString(System.lineSeparator())
    parse(normalised, exprList(_)).fold(onFailure, onSuccess)
  }
}
