package hack.vm

import fastparse._
import StackMachine._

object Parser {
  import NoWhitespace._

  private def number[_: P] = P(CharIn("0-9"))

  object ArithmeticParser {
    def add[_: P] = P("add").map(_ => Arithmetic.Add)
    def sub[_: P] = P("sub").map(_ => Arithmetic.Sub)
    def neg[_: P] = P("neg").map(_ => Arithmetic.Neg)
    def eq[_: P] = P("eq").map(_ => Arithmetic.Eq)
    def gt[_: P] = P("gt").map(_ => Arithmetic.Gt)
    def lt[_: P] = P("lt").map(_ => Arithmetic.Lt)
    def and[_: P] = P("and").map(_ => Arithmetic.And)
    def or[_: P] = P("or").map(_ => Arithmetic.Or)
    def not[_: P] = P("not").map(_ => Arithmetic.Not)
    def arithmetic[_: P] = P(add | sub | neg | eq | gt | lt | and | or | not)
  }

  object MemoryAccessParser {
    import MemoryAccess._
    object SegmentParser {
      def constant[_: P] = P("constant").map(_ => Segment.Constant)
      def local[_: P] = P("local").map(_ => Segment.Local)
      def argument[_: P] = P("argument").map(_ => Segment.Argument)
      def `this`[_: P] = P("this").map(_ => Segment.This)
      def that[_: P] = P("that").map(_ => Segment.That)
      def static[_: P] = P("static").map(_ => Segment.Static)
      def temp[_: P] = P("temp").map(_ => Segment.Temp)
      def pointer[_: P] = P("pointer").map(_ => Segment.Pointer)
      def segment[_: P] = P(constant | local | argument | `this` | that | static | temp | pointer)
    }
    def pop[_: P] = P("pop" ~ " " ~ SegmentParser.segment ~ " " ~ number.rep.!).map{
      case (segment, address) => Pop(segment, address.toInt)
    }
    def push[_: P] = P("push" ~ " " ~ SegmentParser.segment ~ " " ~ number.rep.!).map{
      case (segment, address) => Push(segment, address.toInt)
    }
    def memoryAccess[_: P] = P(pop | push)
  }
  private def comment[_: P] = P( ("//" ~ CharsWhile(c => (c != '\r' && c != '\n')).rep))//.log
  private def tabOrSpace[_: P] = P(" " | "\t")//.log
  private def lineSeparator[_: P] = P(System.lineSeparator())//.log

  private def expr[_: P] = P( (MemoryAccessParser.memoryAccess | ArithmeticParser.arithmetic) ~ tabOrSpace.rep ~ comment.?)//.log
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
