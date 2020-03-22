package assembler

import fastparse._
import NoWhitespace._

object Parser {
  sealed trait Expression
  sealed trait AddressInstruction extends Expression
  final case class Constant(value: Int) extends AddressInstruction
  final case class Reference(name: String) extends AddressInstruction
  final case class CInstruction(dest: Option[String], comp: String, jump: Option[String]) extends Expression
  final case class Label(name: String) extends Expression

  private def alpha[_: P] = P(CharIn("a-zA-Z"))
  private def underscoreOrDash[_: P] = P("_" | "-")
  private def number[_: P] = P(CharIn("0-9"))
  private def alphaNumeric[_: P] = P(alpha | number)
  private def aInstruction[_: P]: P[AddressInstruction] = P("@" ~ (alphaNumeric | underscoreOrDash).rep.!).map{ value =>
    value.toIntOption.map(Constant).getOrElse(Reference(value))
  }.log

  private def dest[_: P] = P(CharIn("ADM").rep.! ~ "=").log
  // I don't understand why it is not working without `!"("
  // somehow `comp` parser succeeds
  private def comp[_: P] = P(!"(" ~ (CharIn("ADM", "10+!&|") | "-").rep.!).log
  private def jmp[_: P] = P(";" ~ CharIn("JGTEQLNMP").rep.!).log
  private def cInstruction[_: P] =
    P(dest.? ~ comp ~ jmp.?).map {
      case (dest, comp, jmp) => CInstruction(dest, comp, jmp)
    }.log
  private def label[_: P] = P("(" ~ (alpha | underscoreOrDash).rep.! ~ ")").map(Label).log
  private def lineSeparator[_: P] = P(System.lineSeparator())
  private def expr[_: P] = P(aInstruction | cInstruction | label).log
  // here separator should  be done only with `....rep(sep = lineSeparator)`
  // direct adding on `~ (lineSeparator | End)` doesn't work
  private def exprList[_: P] = P(expr.rep(sep = lineSeparator) ~ End)
  private def onFailure(x: String, y: Int, z: Parsed.Extra) = Left((x, y, z))
  private def onSuccess(exp: Seq[Expression], index: Int) = Right((index, exp))

  type FailureString = String
  type Index = Int
  def parseRaw(raw: String): Either[(FailureString, Index, Parsed.Extra), (Index, Seq[Expression])] = {
    parse(raw, exprList(_)).fold(onFailure, onSuccess)
  }
}
