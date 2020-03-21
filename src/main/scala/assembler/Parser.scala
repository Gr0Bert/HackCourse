package assembler

import fastparse._, SingleLineWhitespace._

object Parser {
  sealed trait Expression
  final case class AInstruction(value: String) extends Expression
  final case class CInstruction(dest: Option[String], comp: String, jump: Option[String])
      extends Expression

  private def alpha[_: P] = P(CharIn("a-zA-Z"))
  private def number[_: P] = P(CharIn("0-9"))
  private def alphaNumeric[_: P] = P(alpha | number)
  private def aInstruction[_: P] = P("@" ~ alphaNumeric.rep.!).map(AInstruction)
  private def dest[_: P] = P(CharIn("ADM").rep.! ~ "=")
  private def comp[_: P] = P((CharIn("ADM10") | "-" | "+" | "!" | "&" | "|").rep.!)
  private def jmp[_: P] = P(";" ~ CharIn("JGTEQLNMP").rep.!)
  private def cInstruction[_: P] =
    P(dest.? ~ comp ~ jmp.?).map {
      case (dest, comp, jmp) => CInstruction(dest, comp, jmp)
    }
  private def lineSeparator[_: P] = P(System.lineSeparator())
  private def expr[_: P] = P(aInstruction | cInstruction)
  private def exprList[_: P] = P(expr.rep(sep = lineSeparator) ~ End)
  private def onFailure(x: String, y: Int, z: Parsed.Extra) = Left((x, y, z))
  private def onSuccess(exp: Seq[Expression], index: Int) = Right((index, exp))

  type FailureString = String
  type Index = Int
  def parseRaw(raw: String): Either[(FailureString, Index, Parsed.Extra), (Index, Seq[Expression])] = {
    parse(raw, exprList(_)).fold(onFailure, onSuccess)
  }
}
