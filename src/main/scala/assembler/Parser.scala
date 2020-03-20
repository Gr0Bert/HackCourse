package assembler

import fastparse._, NoWhitespace._

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
  private def expr[_: P] = P(aInstruction | cInstruction)

  private def onFailure(x: String, y: Int, z: Parsed.Extra) = Left((x, y, z))
  private def onSuccess(exp: Expression, index: Int) = Right((index, exp))

  def parseRaw(raw: String): Either[(String, Int, Parsed.Extra), (Int, Expression)] = {
    parse(raw, expr(_)).fold(onFailure, onSuccess)
  }
}
