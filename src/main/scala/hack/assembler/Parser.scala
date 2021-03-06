package hack.assembler

import fastparse._

object Parser {
  import NoWhitespace._
  import Assembly._
  
  private def alpha[_: P] = P(CharIn("a-zA-Z"))
  private def underscoreOrDash[_: P] = P("_" | "-")
  private def dot[_: P] = P(".")
  private def dollar[_: P] = P("$")
  private def number[_: P] = P(CharIn("0-9"))
  private def alphaNumeric[_: P] = P(alpha | number)
  private def aInstruction[_: P]: P[AddressInstruction] = P("@" ~ (alphaNumeric | underscoreOrDash | dot | dollar).rep.!).map{ value =>
    value.toIntOption.map(Constant).getOrElse(Reference(value))
  }//.log

  private def dest[_: P] = P(("A" | "D" | "M").rep.! ~ "=")//.log
  // I don't understand why it is not working without `!"("
  // somehow `comp` parser succeeds
  private def comp[_: P] = P(!"(" ~ (CharIn("ADM", "10+!&|") | "-").rep.!)//.log
  private def jmp[_: P] = P(";" ~ CharIn("JGTEQLNMP").rep.!)//.log
  private def cInstruction[_: P] =
    P(dest.? ~ comp ~ jmp.?).map {
      case (dest, comp, jmp) => CInstruction(dest, comp, jmp)
    }//.log
  private def label[_: P] = P("(" ~ (alphaNumeric | underscoreOrDash | dot | dollar).rep.! ~ ")").map(Label)//.log
  private def lineSeparator[_: P] = P(System.lineSeparator())//.log
  private def tabOrSpace[_: P] = P(" " | "\t")//.log
  private def comment[_: P] = P( ("//" ~ CharsWhile(c => (c != '\r' && c != '\n')).rep))//.log
  private def expr[_: P] = P( (aInstruction | cInstruction | label) ~ tabOrSpace.rep ~ comment.?)//.log
  // here separator should  be done only with `....rep(sep = lineSeparator)`
  // direct adding on `~ (lineSeparator | End)` doesn't work
  private def exprList[_: P] = P(expr.rep(sep = lineSeparator) ~ End)//.log
  private def onFailure(x: String, y: Int, z: Parsed.Extra) = Left((x, y, z))
  private def onSuccess(exp: Seq[Expression], index: Int) = Right((index, exp))

  type FailureString = String
  type Index = Int
  def parseRaw(raw: String): Either[(FailureString, Index, Parsed.Extra), (Index, Seq[Expression])] = {
    val normalisedString =
      raw
        .split(System.lineSeparator())
        .map(str => str.replace("\n", "").replace("\r", "").strip)
        .filterNot(str => str.isEmpty || str.startsWith("//") || str.isBlank)
    val normalised = normalisedString.mkString(System.lineSeparator())
    parse(normalised, exprList(_)).fold(onFailure, onSuccess)
  }
}
