package assembler

object LabelsResolver {
  sealed trait Expression
  final case class Constant(value: Int) extends Expression
  final case class Reference(name: String) extends Expression
  final case class CInstruction(dest: Option[String], comp: String, jump: Option[String]) extends Expression

  final case class LabelsResolverResult(expressions: Seq[Expression], context: Map[Label, Address])

  private def convertExpression(expr: Parser.Expression): Expression = {
    expr match {
      case Parser.Constant(value) => Constant(value)
      case Parser.Reference(name) => Reference(name)
      case Parser.CInstruction(dest, comp, jump) => CInstruction(dest, comp, jump)
      case Parser.Label(name) => throw new RuntimeException(s"Label $name encountered during labels elimination. Label should have been removed before this function call")
    }
  }

  private def resolveLabels(state: (Map[Label, Address], List[Expression], Int), currentExpression: Parser.Expression): (Map[Label, Address], List[Expression], Int) = {
    val (context, expressions, currentInstruction) = state
    val nextInstruction = currentInstruction + 1
    currentExpression match {
      case Parser.Label(name) =>
        val newContext = context + (name -> currentInstruction)
        (newContext, expressions, currentInstruction)
      case other =>
        val expr = convertExpression(other)
        (context, expr :: expressions, nextInstruction)
    }
  }

  private val defaultContext: Map[Label, Address] = Map(
    "R0" -> 0,
    "R1" -> 1,
    "R2" -> 2,
    "R3" -> 3,
    "R4" -> 4,
    "R5" -> 5,
    "R6" -> 6,
    "R7" -> 7,
    "R8" -> 8,
    "R9" -> 9,
    "R10" -> 10,
    "R11" -> 11,
    "R12" -> 12,
    "R13" -> 13,
    "R14" -> 14,
  )

  def resolveLabels(expressions: Seq[Parser.Expression]): LabelsResolverResult = {
    val (context, result, _) = expressions.toList
      .foldLeft((defaultContext, List.empty[Expression], 0))(resolveLabels)
    LabelsResolverResult(result.reverse, context)
  }
}
