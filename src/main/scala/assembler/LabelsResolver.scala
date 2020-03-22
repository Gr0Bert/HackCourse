package assembler

object LabelsResolver {
  sealed trait Expression
  final case class Constant(value: Int) extends Expression
  final case class Reference(name: String) extends Expression
  final case class CInstruction(dest: Option[String], comp: String, jump: Option[String]) extends Expression

  type Label = String
  type Address = Int
  final case class LabelsResolverResult(expressions: Seq[Expression], context: Map[Label, Address])

  private def convertExpression(expr: Parser.Expression): Expression = {
    expr match {
      case Parser.Constant(value) => Constant(value)
      case Parser.Reference(name) => Reference(name)
      case Parser.CInstruction(dest, comp, jump) => CInstruction(dest, comp, jump)
      case Parser.Label(name) => throw new RuntimeException(s"Label $name encountered during labels elimination. Label should have been removed before this function call")
    }
  }
  private def combine(state: (Map[Label, Address], List[Expression], Int), currentExpression: Parser.Expression): (Map[Label, Address], List[Expression], Int) = {
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
  def resolveLabels(expressions: Seq[Parser.Expression]): LabelsResolverResult = {
    val (context, result, _) = expressions.toList
      .foldLeft((Map.empty[Label, Address], List.empty[Expression], 0))(combine)
    LabelsResolverResult(result.reverse, context)
  }
}
