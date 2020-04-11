package hack.assembler

import hack.assembler.LabelsResolver.LabelsResolverResult

object ReferenceResolver {
  sealed trait Expression
  final case class AddressConstant(value: Int) extends Expression
  final case class Instruction(dest: Option[String], comp: String, jump: Option[String]) extends Expression

  private def convert(expr: LabelsResolver.Expression): Expression = {
    expr match {
      case LabelsResolver.Constant(value) => AddressConstant(value)
      case LabelsResolver.CInstruction(dest, comp, jump) => Instruction(dest, comp, jump)
      case LabelsResolver.Reference(name) => throw new RuntimeException(s"Reference $name encountered during references elimination. Reference should have been removed before this function call")
    }
  }

  private def resolveReferences(state: (Map[Label, Address], List[Expression], Int), currentExpression: LabelsResolver.Expression) = {
    val (context, expressions, freeAddress) = state
    currentExpression match {
      case LabelsResolver.Reference(name) =>
        context.get(name) match {
          case Some(value) =>
            val constant = AddressConstant(value)
            (context, constant :: expressions, freeAddress)
          case None =>
            val constant = AddressConstant(freeAddress)
            val nextFreeAddress = freeAddress + 1
            (context + ((name -> freeAddress)), constant :: expressions, nextFreeAddress)
        }
      case other =>
        val expression = convert(other)
        (context, expression :: expressions, freeAddress)
    }
  }

  def resolveReferences(labelsResolverResult: LabelsResolverResult): Seq[Expression] = {
    val context = labelsResolverResult.context
    val expressions = labelsResolverResult.expressions
    val freeAddress = 16
    val (_, expressionsWithoutReferences, _) = expressions.foldLeft((context, List.empty[Expression], freeAddress))(resolveReferences)
    expressionsWithoutReferences.reverse
  }
}
