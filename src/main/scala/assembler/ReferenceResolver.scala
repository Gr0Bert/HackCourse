package assembler

object ReferenceResolver {
  sealed trait Expression
  final case class AddressConstant(value: Int) extends Expression
  final case class Instruction(dest: Option[String], comp: String, jump: Option[String]) extends Expression
}
