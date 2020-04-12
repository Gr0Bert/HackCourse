package hack

package object assembler {
  type Label = String
  type Address = Int

  object Assembly {
    sealed trait Expression
    sealed trait AddressInstruction extends Expression
    final case class Constant(value: Int) extends AddressInstruction
    final case class Reference(name: String) extends AddressInstruction
    final case class CInstruction(dest: Option[String], comp: String, jump: Option[String]) extends Expression
    final case class Label(name: String) extends Expression
  }
}
