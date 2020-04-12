package hack

package object assembler {
  type Label = String
  type Address = Int

  object Assembly {
    sealed trait Expression
    sealed trait AddressInstruction extends Expression
    final case class Constant(value: Int) extends AddressInstruction {
      override def toString: assembler.Label = s"@$value"
    }
    final case class Reference(name: String) extends AddressInstruction {
      override def toString: assembler.Label = s"@$name"
    }
    final case class CInstruction(dest: Option[String], comp: String, jump: Option[String]) extends Expression {
      override def toString: assembler.Label = s"${dest.getOrElse("")}=${comp}${jump.map(j => s";$j").getOrElse("")}"
    }
    final case class Label(name: String) extends Expression {
      override def toString: assembler.Label = s"($name)"
    }
  }
}
