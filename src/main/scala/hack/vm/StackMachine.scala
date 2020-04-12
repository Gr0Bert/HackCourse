package hack.vm

object StackMachine extends App {
  sealed trait Command

  sealed trait Arithmetic extends Command
  object Arithmetic {
    final case object Add extends Arithmetic
    final case object Sub extends Arithmetic
    final case object Neg extends Arithmetic
    final case object Eq extends Arithmetic
    final case object Gt extends Arithmetic
    final case object Lt extends Arithmetic
    final case object And extends Arithmetic
    final case object Or extends Arithmetic
    final case object Not extends Arithmetic
  }

  sealed trait MemoryAccess extends Command
  object MemoryAccess {
    sealed trait Segment
    sealed trait Special extends Segment
    sealed trait Addressable extends Segment
    object Segment {
      final case object Local extends Addressable
      final case object Argument extends Addressable
      final case object This extends Addressable
      final case object That extends Addressable
      final case object Constant extends Special
      final case object Static extends Special
      final case object Temp extends Addressable
      final case object Pointer extends Addressable
    }

    final case class Pop(segment: Segment, address: Int) extends MemoryAccess
    final case class Push(segment: Segment, address: Int) extends MemoryAccess
  }

  sealed trait Branching extends Command
  object Branching {
    final case class Label(label: String) extends Branching
    final case class GoTo(label: String) extends Branching
    final case class IfGoTo(label: String) extends Branching
  }

  sealed trait Function extends Command
  object Function {
    final case class Def(name: String, vars: List[Any]) extends Function
    final case class Call(name: String, args: List[Any]) extends Function
    final case object Return extends Function
  }
}
