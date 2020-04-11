package hack.vm

object StackMachine extends App {
  sealed trait Command

  sealed trait Arithmetic
  object Arithmetic {
    final case object Add
    final case object Sub
    final case object Neg
    final case object Eq
    final case object Gt
    final case object Lt
    final case object And
    final case object Or
    final case object Not
  }

  sealed trait MemoryAccess
  object MemoryAccess {
    sealed trait Segment
    object Segment {
      final case object Local extends Segment
      final case object Argument extends Segment
      final case object This extends Segment
      final case object That extends Segment
      final case object Constant extends Segment
      final case object Static extends Segment
      final case object Temp extends Segment
      final case object Pointer extends Segment
    }

    final case class Pop(segment: Segment, address: Int)
    final case class Push(segment: Segment, address: Int)
  }

  sealed trait Branching
  object Branching {
    final case class Label(label: String)
    final case class GoTo(label: String)
    final case class IfGoTo(label: String)
  }

  sealed trait Function
  object Function {
    final case class Def(name: String, vars: List[Any])
    final case class Call(name: String, args: List[Any])
    final case object Return
  }
}
