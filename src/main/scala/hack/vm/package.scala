package hack

package object vm {
  object StackMachine {
    sealed trait Command

    sealed trait Arithmetic extends Command
    object Arithmetic {
      final case object Add extends Arithmetic {
        override def toString: String = "add"
      }
      final case object Sub extends Arithmetic {
        override def toString: String = "sub"
      }
      final case object Neg extends Arithmetic {
        override def toString: String = "neg"
      }
      final case object Eq extends Arithmetic {
        override def toString: String = "eq"
      }
      final case object Gt extends Arithmetic {
        override def toString: String = "gt"
      }
      final case object Lt extends Arithmetic {
        override def toString: String = "lt"
      }
      final case object And extends Arithmetic {
        override def toString: String = "and"
      }
      final case object Or extends Arithmetic {
        override def toString: String = "or"
      }
      final case object Not extends Arithmetic {
        override def toString: String = "not"
      }
    }

    sealed trait MemoryAccess extends Command
    object MemoryAccess {
      sealed trait Segment
      sealed trait Special extends Segment
      sealed trait Addressable extends Segment
      object Segment {
        final case object Local extends Addressable {
          override def toString: String = "local"
        }
        final case object Argument extends Addressable {
          override def toString: String = "argument"
        }
        final case object This extends Addressable {
          override def toString: String = "this"
        }
        final case object That extends Addressable {
          override def toString: String = "that"
        }
        final case object Constant extends Special {
          override def toString: String = "constant"
        }
        final case object Static extends Special {
          override def toString: String = "static"
        }
        final case object Temp extends Addressable {
          override def toString: String = "temp"
        }
        final case object Pointer extends Addressable {
          override def toString: String = "pointer"
        }
      }

      final case class Pop(segment: Segment, address: Int) extends MemoryAccess {
        override def toString: String = s"pop $segment $address"
      }
      final case class Push(segment: Segment, address: Int) extends MemoryAccess {
        override def toString: String = s"push $segment $address"
      }
    }

    sealed trait Branching extends Command
    object Branching {
      final case class Label(label: String) extends Branching {
        override def toString: String = s"label $label"
      }
      final case class GoTo(label: String) extends Branching {
        override def toString: String = s"goto $label"
      }
      final case class IfGoTo(label: String) extends Branching {
        override def toString: String = s"if-goto $label"
      }
    }

    sealed trait Function extends Command
    object Function {
      final case class Def(name: String, localVars: Int) extends Function {
        override def toString: String = s"function $name $localVars"
      }
      final case class Call(name: String, args: Int) extends Function {
        override def toString: String = s"call $name $args"
      }
      final case object Return extends Function {
        override def toString: String = "return"
      }
    }
  }
}
