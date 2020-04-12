package hack.vm

import hack.vm.StackMachine.Arithmetic.Add
import hack.vm.StackMachine.{Command, MemoryAccess}
import hack.vm.StackMachine.MemoryAccess.{Addressable, Segment}

object Translator extends App {
  sealed trait MemoryAccessType
  final case class Direct(offset: Int) extends MemoryAccessType
  final case class Indirect(label: String) extends MemoryAccessType

  def offset(segment: Addressable): MemoryAccessType = {
    segment match {
      case Segment.Local => Indirect("LCL")
      case Segment.Argument => Indirect("ARG")
      case Segment.This => Indirect("THIS")
      case Segment.That => Indirect("THAT")
      case Segment.Temp => Direct(5)
      case Segment.Pointer => Direct(3)
    }
  }

  import hack.assembler.Assembly._
  def eval(command: Command, filename: String): List[Expression] = {
    command match {
      case _: StackMachine.Arithmetic => ???
      case access: StackMachine.MemoryAccess => access match {
        case MemoryAccess.Pop(segment, address) => segment match {
          case segment: MemoryAccess.Special => segment match {
            case Segment.Constant => throw new RuntimeException("Can not pop constant")
            case Segment.Static =>
              List(
                Reference("SP"),
                CInstruction(Some("A"), "M", None),
                CInstruction(Some("D"), "M", None),
                Reference(s"$filename.$address"),
                CInstruction(Some("M"), "D", None),
                Reference("SP"),
                CInstruction(Some("M"), "M-1", None)
              )
          }
          case segment: MemoryAccess.Addressable => offset(segment) match {
            case Direct(offset) =>
              List(
                Reference("SP"),
                CInstruction(Some("A"), "M", None),
                CInstruction(Some("D"), "M", None),
                Constant(offset + address),
                CInstruction(Some("M"), "D", None),
                Reference("SP"),
                CInstruction(Some("M"), "M-1", None)
              )
            case Indirect(label) =>
              List(
                Reference("SP"),
                CInstruction(Some("A"), "M", None),
                CInstruction(Some("D"), "M", None),
                Reference(label),
                CInstruction(Some("A"), "M", None),
                CInstruction(Some("M"), "D", None),
                Reference("SP"),
                CInstruction(Some("M"), "M-1", None)
              )
          }
        }
        case MemoryAccess.Push(segment, address) => segment match {
          case special: MemoryAccess.Special => special match {
            case Segment.Constant =>
              List(
                Constant(address),
                CInstruction(Some("D"), "A", None),
                Reference("SP"),
                CInstruction(Some("A"), "M", None),
                CInstruction(Some("M"), "D", None),
                Reference("SP"),
                CInstruction(Some("M"), "M+1", None)
              )
            case Segment.Static =>
              List(
                Reference(s"$filename.$address"),
                CInstruction(Some("D"), "A", None),
                Reference("SP"),
                CInstruction(Some("A"), "M", None),
                CInstruction(Some("M"), "D", None),
                Reference("SP"),
                CInstruction(Some("M"), "M+1", None)
              )
          }
          case addressable: Addressable =>  offset(addressable) match {
            case Direct(offset) =>
              List(
                Constant(offset + address),
                CInstruction(Some("D"), "M", None),
                Reference("SP"),
                CInstruction(Some("A"), "M", None),
                CInstruction(Some("M"), "D", None),
                Reference("SP"),
                CInstruction(Some("M"), "M+1", None)
              )
            case Indirect(label) =>
              List(
                Reference(label),
                CInstruction(Some("A"), "M", None),
                CInstruction(Some("D"), "M", None),
                Reference("SP"),
                CInstruction(Some("A"), "M", None),
                CInstruction(Some("M"), "D", None),
                Reference("SP"),
                CInstruction(Some("M"), "M+1", None)
              )
          }
        }
      }
      case _: StackMachine.Branching => ???
      case _: StackMachine.Function => ???
    }
  }
  import MemoryAccess._
  import hack.vm.StackMachine.Arithmetic._
  val p = List(
    Push(Segment.Constant, 10),
    Pop(Segment.Local, 0),
    Push(Segment.Constant, 21),
    Push(Segment.Constant, 22),
    Pop(Segment.Argument, 2),
    Pop(Segment.Argument, 1),
    Push(Segment.Constant, 36),
    Pop(Segment.This, 6),
    Push(Segment.Constant, 42),
    Push(Segment.Constant, 45),
    Pop(Segment.That, 5),
    Pop(Segment.That, 2),
    Push(Segment.Constant, 510),
    Pop(Segment.Temp, 6),
    Push(Segment.Local, 0),
    Push(Segment.That, 5),
    Add,
    Push(Segment.Argument, 1),
    Sub,
    Push(Segment.This, 6),
    Push(Segment.This, 6),
    Add,
    Sub,
    Push(Segment.Temp, 6),
    Add,
  )
  val res = p.flatMap(eval(_, "Main.vm"))
  println(res.mkString(System.lineSeparator()))
}
