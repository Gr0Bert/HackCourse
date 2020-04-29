package hack.vm

import hack.vm.StackMachine.{Arithmetic, Branching, Command, Function, MemoryAccess}
import hack.vm.StackMachine.MemoryAccess.{Addressable, Segment}

object Translator extends App {
  sealed trait Register
  case object A extends Register
  case object M extends Register
  case object D extends Register

  implicit def regToStr(r:Register): String = r.toString

  implicit def regToAsm(r: Register): Option[String] = {
    Some(r.toString)
  }

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

  def performBinaryArithmetic(instruction: CInstruction) = {
    List(
      Reference("SP"),
      CInstruction(M, "M-1", None),
      CInstruction(A, M, None),
      CInstruction(D, M, None),
      Reference("SP"),
      CInstruction(M, "M-1", None),
      CInstruction(A, M, None),
      instruction,
      Reference("SP"),
      CInstruction(M, "M+1", None)
    )
  }

  def performBinaryBoolean(idx: Int, jumpForTrue: String) = {
    val uniqueLabel = s"UNIQUE_LABEL_$idx"
    List(
      // pop top value to D
      Reference("SP"),
      CInstruction(M, "M-1", None),
      CInstruction(A, M, None),
      CInstruction(D, M, None),
      // select top - 1
      Reference("SP"),
      CInstruction(M, "M-1", None),
      CInstruction(A, M, None),
      // perform operation and save result `in place`
      CInstruction(M, "M-D", None),
      // push return address
      Reference("SP"),
      CInstruction(M, "M+1", None),
      Reference(uniqueLabel),
      CInstruction(D, A, None),
      Reference("SP"),
      CInstruction(A, M, None),
      CInstruction(M, D, None),
      // go to binary operation result
      Reference("SP"),
      CInstruction(M, "M-1", None),
      CInstruction(A, M, None),
      CInstruction(D, M, None),
      Reference("SP"),
      CInstruction(M, "M+1", None),
      // if result < 0
      Reference("PUSH_TRUE"),
      CInstruction(None, "D", Some(jumpForTrue)),
      // else
      Reference("PUSH_FALSE"),
      CInstruction(None, "0", Some("JMP")),
      // push `true` procedure
      Label("PUSH_TRUE"),
      Reference("SP"),
      CInstruction(A, M, None),
      CInstruction(D, M, None), // save return address
      Reference("SP"),
      CInstruction(Some("M"), "M-1", None),
      CInstruction(A, M, None),
      CInstruction(M, "1", None),
      // jump to return address
      CInstruction(A, D, None),
      CInstruction(None, "0", Some("JMP")),
      // push `false` procedure
      Label("PUSH_FALSE"),
      Reference("SP"),
      CInstruction(A, M, None),
      CInstruction(D, M, None), // save return address
      Reference("SP"),
      CInstruction(Some("M"), "M-1", None),
      CInstruction(A, M, None),
      CInstruction(M, "0", None),
      // jump to return address
      CInstruction(A, D, None),
      CInstruction(None, "0", Some("JMP")),
      // program continuation
      Label(uniqueLabel),
      Reference("SP"),
      CInstruction(M, "M+1", None),
    )
  }

  def eval(idx: Int, command: Command, filename: String): List[Expression] = {
    command match {
      case arithmetic: StackMachine.Arithmetic => arithmetic match {
        case Arithmetic.Add => performBinaryArithmetic(CInstruction(M, s"D+M", None))
        case Arithmetic.Sub => performBinaryArithmetic(CInstruction(M, "M-D", None))
        case Arithmetic.Neg =>
          List(
            Reference("SP"),
            CInstruction(M, "M-1", None),
            CInstruction(A, M, None),
            CInstruction(M, "-M", None),
            Reference("SP"),
            CInstruction(M, "M+1", None),
          )
        case Arithmetic.Eq => performBinaryBoolean(idx, "JEQ")
        case Arithmetic.Gt => performBinaryBoolean(idx, "JGT")
        case Arithmetic.Lt => performBinaryBoolean(idx, "JLT")
        case Arithmetic.And => performBinaryArithmetic(CInstruction(M, "D&M", None))
        case Arithmetic.Or => performBinaryArithmetic(CInstruction(M, "D|M", None))
        case Arithmetic.Not =>
          List(
            Reference("SP"),
            CInstruction(M, "M-1", None),
            CInstruction(A, M, None),
            CInstruction(M, "!M", None),
            CInstruction(M, "M+1", None),
            CInstruction(M, "M+1", None),
            Reference("SP"),
            CInstruction(M, "M+1", None),
          )
      }
      case access: StackMachine.MemoryAccess => access match {
        case MemoryAccess.Pop(segment, address) => segment match {
          case segment: MemoryAccess.Special => segment match {
            case Segment.Constant => throw new RuntimeException("Can not pop constant")
            case Segment.Static =>
              List(
                Reference("SP"),
                CInstruction(M, "M-1", None),
                CInstruction(A, M, None),
                CInstruction(D, M, None),
                Reference(s"$filename.$address"),
                CInstruction(M, D, None),
                Reference("SP"),
              )
          }
          case segment: MemoryAccess.Addressable => offset(segment) match {
            case Direct(offset) =>
              List(
                Reference("SP"),
                CInstruction(M, "M-1", None),
                CInstruction(A, M, None),
                CInstruction(D, M, None),
                Constant(offset + address),
                CInstruction(M, D, None),
                Reference("SP"),
              )
            case Indirect(label) =>
              List(
                Reference(label),
                CInstruction(D, M, None),
                Constant(address),
                CInstruction(D, s"D+A", None),

                Reference("R13"),
                CInstruction(M, D, None),

                Reference("SP"),
                CInstruction(M, "M-1", None),
                CInstruction(A, M, None),
                CInstruction(D, M, None),

                Reference("R13"),
                CInstruction(A, M, None),
                CInstruction(M, D, None),
              )
          }
        }
        case MemoryAccess.Push(segment, address) => segment match {
          case special: MemoryAccess.Special => special match {
            case Segment.Constant =>
              List(
                Constant(address),
                CInstruction(D, "A", None),
                Reference("SP"),
                CInstruction(A, "M", None),
                CInstruction(M, "D", None),
                Reference("SP"),
                CInstruction(M, "M+1", None)
              )
            case Segment.Static =>
              List(
                Reference(s"$filename.$address"),
                CInstruction(D, "A", None),
                Reference("SP"),
                CInstruction(A, "M", None),
                CInstruction(M, "D", None),
                Reference("SP"),
                CInstruction(M, "M+1", None)
              )
          }
          case addressable: Addressable =>  offset(addressable) match {
            case Direct(offset) =>
              List(
                Constant(offset + address),
                CInstruction(Some("D"), "M", None),
                Reference("SP"),
                CInstruction(A, "M", None),
                CInstruction(M, "D", None),
                Reference("SP"),
                CInstruction(M, "M+1", None)
              )
            case Indirect(label) =>
              List(
                Reference(label),
                CInstruction(D, M, None),
                Constant(address),
                CInstruction(D, s"D+A", None),
                CInstruction(A, D, None),
                CInstruction(D, M, None),

                Reference("SP"),
                CInstruction(A, M, None),
                CInstruction(M, D, None),
                Reference("SP"),
                CInstruction(M, "M+1", None)
              )
          }
        }
      }
      case branch: StackMachine.Branching => branch match {
        case Branching.Label(label) => List(Label(label))
        case Branching.GoTo(label) => List(Reference(label), CInstruction(None, "0", Some("JMP")))
        case Branching.IfGoTo(label) => List(
          Reference("SP"),
          CInstruction(M, "M-1", None),
          CInstruction(A, M, None),
          CInstruction(D, M, None),
          Reference(label),
          CInstruction(None, D, Some("JGT")),
        )
      }
      case function: StackMachine.Function => function match {
        case Function.Def(name) => List(Label(mkFunctionPrefix(filename, name)))
        case Function.Call(name, args) =>
          val functionLabel = mkFunctionPrefix(filename, name)
          val returnLabel = s"$functionLabel.$idx"
          def pushFromLabel(labelName: String) = {
            List(
              Reference(labelName),
              CInstruction(D, M, None),
              Reference("SP"),
              CInstruction(A, M, None),
              CInstruction(M, D, None),
              Reference("SP"),
              CInstruction(M, "M+1", None),
            )
          }
          val pushReturnAddress = List(
            // push return address
            Reference(returnLabel),
            CInstruction(D, A, None),
            Reference("SP"),
            CInstruction(A, M, None),
            CInstruction(M, D, None),
            Reference("SP"),
            CInstruction(M, "M+1", None),
          )
          val pushLcl = pushFromLabel("LCL")
          val pushArg = pushFromLabel("ARG")
          val pushThis = pushFromLabel("THIS")
          val pushThat = pushFromLabel("THAT")
          val setNewArg = List(
            Constant(5 + args),
            CInstruction(D, A, None),
            Reference("SP"),
            CInstruction(D, "M-D", None),
            Reference("ARG"),
            CInstruction(M, D, None),
          )
          val setLclToSP = List(
            Reference("SP"),
            CInstruction(D, M, None),
            Reference("LCL"),
            CInstruction(M, D, None),
          )
          pushReturnAddress ++
            pushLcl ++
            pushArg ++
            pushThis ++
            pushThat ++
            setNewArg ++
            setLclToSP ++
            eval(idx, Branching.GoTo(functionLabel), filename) :+ Label(returnLabel)
        case Function.Return =>
          val placeReturnToArg = List(
            AtSP,
            CInstruction(M, "M-1", None),
            CInstruction(A, M),
            CInstruction(D, M),
            AtArg,
            CInstruction(A, M),
            CInstruction(M, D),
            AtArg,
            CInstruction(D, M),
            AtSP,
            CInstruction(M, D),
            AtSP,
            CInstruction(M, "M+1")
          )
          val putReturnOnStack = List(
            Constant(5),
            CInstruction(D, A),
            AtLCL,
            CInstruction(D, "M-D"),
            CInstruction(A, D),
            CInstruction(D, M),
            AtSP,
            CInstruction(A, M),
            CInstruction(M, D),
          )
          val goToReturn = List(
            AtSP,
            CInstruction(A, M),
            CInstruction(D, M),
            CInstruction(A, D),
            CInstruction(None, "0", Some("JMP")),
          )
          def restore(index: Int, name: String) = List(
            Constant(index),
            CInstruction(D, A),
            AtLCL,
            CInstruction(D, "M-D"),
            CInstruction(A, D),
            CInstruction(D, M),
            Reference(name),
            CInstruction(M, D),
          )

          placeReturnToArg ++
          restore(1, "THAT") ++
          restore(2, "THIS") ++
          restore(3, "ARG") ++
          putReturnOnStack ++
          restore(4, "LCL") ++
          goToReturn
      }
    }
  }

  private val AtSP = Reference("SP")
  private val AtArg = Reference("ARG")
  private val AtLCL = Reference("LCL")

  private def mkFunctionPrefix(filename: String, functionName: String) =
    filename.replace(".vm", "") + "$" + functionName

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

  val lessThanTest = List(
    Push(Segment.Constant, 6),
    Push(Segment.Constant, 5),
    Gt
  )

  val notTest = List(
    Push(Segment.Constant, 12),
    Neg
  )

  val branchTest = List(
    StackMachine.Branching.Label("START"),
    Push(Segment.Constant, 11),
    Push(Segment.Constant, 10),
    Lt,
    StackMachine.Branching.IfGoTo("START"),
    StackMachine.Branching.Label("END"),
    StackMachine.Branching.GoTo("END"),
  )

  val callTest = List(
    Push(Segment.Constant, 2),
    Push(Segment.Constant, 4),
    Function.Call("foo", 2),
    Function.Def("foo"),
    Push(Segment.Constant, 42),
    Function.Return
  )

  callTest.zipWithIndex.foreach{
    case (command, idx) =>
      println{
        s"""// $command
           |${eval(idx, command, "Main.vm").mkString(System.lineSeparator())}
           |""".stripMargin
      }
  }
}
