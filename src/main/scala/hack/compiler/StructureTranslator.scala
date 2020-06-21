package hack.compiler

import hack.compiler.Compiler.Structure.Type

object StructureTranslator {
  import Compiler._
  import hack.vm.StackMachine._

  def translate(structure: Structure): Seq[Command] = {
    structure match {
      case Structure.ClassStruct(className, _, subroutineDec, clSt) =>
        val classSymbolTable = clSt.get
        val classSize = classSymbolTable.entries.values.map(_.`type`.size).sum
        subroutineDec.flatMap {
          case Structure.SubroutineDec(
              keyword,
              tp,
              subroutineName,
              parameterList,
              _,
              subroutineStatements,
              symbolTable
              ) =>
            val st = new StatementTranslator(
              ComposedSymbolTable(symbolTable.get, classSymbolTable),
              className.value
            )
            val localSize = symbolTable.get.entries.values.count(_.kind.value == "local")
            keyword.value match {
              case "constructor" =>
                translateConstructor(
                  st,
                  className.value,
                  subroutineName.value,
                  localSize,
                  classSize,
                  subroutineStatements,
                )
              case "method" =>
                val isVoid = tp match {
                  case Type.Primitive(value) => value == "void"
                  case Type.UserDefined(_) => false
                }
                translateMethod(
                  st,
                  className.value,
                  subroutineName.value,
                  localSize,
                  subroutineStatements,
                  isVoid,
                )
              case "function" =>
                val isVoid = tp match {
                  case Type.Primitive(value) => value == "void"
                  case Type.UserDefined(_) => false
                }
                translateFunction(
                  st,
                  className.value,
                  subroutineName.value,
                  localSize,
                  subroutineStatements,
                  isVoid,
                )
            }
        }
      case other => throw new RuntimeException(s"Expected class definition, found: $other")
    }
  }

  private def translateMethod(
    sTrans: StatementTranslator,
    className: String,
    subroutineName: String,
    localVarsCount: Int,
    statements: Seq[Statement],
    isVoid: Boolean,
  ): Seq[Command] = {
    val commands = Seq(
      Function.Def(s"${className}.${subroutineName}", localVarsCount),
      MemoryAccess.Push(MemoryAccess.Segment.Argument, 0),
      MemoryAccess.Pop(MemoryAccess.Segment.Pointer, 0),
    ) ++ statements.flatMap(sTrans.translate)

    if (isVoid) {
      addVoid(commands)
    } else {
      commands
    }
  }

  private def translateFunction(
    sTrans: StatementTranslator,
    className: String,
    subroutineName: String,
    localVarsCount: Int,
    statements: Seq[Statement],
    isVoid: Boolean,
  ): Seq[Command] = {
    val commands = Seq(Function.Def(s"${className}.${subroutineName}", localVarsCount)) ++ statements
      .flatMap(sTrans.translate)
    if (isVoid) {
      addVoid(commands)
    } else {
      commands
    }
  }

  private def addVoid(commands: Seq[Command]) = {
    val rv = commands.reverse
    val ret = rv.head
    val res = rv.tail.reverse
    res ++ Seq(MemoryAccess.Push(MemoryAccess.Segment.Constant, 0)) ++ Seq(ret)
  }

  private def translateConstructor(
    sTrans: StatementTranslator,
    className: String,
    subroutineName: String,
    parametersCount: Int,
    classSize: Int,
    statements: Seq[Statement],
  ): Seq[Command] = {
    Seq(
      Function.Def(s"${className}.${subroutineName}", parametersCount),
      MemoryAccess.Push(MemoryAccess.Segment.Constant, classSize),
      Function.Call("Memory.alloc", 1),
      MemoryAccess.Pop(MemoryAccess.Segment.Pointer, 0)
    ) ++
      statements.flatMap(sTrans.translate) ++
      Seq(MemoryAccess.Push(MemoryAccess.Segment.Pointer, 0))
  }
}
