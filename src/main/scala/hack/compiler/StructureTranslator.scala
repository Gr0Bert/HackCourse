package hack.compiler

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
              _,
              subroutineName,
              parameterList,
              _,
              subroutineStatements,
              symbolTable
            ) =>
            val st = new StatementTranslator(ComposedSymbolTable(symbolTable.get, classSymbolTable), className.value)
            keyword.value match {
              case "constructor" =>
                translateConstructor(
                  st,
                  className.value,
                  subroutineName.value,
                  parameterList.size,
                  classSize,
                  subroutineStatements,
                )
              case _ => ???
            }
        }
      case other => throw new RuntimeException(s"Expected class definition, found: $other")
    }
  }

  private def translateMethod(): Seq[Command] = ???
  private def translateFunction(): Seq[Command] = ???

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
      Seq(
        MemoryAccess.Push(MemoryAccess.Segment.Pointer, 0),
        Function.Return,
      )
  }
}
