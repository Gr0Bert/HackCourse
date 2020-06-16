package hack.compiler

object SymbolTableResolver {
  import Compiler._

  def resolve(ast: Compiler.AST): Compiler.AST = {
    ast match {
      case cl @ Structure.ClassStruct(className, classVarDec, subroutineDec, _) =>
        val clSt = classVarDec.foldLeft(SymbolTable.empty) {
          case (st, Structure.ClassVarDec(keyword, tp, varNames)) =>
            varNames.foldLeft(st) {
              case (st, name) => st.update(keyword, name, tp)
            }
        }
        val subs = subroutineDec.map {
          case sub @ Structure.SubroutineDec(
          keyword,
          _,
          _,
          parameterList,
          subroutineVars,
          _,
          _,
          ) =>
            val argumentKind = Token.Keyword("argument")
            val localKind = Token.Keyword("local")
            val thisIdentifier = Token.Identifier("this")
            val zeroSt = keyword match {
              case Token.Keyword("method") =>
                SymbolTable.empty.update(argumentKind, thisIdentifier, Structure.Type.UserDefined(className))
              case _ => SymbolTable.empty
            }
            val argSt = parameterList.foldLeft(zeroSt){
              case (st, Structure.SubroutineParameter(tp, varName)) =>
                st.update(argumentKind, varName, tp)
            }
            val varsSt = subroutineVars.foldLeft(argSt) {
              case (st, Structure.VarDec(tp, varNames)) =>
                varNames.foldLeft(st) {
                  case (st, name) => st.update(localKind, name, tp)
                }
            }

            sub.copy(symbolTable = Some(varsSt))
        }
        cl.copy(symbolTable = Some(clSt), subroutineDec = subs)
    }
  }
}
