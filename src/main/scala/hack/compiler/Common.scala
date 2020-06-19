package hack.compiler

import hack.compiler.Compiler.Token.Keyword
import hack.vm.StackMachine.MemoryAccess

object Common {
  def kindToSegment(kind: Keyword): MemoryAccess.Segment = kind match {
    case "local" => MemoryAccess.Segment.Local
    case "field" => MemoryAccess.Segment.This
    case "static" => MemoryAccess.Segment.Static
    case "argument" => MemoryAccess.Segment.Argument
  }
}
