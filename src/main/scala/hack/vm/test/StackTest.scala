package hack.vm
package test

import StackMachine._
import MemoryAccess._
import Arithmetic._
import MemoryAccess.Segment._

object StackTest {
  val program = List(
    Push(Constant,  17),
    Push(Constant,  17),
    Eq,
    Push(Constant,  17),
    Push(Constant,  16),
    Eq,
    Push(Constant,  16),
    Push(Constant,  17),
    Eq,
    Push(Constant,  892),
    Push(Constant,  891),
    Lt,
    Push(Constant,  891),
    Push(Constant,  892),
    Lt,
    Push(Constant,  891),
    Push(Constant,  891),
    Lt,
    Push(Constant,  32767),
    Push(Constant,  32766),
    Gt,
    Push(Constant,  32766),
    Push(Constant,  32767),
    Gt,
    Push(Constant,  32766),
    Push(Constant,  32766),
    Gt,
    Push(Constant,  57),
    Push(Constant,  31),
    Push(Constant,  53),
    Add,
    Push(Constant,  112),
    Sub,
    Neg,
    And,
    Push(Constant,  82),
    Or,
    Not,
  )
}
