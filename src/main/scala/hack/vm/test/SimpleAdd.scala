package hack.vm
package test

import StackMachine._
import MemoryAccess._
import Arithmetic._
import MemoryAccess.Segment._

object SimpleAdd {
  val program = List(
    Push(Constant, 7),
    Push(Constant, 8),
    Add,
  )
}
