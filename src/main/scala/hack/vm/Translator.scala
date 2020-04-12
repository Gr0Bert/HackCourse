package hack.vm

object Translator extends App {
  sealed trait Register
  final case object A extends Register
  final case object M extends Register
  final case object D extends Register


  final case class Address(value: Int)

  final case class Pointer(address: Address)


  // @address
  // A = M
  // return register in which accessed value stored (M)
  final case class PointerAccess(pointer: Pointer)

  // $registerTo = $registerFrom
  // return $registerTo
  final case class Store(registerTo: Register, registerFrom: Register)

  // @constant
  // $register = A
  // return $register
  final case class StoreConstant(register: Register, constant: Int)

  val sp = Pointer(5)
  StoreConstant(D, 2) // D
  PointerAccess(sp) // M
  Store(M, D)
  PointerAccess
}
