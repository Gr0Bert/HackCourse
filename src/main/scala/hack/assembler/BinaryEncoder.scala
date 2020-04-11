package hack.assembler

object BinaryEncoder {
  private def binary(repr: Int, padding: Int): String = {
    val bin = repr.toBinaryString
    bin.reverse.padTo(padding, '0').reverse
  }

  val Null: Int = 0
  val NullStr: String = "null"

  object Address {
    val toBinary = binary(_, 15)
  }

  object Register {
    val M: Int = 1
    val D: Int = 2
    val A: Int = 4

    def of(value: String): Either[String, String] = {

      val toBinary = binary(_, 3)

      def parse(registerRaw: Char) = {
        val get: PartialFunction[Char, Int] = {
          case 'M' => M
          case 'D' => D
          case 'A' => A
        }

        get.lift
          .andThen(_.toRight(s"Unknown register: $value"))
          .apply(registerRaw)
      }

      val (errors, registers) = value.map(parse).foldLeft((List.empty[String], List.empty[Int])) {
        case ((errors, registers), value) =>
          value match {
            case Left(error) => (error :: errors) -> registers
            case Right(register) => errors -> (register :: registers)
          }
      }

      if (errors.isEmpty)
        Right(toBinary(registers.sum))
      else
        Left(errors.mkString("\n\t"))
    }
  }

  object Jump {
    val GT: Int = 1
    val EQ: Int = 2
    val LT: Int = 4

    def of(value: String): Either[String, String] = {
      val toBinary = binary(_, 3)
      val get: PartialFunction[String, Int] = {
        case "null" => Null
        case "JGT" => GT
        case "JEQ" => EQ
        case "JLT" => LT
        case "JGE" => GT | EQ
        case "JNE" => GT | LT
        case "JLE" => EQ | LT
        case "JMP" => LT | EQ | GT
      }
      get.lift
        .andThen(_.toRight(s"Unknown jump instruction: $value").map(toBinary))
        .apply(value)
    }
  }

  object Compute {
    val toBinary = binary(_, 7)

    def of(value: String): Either[String, String] = {
      def get: PartialFunction[String, Int] = {
        case "0" => 42
        case "1" => 63
        case "-1" => 58
        case "D" => 12
        case "A" => 48
        case "!D" => 13
        case "!A" => 49
        case "-D" => 15
        case "-A" => 51
        case "D+1" => 31
        case "A+1" => 55
        case "D-1" => 14
        case "A-1" => 50
        case "D+A" => 2
        case "D-A" => 19
        case "A-D" => 7
        case "D&A" => 0
        case "D|A" => 21
        case "M" => 112
        case "!M" => 113
        case "-M" => 115
        case "M+1" => 119
        case "M-1" => 114
        case "D+M" => 66
        case "D-M" => 83
        case "M-D" => 71
        case "D&M" => 64
        case "D|M" => 85
      }
      get.lift
        .andThen(_.toRight(s"Unknown compute instruction: $value").map(toBinary))
        .apply(value)
    }
  }

  def encode(expr: ReferenceResolver.Expression): Either[String, (ReferenceResolver.Expression, String)] = {
    expr match {
      case ReferenceResolver.AddressConstant(value) => Right(expr -> s"0${Address.toBinary(value)}")
      case ReferenceResolver.Instruction(dest, comp, jump) =>
        for {
          d <- dest.map(Register.of).getOrElse(Right(binary(Null, 3)))
          c <- Compute.of(comp)
          j <- jump.map(Jump.of).getOrElse(Right(binary(Null, 3)))
        } yield {
          expr -> s"111$c$d$j"
        }
    }
  }
}
