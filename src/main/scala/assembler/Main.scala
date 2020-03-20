package assembler

import assembler.Parser.Expression

object Main extends App {
  val add = """@2
              |D=A
              |@3
              |D=D+A
              |@0
              |M=D""".stripMargin
  val results: List[Expression] = add.split('\n').toList.map(Parser.parseRaw).collect {
    case Right((_, expr)) => expr
  }

  results.map(BinaryEncoder.encode).collect{
    case Right((_, binary)) => binary
  }.foreach(println)
}
