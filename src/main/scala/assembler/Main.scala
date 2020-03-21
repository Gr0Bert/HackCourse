package assembler

/*
*
0000000000000010
1110110000010000
0000000000000011
1110000010010000
0000000000000000
1110001100001000
 */

object Main extends App {
  val add = """@2
              |D=A
              |@3
              |D=D+A
              |@0
              |M=D""".stripMargin
  val singleExpressionWithLineSeparator = "D=D+A" + System.lineSeparator() + " "
  val singleExpression = "D=D+A"
  val parsingResult = Parser.parseRaw(singleExpressionWithLineSeparator)
   parsingResult match {
      case Left(error) => println(error)
      case Right((_, expressions)) =>
        val errorOrBinary = expressions.map(BinaryEncoder.encode)
        val errors = errorOrBinary.collect { case Left(error) => error }
        if (errors.nonEmpty) {
          errors.foreach(println)
        } else {
          val binary = errorOrBinary.collect { case Right((expression, binary)) => expression -> binary }
          binary.foreach(println)
        }
  }
}
