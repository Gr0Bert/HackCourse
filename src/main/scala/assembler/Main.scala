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
              |(Label)
              |D=A
              |@LABEL
              |D=D+A
              |@0
              |M=D""".stripMargin
  val singleExpression = "(Label)"
  val parsingResult = Parser.parseRaw(add)
   parsingResult match {
      case Left((failureString, index, extra)) =>
        println(index)
        println(failureString)
        println(extra.stack)
        println(extra.trace(true))
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
