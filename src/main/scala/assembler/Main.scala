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
  val add = """@R0
              |D=M
              |@R1
              |D=D-M
              |@OUTPUT_FIRST
              |D;JGT
              |@R1
              |D=M
              |@OUTPUT_D
              |0;JMP
              |(OUTPUT_FIRST)
              |@R0
              |D=M
              |(OUTPUT_D)
              |@R2
              |M=D
              |(INFINITE_LOOP)
              |@INFINITE_LOOP
              |0;JMP""".stripMargin
  //todo:
  // add comments
  // ignore spaces
  // empty last line
  // default address constants
  // dots in labels and references
  val singleExpression = "(Label)"
  val parsingResult = Parser.parseRaw(add)
   parsingResult match {
      case Left((failureString, index, extra)) =>
        println(index)
        println(failureString)
        println(extra.stack)
        println(extra.trace(true))
      case Right((_, expressions)) =>
        val labelsResolverResult = LabelsResolver.resolveLabels(expressions)
        val normalisedExpressions = ReferenceResolver.resolveReferences(labelsResolverResult)
        val errorOrBinary = normalisedExpressions.map(BinaryEncoder.encode)
        val errors = errorOrBinary.collect { case Left(error) => error }
        if (errors.nonEmpty) {
          errors.foreach(println)
        } else {
          val binary = errorOrBinary.collect { case Right((expression, binary)) => expression -> binary }
          binary.foreach(x => println(x._2))
        }
  }
}
