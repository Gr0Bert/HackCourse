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
  val add =
    """// This file is part of www.nand2tetris.org
      |// and the book "The Elements of Computing Systems"
      |// by Nisan and Schocken, MIT Press.
      |// File name: projects/06/max/Max.asm
      |
      |// Computes R2 = max(R0, R1)  (R0,R1,R2 refer to RAM[0],RAM[1],RAM[2])
      |
      |    @R0
      |    D=M              // D = first number
      |    @R1
      |    D=D-M            // D = first number - second number
      |    @OUTPUT_FIRST
      |    D;JGT            // if D>0 (first is greater) goto output_first
      |    @R1
      |    D=M              // D = second number
      |    @OUTPUT_D
      |    0;JMP            // goto output_d
      |(OUTPUT_FIRST)
      |    @R0
      |    D=M              // D = first number
      |(OUTPUT_D)
      |    @R2
      |    M=D              // M[2] = D (greatest number)
      |(INFINITE_LOOP)
      |    @INFINITE_LOOP
      |    0;JMP            // infinite loop
      |""".stripMargin
  //todo:
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
