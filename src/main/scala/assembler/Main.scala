package assembler

import java.nio.file.Path

import scala.io.Source
import scala.util.{Try, Using}

object Main extends App {
  private def readFile(path: String): Try[String] = {
    Using(Source.fromFile(path)) { source =>
      source.getLines.mkString(System.lineSeparator())
    }
  }
  private def writeToFile(path: String, contents: String): Try[Path] = {
    import java.nio.file.{Paths, Files}
    import java.nio.charset.StandardCharsets
    Try {
      Files.write(Paths.get(path), contents.getBytes(StandardCharsets.UTF_8))
    }
  }

  val debug = false
  val inputFilePath = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\hardware\\06\\pong\\Pong.asm"
  val outputFilePath = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\hardware\\06\\pong\\Pong_1.hack"
  val fileContents = readFile(inputFilePath)
  fileContents.map { contents =>
    val parsingResult = Parser.parseRaw(contents)
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
          val result = binary.zipWithIndex.map{
            case ((expression, instruction), index) =>
              if (debug) {
                s"${index.toString.padTo(5, " ").mkString("")} $instruction          $expression"
              } else {
                s"$instruction"
              }
          }.mkString(System.lineSeparator())
          writeToFile(outputFilePath, result)
        }
    }
  }
}
