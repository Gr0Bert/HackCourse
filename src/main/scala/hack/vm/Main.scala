package hack.vm

object Main extends App {

  import java.nio.file.Path

  import scala.io.Source
  import scala.util.{Try, Using}

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
  val fileName = "StackTest"
  val path = s"C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\stack_machine\\07\\StackArithmetic\\StackTest\\$fileName"
  val inputFilePath = s"$path.vm"
  val outputFilePath = s"$path.asm"
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
        val assembly = expressions.zipWithIndex.flatMap {
          case (exp, idx) => Translator.eval(idx, exp, fileName)
        }

        val result = assembly.zipWithIndex.map {
          case (instruction, index) =>
            if (debug) {
              s"${index.toString.padTo(5, " ").mkString("")} $instruction"
            } else {
              s"$instruction"
            }
        }.mkString(System.lineSeparator())
        writeToFile(outputFilePath, result)
    }
  }
}
