package hack.compiler

import java.nio.file.{Files, Path}

import scala.io.Source
import scala.util.{Try, Using}

object Main extends App {

  private def writeToFile(path: String, contents: String): Try[Path] = {
    import java.nio.charset.StandardCharsets
    import java.nio.file.{Files, Paths}
    Try {
      Files.write(Paths.get(path), contents.getBytes(StandardCharsets.UTF_8))
    }
  }

  private def readFile(path: String): Try[String] = {
    Using(Source.fromFile(path)) { source =>
      source.getLines.mkString(System.lineSeparator())
    }
  }

  def compile(contents: String) = {
    val parsingResult = Parser.parseRaw(contents)
    parsingResult match {
      case Left((failureString, index, extra)) =>
        println(index)
        println(failureString)
        println(extra.stack)
        println(extra.trace(true))
        throw new RuntimeException("Failed to compile")
      case Right((_, expressions)) =>
        expressions
    }
  }

  def processFile(filePath: Path) = {
    val contents = readFile(filePath.toAbsolutePath.toString).get
    val ast = compile(contents)
    val resolvedTree = SymbolTableResolver.resolve(ast)
    println(resolvedTree)
    val fileNameTxt = filePath.getFileName.toString.replace(".jack", ".xml")
    val outputFilePath = Path.of(filePath.getParent.toString, s"m_$fileNameTxt").toString
    writeToFile(outputFilePath, ast.toXml)
  }

  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\jack\\10\\Square"
  val path = Path.of(pathRaw)
  if (Files.isDirectory(path)) {
    import scala.jdk.CollectionConverters._
    val files = Files.walk(path, 1).iterator().asScala.toList.filter(_.getFileName.toString.endsWith(".jack"))
    println(path)
    println(files)
    files.map(processFile)
  }
}