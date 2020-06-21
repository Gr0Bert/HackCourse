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

  def compile(contents: String, path: Path) = {
    val parsingResult = Parser.parseRaw(contents)
    parsingResult match {
      case Left((failureString, index, extra)) =>
        throw new RuntimeException(s"Failed to compile $path")
      case Right((_, expressions)) =>
        expressions
    }
  }

  def processFile(filePath: Path) = {
    val contents = readFile(filePath.toAbsolutePath.toString).get
    val ast = compile(contents, filePath)
    val resolvedTree = SymbolTableResolver.resolve(ast)
    val vmCommands = StructureTranslator.translate(resolvedTree.asInstanceOf[Compiler.Structure])
    val fileNameTxt = filePath.getFileName.toString.replace(".jack", ".vm")
    val outputFilePath = Path.of(filePath.getParent.toString, s"$fileNameTxt").toString
    writeToFile(outputFilePath, vmCommands.map(_.toString).mkString(System.lineSeparator()))
  }

  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\jack\\11\\Pong"
  val path = Path.of(pathRaw)
  if (Files.isDirectory(path)) {
    import scala.jdk.CollectionConverters._
    val files = Files.walk(path, 1).iterator().asScala.toList.filter(_.getFileName.toString.endsWith(".jack"))
    println(path)
    println(files)
    files.map(processFile)
  }
}
