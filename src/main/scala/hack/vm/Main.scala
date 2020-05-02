package hack.vm

import java.nio.file.{Files, Path}
import scala.io.Source
import scala.util.{Try, Using}

object Main extends App {

  private def writeToFile(path: String, contents: String): Try[Path] = {
    import java.nio.file.{Paths, Files}
    import java.nio.charset.StandardCharsets
    Try {
      Files.write(Paths.get(path), contents.getBytes(StandardCharsets.UTF_8))
    }
  }

  private def readFile(path: String): Try[String] = {
    Using(Source.fromFile(path)) { source =>
      source.getLines.mkString(System.lineSeparator())
    }
  }

  def compile(contents: String, fileName: String) = {
    val parsingResult = Parser.parseRaw(contents)
    parsingResult match {
      case Left((failureString, index, extra)) =>
        println(index)
        println(failureString)
        println(extra.stack)
        println(extra.trace(true))
        throw new RuntimeException("Failed to compile")
      case Right((_, expressions)) =>
        val assembly = expressions.zipWithIndex.map {
          case (exp, idx) => Translator.eval(idx, exp, fileName) -> exp
        }
        assembly
    }
  }

//  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\stack_machine\\07\\StackArithmetic\\StackTest\\StackTest.vm"
//  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\stack_machine\\07\\StackArithmetic\\SimpleAdd\\SimpleAdd.vm"
//  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\stack_machine\\07\\MemoryAccess\\StaticTest\\StaticTest.vm"
//  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\stack_machine\\07\\MemoryAccess\\PointerTest\\PointerTest.vm"
//  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\stack_machine\\07\\MemoryAccess\\BasicTest\\BasicTest.vm"
//  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\stack_machine\\08\\ProgramFlow\\BasicLoop\\BasicLoop.vm"
//  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\stack_machine\\08\\FunctionCalls\\FibonacciElement\\fusedFibo\\FibonacciElement.vm"
//  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\stack_machine\\test\\goto\\goto.vm"
//  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\stack_machine\\08\\FunctionCalls\\NestedCall"
//  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\stack_machine\\08\\FunctionCalls\\FibonacciElement"
//  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\stack_machine\\08\\FunctionCalls\\SimpleFunction\\SimpleFunction.vm"
//  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\stack_machine\\08\\FunctionCalls\\StaticsTest"
  val pathRaw = "C:\\Users\\Tanya\\IdeaProjects\\HackCourse\\stack_machine\\08\\ProgramFlow\\FibonacciSeries\\FibonacciSeries.vm"
  val path = Path.of(pathRaw)
  if (Files.isDirectory(path)) {
    import scala.jdk.CollectionConverters._
    val files = Files.walk(path, 1).iterator().asScala.toList.filter(_.getFileName.toString.endsWith(".vm"))
    println(path)
    println(files)
    val compiledLines = files.flatMap { filePath =>
      val fileName = filePath.getFileName.toString
      val contents = readFile(filePath.toAbsolutePath.toString)
      contents.map(compile(_, fileName.replace(".vm", ""))).get
    }
    val init = List(
      hack.assembler.Assembly.Constant(256),
      hack.assembler.Assembly.CInstruction(Some("D"), "A", None),
      hack.assembler.Assembly.Reference("SP"),
      hack.assembler.Assembly.CInstruction(Some("M"), "D", None),
    ) ++ Translator.eval(0, StackMachine.Function.Call("Sys.init", 0), "Sys")
    val outputFilePath = Path.of(path.toString, s"${path.getFileName.toString}.asm").toString
    val asm = init.map(exp => s"$exp // Init") ++ compiledLines.flatMap{
      case (exps, command) => exps.map(exp => s"$exp // $command")
    }
    val result = asm.mkString(System.lineSeparator())
    writeToFile(outputFilePath, result).get
  } else {
    val fileName = path.getFileName.toString
    val fileNameAsm = path.getFileName.toString.replace(".vm", ".asm")
    val outputFilePath = Path.of(path.getParent.toString, fileNameAsm).toString
    val contents = readFile(path.toAbsolutePath.toString)
    contents.map(compile(_, fileName.replace(".vm", ""))).flatMap { asm =>
      val result = asm.flatMap{
        case (exps, command) => exps.map(exp => s"$exp // $command")
      }.mkString(System.lineSeparator())
      writeToFile(outputFilePath, result)
    }
  }
}
