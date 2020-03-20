package main

object Main extends App {
  Parser.parseRaw("A=D+M;JEQ").foreach {
    case (i, expression) => println(i -> BinaryEncoder.encode(expression))
  }
}
