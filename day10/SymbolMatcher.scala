import scala.io.Source._
import scala.collection.mutable._

class InputReader(isTest: Boolean = true) {
  val testPath = "input.txt"
  val prodPath = "inputLarge.txt"
  val pathToUse = if (isTest) testPath else prodPath
  def getLines(): List[String] = {
    val source = scala.io.Source.fromFile(pathToUse)
    val lines = source.getLines.toList
    lines
  }
}

object SymbolMatcher extends App {
  def getPoints(char: Char): Int = {
    char match {
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137
      case 'O' => 1
      case _   => throw new Exception("Received Illegal Char ")
    }
  }
  def isLineIncomplete(line: String): (Boolean, Stack[Char]) = {
    var stack: Stack[Char] = Stack()
    for (char <- line) {
      if (opens.contains(char)) {
        stack.push(char)
      } else if (closes.contains(char)) {
        val top = stack.top
        if (matches.contains(top) && matches(top).equals(char)) {
          stack.pop
        } else {
          stack.pop
          // return invalid matched char
          return (false, stack)
        }
      } else {
        throw new Exception("Illegal Character Found! " + char)
      }
    }
    return (true, stack)
  }
  def getInvalidChar(line: String): (Boolean, Char) = {
    var stack: Stack[Char] = Stack()
    for (char <- line) {
      if (opens.contains(char)) {
        stack.push(char)
      } else if (closes.contains(char)) {
        val top = stack.top
        if (matches.contains(top) && matches(top).equals(char)) {
          stack.pop
        } else {
          // throwaway the invalid char
          stack.pop
          // throwaway unmatched chars
          println(char, top, char)
          // return invalid matched char
          return (false, char)
        }
      } else {
        throw new Exception("Illegal Character Found! " + char)
      }
    }
    return (true, 'O')
  }
  // marks 0 as a valid row
  val matches: Map[Char, Char] =
    HashMap('[' -> ']', '(' -> ')', '{' -> '}', '<' -> '>', 'O' -> 'O')
  val charScores: Map[Char, Int] =
    HashMap('(' -> 1, '[' -> 2, '{' -> 3, '<' -> 4)
  val opens: List[Char] = matches.keys.toList
  val closes: List[Char] = matches.values.toList
  val lines = new InputReader(false).getLines()
  var invalidCharTracker: Map[Char, Int] = matches.map({ case (k, v) =>
    v -> 0
  })
  var scores: List[Double] = List()
  for (line <- lines) {
    val (useForPart2, stack) = isLineIncomplete(line)
    if (useForPart2) {
      var remainderChars: List[Char] = List()
      while (!stack.isEmpty) {
        remainderChars = remainderChars :+ stack.pop
      }
      var score: Double = 0
      for (char <- remainderChars) {
        score = score * 5 + charScores(char)
        println(char, score)
      }
      scores = scores :+ score
    }
  }
  val sorted = scores.sorted
  println(sorted)
  val middleScore = sorted(sorted.size / 2)
  val toPrint = new java.math.BigDecimal(middleScore).toPlainString
  println(toPrint)
}
