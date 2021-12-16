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
      case _ => throw new Exception("Received Illegal Char ")
    }
  }
  def getInvalidChar(line: String): (Boolean, Char) = {
    for(char <- line){
      if(opens.contains(char)){
        stack.push(char)
      }else if(closes.contains(char)){
        val top = stack.top
        if(matches.contains(top) && matches(top).equals(char)){
          stack.pop
        }else{
          println(char, top, char)
          // return invalid matched char
          return (false, char)
        }
      }else{
        throw new Exception("Illegal Character Found! " + char)
      }
    }
    return (true, 'O')
  }
  // marks 0 as a valid row
  val matches: Map[Char, Char] = HashMap('[' -> ']', '(' -> ')', '{' -> '}','<' -> '>', 'O' -> 'O')
  val opens: List[Char] = matches.keys.toList
  val closes: List[Char] = matches.values.toList
  val lines = new InputReader(false).getLines()
  var invalidCharTracker: Map[Char, Int] = matches.map({case (k,v) => v -> 0})
  var stack: Stack[Char] = Stack()
  for(line <- lines){
    val (isValid, invalidChar) = getInvalidChar(line)
    if(!isValid){
      invalidCharTracker(invalidChar) = invalidCharTracker(invalidChar) + 1
    }
  }
  var sum = 0
  for((char, count) <- invalidCharTracker){
    val points = getPoints(char)
    println(char, points, count)
    sum = sum + points * count
  }
  println(sum)
}
