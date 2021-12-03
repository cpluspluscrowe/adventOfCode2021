import scala.io.Source._
import scala.collection.mutable._

class InputReader(isTest: Boolean = true) {
  val testPath = "input.txt"
  val prodPath = "inputLarge.txt"
  val pathToUse = if(isTest) testPath else prodPath
  def getLines(): List[String] = {
    val source = scala.io.Source.fromFile(pathToUse)
    val lines = source.getLines.toList
    lines
  }
}

class GetBitFrequency {
  val lines = new InputReader(false).getLines()
  def part1(): Int = {
    val verticals = transverseLines()
    val mostCommon = getMostCommonBits(verticals)
    val leastCommon = inverseBits(mostCommon)
    val commonNumber = binaryToNumber(mostCommon)
    val leastCommonNumber = binaryToNumber(leastCommon)
    (commonNumber * leastCommonNumber).toInt
  }
  def binaryToNumber(binary: List[Int]): Double = {
    var number: Double = 0D
    var power: Double = binary.size - 1
    for(bit <- binary){
      if(bit == 1){
        number = number + Math.pow(2,power)
      }
      power = power - 1
    }
    number
  }
  // return contains the binary integers for the most common vertical bits
  def getMostCommonBits(verticals: List[List[Int]]): List[Int] = {
    var mostCommonBits: List[Int] = List()
    for(vertical <- verticals){
      val oneCount = vertical.sum
      val zeroCount = vertical.size - oneCount
      val mostCommon = if(oneCount > zeroCount) 1 else 0
      mostCommonBits = mostCommonBits :+ mostCommon
    }
    mostCommonBits
  }
  def inverseBits(mostCommon: List[Int]): List[Int] = {
    var leastCommonBits: List[Int] = List()
    for(bit <- mostCommon){
      val bitToAdd = if(bit == 0) 1 else 0
      leastCommonBits = leastCommonBits :+ bitToAdd
    }
    leastCommonBits
  }
  def transverseLines(): List[List[Int]] = {
    val verticalKeys: HashMap[Int, List[Int]] = HashMap()
    for(line <- lines){
      for(binaryIndex <- 0 until line.size){
        val binary = if(line(binaryIndex) == '0') 0 else 1
        if(!verticalKeys.contains(binaryIndex)){
          verticalKeys += (binaryIndex -> List[Int]())
        }
        verticalKeys(binaryIndex) = verticalKeys(binaryIndex) :+ binary
      }
    }
    return verticalKeys.values.toList
  }
}

object GetBitFrequency extends App {
  println(new GetBitFrequency().part1)
}
