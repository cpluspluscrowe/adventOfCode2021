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
  def part1(): Int = {
    val lines = new InputReader(true).getLines()
    val verticals = transverseLines(lines)
    val mostCommon = getMostCommonBits(verticals)
    val leastCommon = inverseBits(mostCommon)
    val commonNumber = binaryToNumber(mostCommon)
    val leastCommonNumber = binaryToNumber(leastCommon)
    (commonNumber * leastCommonNumber).toInt
  }
  def part2(): Int = {
    val lines = new InputReader(false).getLines()    
    val horizontals = processBinaryStrings(lines)
    val verticals = transverseLines(lines)
    // must do this check at each filtered loop
    //val mostCommon = getMostCommonBits(verticals)
    //val leastCommon = inverseBits(mostCommon)
    val matchingColumnCommon = filterToMatchingBitset(true, horizontals, verticals)
    val matchingColumnLeastCommon = filterToMatchingBitset(false, horizontals, verticals)
    println(matchingColumnCommon, matchingColumnLeastCommon)
    val commonNumber = binaryToNumber(matchingColumnCommon)
    val leastCommonNumber = binaryToNumber(matchingColumnLeastCommon)
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
  def filterToMatchingBitset(matchMostCommon: Boolean = true, horizontals: List[List[Int]], verticals: List[List[Int]]): List[Int] = {
    var filteredHorizontals = horizontals
    var filteredVerticals = transverseHorizontals(horizontals)
    for(bitIndex <- 0 until horizontals(0).size){
      val mostCommon = getMostCommonBitsAtIndex(filteredVerticals, bitIndex)
      val leastCommon = if(mostCommon == 1) 0 else 1
      val bitToMatch = if(matchMostCommon) mostCommon else leastCommon
      filteredHorizontals = filteredHorizontals.filter(_(bitIndex).equals(bitToMatch))
      filteredVerticals = transverseHorizontals(filteredHorizontals)
      println(bitToMatch, filteredHorizontals.size)
      if(filteredHorizontals.size == 1) return filteredHorizontals(0)
    }
    filteredHorizontals(0)
  }
  // return contains the binary integers for the most common vertical bits
  def transverseHorizontals(horizontals: List[List[Int]]): List[List[Int]] = {
    val verticalKeys: HashMap[Int, List[Int]] = HashMap()
    for(line <- horizontals){
      for(binaryIndex <- 0 until line.size){
        val binary = line(binaryIndex)
        if(!verticalKeys.contains(binaryIndex)){
          verticalKeys += (binaryIndex -> List[Int]())
        }
        verticalKeys(binaryIndex) = verticalKeys(binaryIndex) :+ binary
      }
    }
    return verticalKeys.values.toList
  }
  def getMostCommonBitsAtIndex(verticals: List[List[Int]], index: Int): Int = {
    val vertical: List[Int] = verticals(index)
    val oneCount = vertical.sum
    val zeroCount = vertical.size - oneCount
    if(oneCount == zeroCount){
      return 1
    }
    if(oneCount > zeroCount) return 1 else return 0
  }
  def getMostCommonBits(verticals: List[List[Int]]): List[Int] = {
    var mostCommonBits: List[Int] = List()
    for(vertical <- verticals){
      val oneCount = vertical.sum
      val zeroCount = vertical.size - oneCount
      var mostCommon = if(oneCount > zeroCount) 1 else 0
      if(oneCount == zeroCount){
        mostCommon = 1
      }
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
  def processBinaryStrings(lines: List[String]): List[List[Int]] = {
    lines.map(line =>
      line.map(binaryChar => if(binaryChar == '0') 0 else 1).toList
    ).toList
  }
  def transverseLines(lines: List[String]): List[List[Int]] = {
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
  println(new GetBitFrequency().part2)
}
