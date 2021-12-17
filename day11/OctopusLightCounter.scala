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

class Turn(var grid: Array[Array[Int]]) {
  var flashCount = 0
  def getAdjacentIndexes(i: Int, j: Int): List[(Int, Int)] = {
    List(
      (i - 1, j - 1),
      (i - 1, j),
      (i - 1, j + 1),
      (i, j + 1),
      (i + 1, j + 1),
      (i + 1, j),
      (i + 1, j - 1),
      (i, j - 1)
    ).filter({ case (ai, aj) => isInArray(ai, aj) })
  }
  def getIndexesAbove9(): List[(Int, Int)] = {
    var above9: List[(Int, Int)] = List()
    for (i <- 0 until 10) {
      for (j <- 0 until 10) {
        if (grid(i)(j) > 9) {
          above9 = above9 :+ (i, j)
        }
      }
    }
    above9
  }
  def incrementAll(): Unit = {
    for (i <- 0 until 10) {
      for (j <- 0 until 10) {
        grid(i)(j) = grid(i)(j) + 1
      }
    }
  }
  def setToZero(i: Int, j: Int): Unit = {
    grid(i)(j) = 0
    flashCount = flashCount + 1
  }
  def increment(i: Int, j: Int): Unit = {
    val value = grid(i)(j)
    if (value > 0) {
      grid(i)(j) = grid(i)(j) + 1
    }
  }
  def isInArray(i: Int, j: Int): Boolean = {
    if (i < 0 || j < 0) {
      return false
    }
    if (i == 10 || j == 10) {
      return false
    }
    return true
  }
  def printReadyToFlash(): Unit = {
    println()
    for (i <- 0 until 10) {
      for (j <- 0 until 10) {
        if (grid(i)(j) > 9) {
          print("X")
        } else {
          print(grid(i)(j))
        }
      }
      println()
    }
  }
  def main(): Unit = {
    val readyToFlash: List[(Int, Int)] = getIndexesAbove9()
    readyToFlash.map({
      case (i, j) => {
        setToZero(i, j)
      }
    })
    val adjacents: List[List[(Int, Int)]] = readyToFlash.map({ case (i, j) =>
      getAdjacentIndexes(i, j)
    })
    adjacents.map(adjacent =>
      adjacent.map({ case (i, j) =>
        increment(i, j)
      })
    )
    val moreToFlash = getIndexesAbove9()
    if (moreToFlash.size > 0) main
  }
  def printGrid(): Unit = {
    println()
    for (i <- 0 until 10) {
      for (j <- 0 until 10) {
        print(grid(i)(j))
      }
      println()
    }
  }
  // only occurs once
  incrementAll()
  main()
  // printGrid()
}

object OctopusLightCounter extends App {
  def toGrid(lines: List[String]): Array[Array[Int]] = {
    var filling: Array[Array[Int]] = Array.ofDim[Int](lines.size, 0)
    for (lineIndex <- 0 until lines.size) {
      val line: String = lines(lineIndex)
      for (char <- line) {
        val value: Int = char.toString.toInt
        filling(lineIndex) = filling(lineIndex) :+ value
      }
    }
    filling
  }
  val lines = new InputReader(false).getLines
  val grid: Array[Array[Int]] = toGrid(lines)
  var updatedGrid = grid
  var total = 0
  for (i <- 0 until 100000) {
    val turn = new Turn(updatedGrid)
    updatedGrid = turn.grid
    total = total + turn.flashCount
    if (turn.flashCount == 100) {
      println(i + 1)
    }
  }
  // println(total)
}
