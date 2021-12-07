import scala.io.Source._
import scala.collection.mutable._

case class Coordinate(x1: Int, y1: Int, x2: Int, y2: Int)

class InputReader(isTest: Boolean = true) {
  val testPath = "input.txt"
  val prodPath = "inputLarge.txt"
  val pathToUse = if (isTest) testPath else prodPath
  def getLines(): List[String] = {
    val source = scala.io.Source.fromFile(pathToUse)
    val lines = source.getLines.toList
    lines
  }
  val lines = getLines()
  val coordinates = lines
    .map(line => {
      val lr = line.split(" -> ")
      val left = lr(0)
      val right = lr(1)
      val leftxy = left.split(",")
      val rightxy = right.split(",")
      val x1 = leftxy(0).toInt
      val y1 = leftxy(1).toInt
      val x2 = rightxy(0).toInt
      val y2 = rightxy(1).toInt
      Coordinate(x1, y1, x2, y2)
    })
    .toList
}

class BadMath {
  def getCoordinateChange(
      x: Int,
      y: Int,
      coordinate: Coordinate
  ): (Int, Int) = {
    val xDirection = (coordinate.x2 - coordinate.x1)
    val yDirection = (coordinate.y2 - coordinate.y1)
    // positive x means we go right
    // positive y means we go down
    if (xDirection > 0 && yDirection > 0) {
      return (x + 1, y + 1)
    } else if (xDirection > 0 && yDirection < 0) {
      // right and up
      return (x + 1, y - 1)
    } else if (xDirection < 0 && yDirection > 0) {
      // left down
      return (x - 1, y + 1)
    } else if (xDirection < 0 && yDirection < 0) {
      // left up
      return (x - 1, y - 1)
    }
    throw new Exception("This case should not occur")
  }
  def getSlope(coordinate: Coordinate): Int = {
    val ydiff = (coordinate.y2 - coordinate.y1)
    val xdiff = (coordinate.x2 - coordinate.x1)
    if (xdiff == 0) return 0
    return ydiff / xdiff
  }
  def isSlope45(coordinate: Coordinate): Boolean = {
    val slope = getSlope(coordinate)
    slope.equals(1) || slope.equals(-1)
  }
  val coordinates = new InputReader(false).coordinates
  val correctlyOrderedCoordinates = coordinates
  val acceptedLines = correctlyOrderedCoordinates.filter(coordinate =>
    coordinate.x1.equals(coordinate.x2) || coordinate.y1.equals(coordinate.y2)
      || isSlope45(coordinate)
  )
  val maxX =
    acceptedLines.map(coordinate => Math.max(coordinate.x1, coordinate.x2)).max
  val maxY =
    acceptedLines.map(coordinate => Math.max(coordinate.y1, coordinate.y2)).max
  val matrix = Array.ofDim[Int](maxX + 1, maxY + 1)
  for (coordinate <- acceptedLines) {
    if (!isSlope45(coordinate)) {
      for (
        y <- Math.min(coordinate.y1, coordinate.y2) to Math.max(
          coordinate.y1,
          coordinate.y2
        )
      ) {
        for (
          x <- Math.min(coordinate.x1, coordinate.x2) to Math.max(
            coordinate.x1,
            coordinate.x2
          )
        ) {
          matrix(y)(x) = matrix(y)(x) + 1
        }
      }
    } else {
      var x = coordinate.x1
      var y = coordinate.y1
      var oldX = x
      do {
        matrix(y)(x) = matrix(y)(x) + 1
        val (newX, newY) = getCoordinateChange(x, y, coordinate)
        oldX = x
        x = newX
        y = newY
      } while (oldX != coordinate.x2)
    }
  }
  def countMatrixOverlaps(): Int = {
    var counter = 0
    for (y <- 0 until matrix(0).size) {
      for (x <- 0 until matrix.size) {
        val value = matrix(y)(x)
        if (value > 1) {
          counter = counter + 1
        }
      }
    }
    // visualize matrix
    // matrix foreach { row => row foreach print; println }
    counter
  }
}

object BadMath extends App {
  val scribbles = new BadMath()
  val overlaps = scribbles.countMatrixOverlaps()
  println(overlaps)
}

// 18571 not correct
