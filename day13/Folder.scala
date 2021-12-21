import scala.io.Source._
import scala.collection.mutable._

class InputReader(isTest: Boolean = true, testCase: Int = 0) {
  val testPath = testCase match {
    case 0 => "input.txt"
  }
  val prodPath = "inputLarge.txt"
  val pathToUse = if (isTest) testPath else prodPath
  val lines = getLines()
  val points = lines.map(toPoints(_)).filter(_ != null)
  val folds = getFolds
  def getFoldInstructionStart(): Int = {
    for (lineIndex <- 0 until lines.size) {
      val line = lines(lineIndex)
      if (line.contains("fold")) {
        return lineIndex
      }
    }
    throw new Exception("line index not found!")
  }
  def getFolds(): List[(String, Int)] = {
    var instructions: List[(String, Int)] = List()
    val foldRegex = """.+([x|y])=(\d+)""".r
    val startingLine = getFoldInstructionStart()
    for (lineIndex <- startingLine until lines.size) {
      val line = lines(lineIndex)
      val foldRegex(direction, index) = line
      instructions = instructions :+ (direction, index.toInt)
    }
    instructions
  }
  def getLines(): List[String] = {

    val source = scala.io.Source.fromFile(pathToUse)
    val lines = source.getLines.toList
    lines
  }
  def toPoints(point: String): (Int, Int) = {
    val commanSeparated = point.split(',')
    if (commanSeparated.size == 2) {
      return (commanSeparated(0).toInt, commanSeparated(1).toInt)
    }
    null
  }
}

class Folder(val points: List[(Int, Int)]) {
  val grid = createArray()
  def findArraySize(): (Int, Int) = {
    val xMax = points.map(_._1).max + 1
    val yMax = points.map(_._2).max + 1
    (xMax, yMax)
  }
  def createArray(): Array[Array[Int]] = {
    val (horizontalSize, verticalSize) = findArraySize()
    var filling: Array[Array[Int]] =
      Array.ofDim[Int](verticalSize, horizontalSize)
    for (point <- points) {
      val (x, y) = point
      filling(y)(x) = 1
    }
    filling
  }
  def printArray(): Unit = {
    for (i <- 0 until grid.size) {
      for (j <- 0 until grid(0).size) {
        print(grid(i)(j))
      }
      println()
    }
  }

  //printArray()
}

object Folds {
  def verticalInverse(bottom: Array[Array[Int]]): Array[Array[Int]] = {
    var filling: Array[Array[Int]] =
      Array.ofDim[Int](bottom.size, bottom(0).size)
    for (i <- 0 until bottom.size) {
      for (j <- 0 until bottom(0).size) {
        filling(bottom.size - 1 - i)(j) = bottom(i)(j)
      }
    }
    filling
  }
  def horizontalInverse(bottom: Array[Array[Int]]): Array[Array[Int]] = {
    var filling: Array[Array[Int]] =
      Array.ofDim[Int](bottom.size, bottom(0).size)
    for (i <- 0 until bottom.size) {
      for (j <- 0 until bottom(0).size) {
        filling(i)(bottom(0).size - 1 - j) = bottom(i)(j)
      }
    }
    filling
  }
  def addArrays(
      top: Array[Array[Int]],
      bottomFlipped: Array[Array[Int]]
  ): Array[Array[Int]] = {
    var filling: Array[Array[Int]] =
      Array.ofDim[Int](bottomFlipped.size, bottomFlipped(0).size)
    for (i <- 0 until bottomFlipped.size) {
      for (j <- 0 until bottomFlipped(0).size) {
        filling(i)(j) = Math.max(bottomFlipped(i)(j), top(i)(j))
      }
    }
    filling
  }
  def printArray(toPrint: Array[Array[Int]]): Unit = {
    println()
    for (i <- 0 until toPrint.size) {
      for (j <- 0 until toPrint(0).size) {
        val value = toPrint(i)(j)
        val charToPrint = value match {
          case 1 => "#"
          case 0 => "."
        }
        print(charToPrint)
      }
      println()
    }
    println()
  }
}

class VerticalFold(val grid: Array[Array[Int]], yFold: Int) {
  def createTopFold(): Array[Array[Int]] = {
    val (verticalSize, horizontalSize) = (yFold, grid(0).size)
    var filling: Array[Array[Int]] =
      Array.ofDim[Int](verticalSize, horizontalSize)
    for (i <- 0 until filling.size) {
      for (j <- 0 until filling(0).size) {
        filling(i)(j) = grid(i)(j)
      }
    }
    filling
  }
  def createBottomFold(): Array[Array[Int]] = {
    val verticalSize = yFold
    val horizontalSize = grid(0).size
    var filling: Array[Array[Int]] =
      Array.ofDim[Int](verticalSize, horizontalSize)
    for (i <- yFold + 1 until grid.size) {
      for (j <- 0 until filling(0).size) {
        filling(i - yFold - 1)(j) = grid(i)(j)
      }
    }
    filling
  }
  val topFold = createTopFold()
  val bottomFold = createBottomFold()
  val bottomFlipped = Folds.verticalInverse(bottomFold)
  val verticallyFolded = Folds.addArrays(bottomFlipped, topFold)
  //printArray(verticallyFolded)
  println("vertical: ", verticallyFolded.map(y => y.sum).sum)
}

class RightFold(val grid: Array[Array[Int]], xFold: Int) {
  def createLeftFold(): Array[Array[Int]] = {
    val (verticalSize, horizontalSize) = (grid.size, xFold)
    var filling: Array[Array[Int]] =
      Array.ofDim[Int](verticalSize, horizontalSize)
    for (i <- 0 until filling.size) {
      for (j <- 0 until filling(0).size) {
        filling(i)(j) = grid(i)(j)
      }
    }
    filling
  }
  def createRightFold(): Array[Array[Int]] = {
    val verticalSize = grid.size
    val horizontalSize = grid(0).size - xFold - 1
    var filling: Array[Array[Int]] =
      Array.ofDim[Int](verticalSize, horizontalSize)
    for (i <- 0 until filling.size) {
      for (j <- xFold + 1 until grid(0).size) {
        filling(i)(j - xFold - 1) = grid(i)(j)
      }
    }
    filling
  }
  def printArray(toPrint: Array[Array[Int]]): Unit = {
    println()
    for (i <- 0 until toPrint.size) {
      for (j <- 0 until toPrint(0).size) {
        val value = toPrint(i)(j)
        val charToPrint = value match {
          case 1 => "#"
          case 0 => "."
        }
        println(charToPrint)
      }
      println()
    }
    println()
  }
  val leftFold = createLeftFold()
  val rightFold = createRightFold()
  val rightFlipped = Folds.horizontalInverse(rightFold)
  val horizontallyFlipped = Folds.addArrays(rightFlipped, leftFold)
  //printArray(horizontallyFlipped)
  println("horizontal: ", horizontallyFlipped.map(y => y.sum).sum)
}

object Main extends App {
  val reader = new InputReader(false)
  val folder = new Folder(reader.points)
  var grid = folder.grid
  for ((direction, index) <- reader.folds) {
    println("grid size: x: ", grid(0).size, "y: ", grid.size)
    println("fold direction: ", direction)
    println("fold amount: ", index)
    direction match {
      case "x" => {
        val rightFold = new RightFold(grid, index)
        grid = rightFold.horizontallyFlipped
      }
      case "y" => {
        val verticalFold = new VerticalFold(grid, index)
        grid = verticalFold.verticallyFolded
      }
      case _ => throw new Exception("this should not have happened")
    }
  }
  Folds.printArray(grid)

}
