import scala.io.Source._
import scala.collection.mutable._

class InputReader(isTest: Boolean = true) {
  private val testPath = "input.txt"
  private val prodPath = "inputLarge.txt"
  private val pathToUse = if (isTest) testPath else prodPath
  private val source = scala.io.Source.fromFile(pathToUse)
  private val lines = source.getLines.toList
  val crabs = lines(0).split(",").map(_.toInt).toList
}

object Crabs extends App {
  def calculateMovementCost(movementSize: Int): Int = {
    var cost = 0
    var increment = 1
    for (i <- 0 until movementSize) {
      cost = cost + increment
      increment = increment + 1
    }
    cost
  }
  val crabs = new InputReader(false).crabs.toList.sorted
  val max = crabs.max
  val fixations = (0 to max).toList
  val minFixation = fixations
    .map(fixation =>
      crabs.map(crab => calculateMovementCost(Math.abs(fixation - crab))).sum
    )
    .min
  println(minFixation)
}
