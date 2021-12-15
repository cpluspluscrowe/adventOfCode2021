import scala.io.Source._
import scala.collection.immutable._

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

class LowpointFinder {
  def toGrid(lines: List[String]): Array[Array[Int]] = {
    var filling: Array[Array[Int]] = Array.ofDim[Int](lines.size, 0)
    for(lineIndex <- 0 until lines.size){
      val line: String = lines(lineIndex)
      for(char <- line){
        val value: Int = char.toString.toInt
        filling(lineIndex) = filling(lineIndex) :+ value
      }
    }
    filling
  }
  def print2dArray(array: Array[Array[Int]]): Unit = {
    for(i <- 0 until array.size){
      for(j <- 0 until array(i).size){
        print(array(i)(j))
      }
      println()
    }
  }
  def isLowpoint(i: Int, j: Int, array: Array[Array[Int]]): Boolean = {
    val value = array(i)(j)
    val above = if(i == 0) Integer.MAX_VALUE else array(i-1)(j)
    val below = if(i >= array.size - 1) Integer.MAX_VALUE else array(i+1)(j)    
    val right = if(j >= array(i).size - 1) Integer.MAX_VALUE else array(i)(j + 1)
    val left = if(j == 0) Integer.MAX_VALUE else array(i)(j - 1)
    val isLowpoint = value < above && value < below && value < right && value < left
    return isLowpoint
  }
  def getLowpoints(grid: Array[Array[Int]]): List[Int] = {
    var lowpoints: List[Int] = List()
    for(i <- 0 until grid.size){
      for(j <- 0 until grid(i).size) {
        if(isLowpoint(i,j,grid)){
          lowpoints = lowpoints :+ grid(i)(j)
        }
      }
    }
    lowpoints
  }
  def indexesToString(i: Int, j:Int): String = {
    i.toString + j.toString
  }
  def getBasin(i: Int, j: Int, array: Array[Array[Int]], history: Set[String]): Set[String] = {
    // only operate on a valid index
    if(i < 0 || i > array.size - 1 || j < 0 || j > array(0).size - 1){
      return return history
    }
    // also exclude 9
    val value = array(i)(j)
    if(value == 9) return history

    // also don't repeat ourselves
    val key: String = indexesToString(i,j)
    if(history.contains(key)) return history

    // now reach out in all directions
    val newHistory: Set[String] = history + key
    val above = getBasin(i-1, j, array, newHistory)
    val below = getBasin(i+1, j, array, above)
    val right = getBasin(i, j+1, array, below)
    val left = getBasin(i, j-1, array, right)
  return left
  }
  def printBasin(basin: List[String], array: Array[Array[Int]]): Unit = {
    for(i <- 0 until array.size){
      for(j <- 0 until array(i).size){
        val key = indexesToString(i,j)
        if(basin.contains(key)) print(array(i)(j)) else print(9)
      }
      println()
    }
  }
}
// store string in hashset and grow the hashset
object LowpointFinder extends App {
  val lines = new InputReader(false).getLines
  val lowpointFinder = new LowpointFinder()
  val grid = lowpointFinder.toGrid(lines)//.map(_.sum).sum
  // lowpointFinder.print2dArray(grid)
  val lowpoints = lowpointFinder.getLowpoints(grid)
  var basins: List[List[String]] = List()
  var basinHistory: Set[String] = basins.flatten.toSet
  for(i <- 0 until grid.size){
    for(j <- 0 until grid(i).size){
      val key = lowpointFinder.indexesToString(i,j)
      if(!basinHistory.contains(key)){
      val value = grid(i)(j)
      if(value != 9){
        val basin = lowpointFinder.getBasin(i,j,grid, new HashSet[String]())
        println(basin.size)
        //println(basin.distinct.size, basin)
        //lowpointFinder.printBasin(basin, grid)
        basins = basins :+ basin.toList.sorted
        basinHistory = basins.flatten.toSet
      }
      }
    }
  }
  basins = basins.distinct
  val sizes = basins.map(_.size).sorted.reverse
  println(sizes)
  println(sizes(0) * sizes(1) * sizes(2))
}
