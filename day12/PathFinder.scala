
import scala.io.Source._
import scala.collection.mutable._

class InputReader(isTest: Boolean = true, testCase: Int = 0) {
  val testPath = testCase match {
    case 0 => "input.txt"
    case 1 => "input2.txt"
    case 2 => "input3.txt"
  }
  val prodPath = "inputLarge.txt"
  val pathToUse = if (isTest) testPath else prodPath
  def getLines(): List[String] = {
    val source = scala.io.Source.fromFile(pathToUse)
    val lines = source.getLines.toList
    lines
  }
}

class PathFinder(val lines: List[String]) {
  val onewayPoints = lines.map(getEndpoints(_))
  val points: List[(String, String)] = createPathsBothWays(onewayPoints)
  val paths = createTravelMap(points)
  val exhaustivePathways: List[List[String]] = buildPath("start", List[String]()).distinct
  def getEndpoints(line: String): (String, String) = {
    val parts = line.split("-")
    (parts(0), parts(1))
  }
  def createPathsBothWays(points: List[(String, String)]): List[(String, String)] = {
    val oppositeDirection = points.map({case (a,b) => (b,a)}).toList
    points ++ oppositeDirection
  }
  def createTravelMap(points: List[(String, String)]): Map[String, List[String]] = {
    var paths: HashMap[String, List[String]] = HashMap()
    for(point <- points){
      val start = point._1
      val end = point._2
      if(!paths.contains(start)){
        paths(start) = List[String]()
      }
      paths(start) = paths(start) :+ end
    }
    paths
  }
  def isLower(path: String): Boolean = {
    if(path.size == 0) return false
    if(path == "end") return false // edge case
    if(path(0).toInt >= 97) return true else false // assume we're only dealing with the alphabet
  }
  def countHistory(path: String, history: List[String]): Int = {
    history.filter(_.equals(path)).size
  }
  def buildPath(currentLocation: String, history: List[String], hasVisitedSmallCaveTwice: Boolean = false): List[List[String]] = {
    if(currentLocation == "end") return List(history :+ "end")
    var possiblePaths: List[List[String]] = List()
    for(potentialNextStep <- paths(currentLocation)){
      val isLowercase = isLower(potentialNextStep)
      val historyCount = countHistory(potentialNextStep, history)
      if(potentialNextStep == "start"){
        // skip
      }
      else if(isLowercase){
        if(historyCount == 0){
          possiblePaths = possiblePaths ++ buildPath(potentialNextStep, history :+ currentLocation, hasVisitedSmallCaveTwice)
        }
        else if(historyCount == 1){
          // run if first visit
          if(hasVisitedSmallCaveTwice == false){
            possiblePaths = possiblePaths ++ buildPath(potentialNextStep, history :+ currentLocation, true)
          }
        } else if(historyCount == 2){
          // skip
        }
      }else{ // uppercase, always run
        possiblePaths = possiblePaths ++ buildPath(potentialNextStep, history :+ currentLocation, hasVisitedSmallCaveTwice)
      }
    }
    possiblePaths
  }
}

object PathFinder extends App {
  val lines = new InputReader(false).getLines
  val pathFinder = new PathFinder(lines)
  println(pathFinder.exhaustivePathways.size)
  for(path <- pathFinder.exhaustivePathways){
    //println(path)
  }
}
