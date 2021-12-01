import scala.io.Source._
val source = scala.io.Source.fromFile("inputLarge.txt")
//val source = scala.io.Source.fromFile("input.txt")
val lines = source.getLines.toList
val numbers = lines.map(_.toInt).toList

// Part A

var depthIncreases = 0
var previousDepth = Integer.MAX_VALUE
for(line <- lines){
  val depth = line.toInt
  if(depth > previousDepth){
    depthIncreases = depthIncreases + 1
  }
  previousDepth = depth
}
println(depthIncreases)

var counter = 0
for(i <- 1 until lines.size){
  val first = lines(i-1).toInt
  val second = lines(i).toInt
  if(second > first){
    counter = counter + 1
  }
}
println(counter)

// Part B

def getTriplet(i: Int, lines: List[Int]): Int = {
  val first = lines(i-2).toInt
  val second = lines(i - 1).toInt
  val third = lines(i).toInt
  return first + second + third
}

var counter = 0
for(i <- 3 until lines.size){
  val first = getTriplet(i-1, numbers)
  val second = getTriplet(i, numbers)
  if(second > first){
    counter = counter + 1
  }
}
println(counter)
