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

object Lantern {
  var daysDp: HashMap[Int, Double] = HashMap()
}

class Lantern(val initialAge: Int = 9) {
  var age = initialAge
  def dayLapse(): List[Lantern] = {
    if (age == 0) {
      age = 6
      return List(new Lantern(8), this)
    }
    age = age - 1
    return List(this)
  }
  def calculateLanternCount(days: Int): Double = {
    if(initialAge == 9){
      if(Lantern.daysDp.contains(days)) return Lantern.daysDp(days)
    }
    var count = 1D
    val goal = days - initialAge
    val start = -initialAge
    for (dayIndex <- start until goal){
      if(dayIndex >= 0){
        if(dayIndex % 7 == 0){
          count = count + new Lantern().calculateLanternCount(days + start - dayIndex)
        }
      }
    }
    if(initialAge == 9){
      Lantern.daysDp += (days -> count)
    }
    count
  }
}

class CountLanterns(days: Int) {
  val ages = new InputReader(false).getLines()(0).split(",").map(_.toInt)
  val lanterns = ages.map(new Lantern(_)).toList
  def modelTime(
      lanterns: List[Lantern] = null,
      dayIndex: Int = 0
  ): List[Lantern] = {
    if (lanterns == null) {
      return modelTime(this.lanterns)
    }
    // base case
    if (dayIndex == days) {
      return lanterns
    }
    // println(lanterns.map(_.age))
    return modelTime(lanterns.map(_.dayLapse).flatten, dayIndex + 1)
  }
}

object CountLanterns extends App {
  val lanterns = new CountLanterns(80).lanterns//.map(_.calculateOffspring())
  var lanterns2 = new CountLanterns(80).lanterns//.map(_.calculateOffspring())
  for(i <- 0 to 256){
    val count = lanterns.map(_.calculateLanternCount(i)).sum
//    lanterns2 = lanterns2.map(_.dayLapse()).flatten
//    val count2 = lanterns2.size
    //println(i,count, count2)    
    println(i,new java.math.BigDecimal(count).toPlainString)
  }
  //println(lanterns)
  //println(lanterns)
//  val lantern = new Lantern(3)
//  println(lantern.calculateOffspring(80))
}
