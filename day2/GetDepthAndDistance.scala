import scala.io.Source._
import scala.collection.immutable._

case class Instruction(direction: String, amount: Int)

class GetDepthAndDistance {
  def getInstructions(path :String): Queue[Instruction] = {
    val lines = getLines(path)
    val instructions = asInstructions(lines)
    instructions
  }
/*  def main(args: Array[String]): Unit = {
    val instructions = getInstructions("./input.txt")
    val vertical = getDown(instructions) - getUp(instructions)
    val horizontal = getHorizontal(instructions)
    val both = vertical * horizontal
  }*/
  // returns the updated aim and depth
  def getChange(instructions: Queue[Instruction], aim: Int = 0, depth: Int = 0, horizontal: Int = 0): Int = {
    if(instructions.isEmpty){
      return (horizontal * depth)
    }
    val (instruction, cdr) = instructions.dequeue
    val direction: String = instruction.direction
    direction match {
      case "up" => getChange(cdr, aim - instruction.amount, depth, horizontal)
      case "down" => getChange(cdr, aim + instruction.amount, depth, horizontal)
      case "forward" => getChange(cdr, aim, depth + instruction.amount * aim, horizontal + instruction.amount)
      case _ => throw new Exception("This case should not exist")
    }
  }
  def part2(): Int = {
    val instructions = getInstructions("./inputLarge.txt")
    getChange(instructions)
  }
  def getUp(instructions: List[Instruction]): Int = {
    instructions.filter(_.direction == "up").map(_.amount).sum
  }
  def getDown(instructions: List[Instruction]): Int = {
    instructions.filter(_.direction == "down").map(_.amount).sum
  }  
  def getHorizontal(instructions: List[Instruction]): Int = {
    instructions.filter(_.direction == "forward").map(_.amount).sum
  }
  def getLines(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines.toList
    lines
  }
  def asInstructions(lines: List[String]): Queue[Instruction] = {
    lines.map{ line =>
      val parts = line.split(" ")
      val instruction = Instruction(parts(0), parts(1).toInt)
      instruction
    }.toList.to(collection.immutable.Queue)
  }
}

object GetDepthAndDistance extends App {
  val answer = new GetDepthAndDistance().part2()
  println(answer)
}

