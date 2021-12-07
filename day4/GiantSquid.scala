import scala.io.Source._
import scala.collection.mutable._

case class Board(lines: List[List[Int]])

class InputReader(isTest: Boolean = true) {
  private val testPath = "input.txt"
  private val prodPath = "inputLarge.txt"
  private val pathToUse = if (isTest) testPath else prodPath
  private val source = scala.io.Source.fromFile(pathToUse)
  private val lines = source.getLines.toList
  val calls = lines(0).split(",").map(_.toInt).toList
  val boards = getBoards()

  def getBoards(): List[Board] = {
    val bingoLines = getHorizontalBingoLines(lines)
    val horizontalBoards = convertToBoards(bingoLines)
    val verticalBoards = inverseBoards(horizontalBoards)
    val boards = horizontalBoards ++ verticalBoards
    boards
  }

  def inverseBoards(boards: List[Board]): List[Board] = {
    boards.map(board =>
      Board(
        List
          .range(0, board.lines(0).size)
          .map(lineIndex => {
            board.lines.map(line => line(lineIndex)).toList
          })
          .toList
      )
    )
  }

  def convertToBoards(
      bingoLines: List[List[Int]],
      boardIndex: Int = 1
  ): List[Board] = {
    val currentBoard = List(
      Board(bingoLines.slice((boardIndex - 1) * 5, boardIndex * 5))
    )
    if (boardIndex * 5 == bingoLines.size) {
      return currentBoard
    } else {
      return currentBoard ++ convertToBoards(bingoLines, boardIndex + 1)
    }
  }
  def getHorizontalBingoLines(lines: List[String]): List[List[Int]] = {
    lines
      .slice(1, lines.size)
      .filter(_.size > 0)
      .map(line => {
        val splitNumbers = line.split("\\s+").filter(_.size > 0)
        splitNumbers.map(_.toInt).toList
      })
      .toList
  }
}

class GameInfo {
  val reader = new InputReader(false)
  val calls = reader.calls
  val boards = reader.boards
}

object GameInfo extends App {
  val gameInfo = new GameInfo
  def callNumber(number: Int, boards: List[Board]): List[Board] = {
    // replace the called number with Integer.MIN_VALUE
    boards.map(board =>
      Board(
        board.lines.map(line =>
          line.map(value =>
            if (value.equals(number)) Integer.MIN_VALUE else value
          )
        )
      )
    )
  }
  def didBoardWin(board: Board): Boolean = {
    // look for a row of all Integer.MIN_VALUE
    board.lines.filter(_.filter(_.equals(Integer.MIN_VALUE)).size == 5).size > 0
  }
  def getBoardScore(board: Board): Int = {
    // replace Integer.MIN_VALUE with 0 and sum all lists
    board.lines
      .map(line =>
        line
          .map(number => if (number.equals(Integer.MIN_VALUE)) 0 else number)
          .sum
      )
      .sum
  }
  def main(calls: List[Int] = null, boards: List[Board] = null): Int = {
    // start the recursion!
    if (calls == null) return main(gameInfo.calls, gameInfo.boards)
    // check for an error
    if (calls.isEmpty) throw new Exception("Eternal Bingo Game, YIKES")

    val call = calls.head
    val rest = calls.tail

    // you can remove all boards with the winning score
    val updatedBoards = callNumber(call, boards)
    val winningBoards = updatedBoards.filter(didBoardWin(_))
    val winningBoardsRawScores = winningBoards.map(getBoardScore(_))
    val leftoverBoards = updatedBoards.filter(board => {
      val score = getBoardScore(board)
      !winningBoardsRawScores.contains(score)
    })
    if (leftoverBoards.size == 0 && winningBoards.size == 2) {
      val scores = winningBoards.map(getBoardScore(_) * call)
      println(scores)
    }
    return main(rest, leftoverBoards)
  }
  val winningScore = main() // so clean
  println(winningScore)
}
