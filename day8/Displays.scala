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
  def getDelimitedLine(): List[(String, String)] = {
    this
      .getLines()
      .map(line => {
        val spl = line.split(""" \| """)
        if (spl.size > 1) (spl(0), spl(1)) else ("", "")
      })
      .toList
  }
}

object Displays extends App {
  val allLetters = "abcdefg"
  val segments: HashMap[Int, String] = HashMap(
    0 -> "abc-efg",
    1 -> "--c--f-",
    2 -> "a-cde-g",
    3 -> "a-cd-fg",
    4 -> "-bcd-f-",
    5 -> "ab-d-fg",
    6 -> "ab-defg",
    7 -> "a-c--f-",
    8 -> "abcdefg",
    9 -> "abcd-fg"
  )
  val numberOfSegments: HashMap[Int, Int] = segments.map(
    { case (k, v) => (k, v.replace("-", "").size) }
  )
  val delimitedLines: List[(String, String)] = new InputReader(
    false
  ).getDelimitedLine
  var results: List[Int] = List()
  for (lines <- delimitedLines) {
    val input = lines._1
    val letterCombos: List[String] = input.split(" ").toList
    // For each group we have a list of possible answers
    val numbers: List[Int] = (0 until 9).toList
    val groupsForEachNumber: Map[Int, List[String]] = numbers
      .map(number =>
        (
          number,
          letterCombos
            .filter(letters => numberOfSegments(number) == letters.size)
            .map(_.permutations)
            .flatten
            .map(toDashForm(_, number))
        )
      )
      .toMap[Int, List[String]]
    // println(groupsForEachNumber)
    var validSequence: List[Char] = Nil
    for (a <- allLetters) {
      for (b <- allLetters) {
        for (c <- allLetters) {
          for (d <- allLetters) {
            for (e <- allLetters) {
              for (f <- allLetters) {
                for (g <- allLetters) {
                  if (isValidCombination(a, b, c, d, e, f, g)) {
                    val validMappings: Map[Int, List[String]] =
                      groupsForEachNumber.map({ case (k, v) =>
                        (
                          k,
                          v.filter(letterPermutation =>
                            (letterPermutation(0) == a || letterPermutation(
                              0
                            ) == '-') &&
                              (letterPermutation(1) == b || letterPermutation(
                                1
                              ) == '-') &&
                              (letterPermutation(2) == c || letterPermutation(
                                2
                              ) == '-') &&
                              (letterPermutation(3) == d || letterPermutation(
                                3
                              ) == '-') &&
                              (letterPermutation(4) == e || letterPermutation(
                                4
                              ) == '-') &&
                              (letterPermutation(5) == f || letterPermutation(
                                5
                              ) == '-') &&
                              (letterPermutation(6) == g || letterPermutation(
                                6
                              ) == '-')
                          )
                        )
                      })
                    val countOfValidNumbers = validMappings
                      .map({ case (k, v) =>
                        if (v.size >= 1) 1 else 0
                      })
                      .sum
                    if (countOfValidNumbers >= 9) {
                      // awesome win.  Now the goal is to count number of 1,4,7, and 8s.
                      // so translate each letterGroup in the output to a number.
                      validSequence = List(a, b, c, d, e, f, g)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    val output = lines._2
    val outputLetterGroups: List[String] = output.split(" ").toList
    var result: List[Int] = List()
    for (outputLetterGroup <- outputLetterGroups) {
      val number =
        translateLetterGroupToNumber(outputLetterGroup, validSequence)
      result = result :+ number
    }
    results = results :+ toNumber(result)
  }
  println(results.sum)
  def toNumber(base10Digits: List[Int]): Int = {
    var summation = 0
    for (baseIndex <- 0 until base10Digits.size) {
      summation = 10 * summation + base10Digits(baseIndex)
    }
    summation
  }
  def isNumberValid(asDash: String, letterMappings: List[Char]): Boolean = {
    for (letterIndex <- 0 until asDash.size) {
      if (
        !(letterMappings(letterIndex) == asDash(letterIndex) || asDash(
          letterIndex
        ) == '-')
      ) {
        return false
      }
    }
    return true
  }
  def translateLetterGroupToNumber(
      letterGroup: String,
      letterMappings: List[Char]
  ): Int = {
    val validNumbers = numberOfSegments
      .filter({ case (k, v) => v == letterGroup.size })
      .map({ case (k, v) => k })
    for (validNumber <- validNumbers) {
      for (permutation <- letterGroup.permutations) {
        val asDash = toDashForm(permutation, validNumber)
        if (isNumberValid(asDash, letterMappings)) {
          return validNumber
        }
      }
    }
    throw new Exception(
      "No valid number found " + letterGroup + " " + validNumbers
    )
  }
  def toDashForm(letterGroup: String, number: Int): String = {
    val numberDashes = segments(number)
    var letterToBuild: List[Char] = List()
    var letterIndex = 0
    for (example <- numberDashes) {
      if (example.equals('-')) {
        letterToBuild = letterToBuild :+ '-'
      } else {
        letterToBuild = letterToBuild :+ letterGroup(letterIndex)
        letterIndex = letterIndex + 1
      }
    }
    letterToBuild.mkString("")
  }
  def isValidCombination(
      a: Char,
      b: Char,
      c: Char,
      d: Char,
      e: Char,
      f: Char,
      g: Char
  ): Boolean = {
    val values = List(a, b, c, d, e, f, g)
    for (i <- 0 until values.size) {
      for (j <- 0 until values.size) {
        if (i != j) {
          if (values(i).equals(values(j))) {
            return false
          }
        }
      }
    }
    return true
  }  
}
