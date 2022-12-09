package day2

import util.FileUtil

// https://adventofcode.com/2022/day/2
@main
def myApp =
  def totalScore(calculatePoints: (String, String) => Int): Int =
    FileUtil
      .getLinesIterator(2)
      .map(l =>
        val moves = l.split(" ")
        calculatePoints(moves(0), moves(1))
      )
      .sum

  val part1Score = totalScore(part1PointCalculation)
  println(s"Answer to part 1 is: $part1Score")

  val part2Score = totalScore(part2PointCalculation)
  println(s"Answer to part 2 is: $part2Score")

def part1PointCalculation(opponentMove: String, playerMove: String): Int =
  val playerPoints =
    if playerMove == "X" then 1
    else if playerMove == "Y" then 2
    else 3

  val calculateWin =
    (opponentMove, playerMove) match {
      case ("B", "X") | ("C", "Y") | ("A", "Z") => 0
      case ("A", "X") | ("B", "Y") | ("C", "Z") => 3
      case ("A", "Y") | ("B", "Z") | ("C", "X") => 6
    }

  playerPoints + calculateWin

def part2PointCalculation(opponentMove: String, outcome: String): Int =
  (opponentMove, outcome) match {
    case ("A", "X") => 3 // 3 + 0
    case ("B", "X") => 1 // 1 + 0
    case ("C", "X") => 2 // 2 + 0
    case ("A", "Y") => 4 // 1 + 3
    case ("B", "Y") => 5 // 2 + 3
    case ("C", "Y") => 6 // 3 + 3
    case ("A", "Z") => 8 // 2 + 6
    case ("B", "Z") => 9 // 3 + 6
    case ("C", "Z") => 7 // 1 + 6
  }
