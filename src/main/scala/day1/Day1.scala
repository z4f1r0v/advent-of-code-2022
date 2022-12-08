package day1

import util.FileUtil
import scala.collection.mutable.SortedSet

// https://adventofcode.com/2022/day/1
@main
def myApp =
  val maxCals = FileUtil
    .getLinesIterator(1)
    .foldLeft((0, SortedSet.empty[Int])) { case ((acc, ss), line) =>
      if line.isEmpty
      then {
        ss += acc
        (0, ss)
      }
      else (acc + line.toInt, ss)
    }._2

  println(s"Answer to part 1: ${maxCals.last}")
  println(s"Answer to part 2: ${maxCals.takeRight(3).sum}")

