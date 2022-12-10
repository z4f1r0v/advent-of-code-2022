package day4

import util.FileUtil

// https://adventofcode.com/2022/day/4
@main
def myApp =

  def countRanges(part: Int, f: (Int, Int, Int, Int) => Boolean): Unit =
    val count = FileUtil
      .getLinesIterator(4)
      .map(l =>
        val assignments = l
          .split(",")
          .map(a =>
            val pairs = a.split("-")
            (pairs(0).toInt, pairs(1).toInt)
          )
        val (firstStart, firstEnd) = assignments(0)
        val (secondStart, secondEnd) = assignments(1)
        if f(firstStart, firstEnd, secondStart, secondEnd)
        then 1
        else 0
      )
      .sum

    println(s"The answer to part $part is: $count")

  val fullyContainedRanges = (
      firstStart: Int,
      firstEnd: Int,
      secondStart: Int,
      secondEnd: Int
  ) =>
    (firstStart <= secondStart && firstEnd >= secondEnd) || (firstStart >= secondStart && firstEnd <= secondEnd)

  countRanges(part = 1, f = fullyContainedRanges)

  val overlappingRanges = (
      firstStart: Int,
      firstEnd: Int,
      secondStart: Int,
      secondEnd: Int
  ) => !(firstEnd < secondStart || secondEnd < firstStart)

  countRanges(part = 2, f = overlappingRanges)
