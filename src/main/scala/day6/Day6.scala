package day6

import util.FileUtil
import scala.collection.mutable.{Stack, Map}

// https://adventofcode.com/2022/day/6
@main
def myApp =
  val input = FileUtil
    .getLinesList(6)
    .head

  val groupingNumber = 4

  def findFirstNonDuplicateIndex(groupingNumber: Int): Int =
    val fourLetterGroups =
      input
        .toCharArray()
        .sliding(groupingNumber)
        .toList

    val lastDuplicate =
      fourLetterGroups
        .takeWhile(a =>
          if a.distinct.size == a.size
          then false
          else true
        )
        .last

    input.indexOf(lastDuplicate.mkString) + groupingNumber + 1

  println(s"The answer to part 1 is: ${findFirstNonDuplicateIndex(4)}")
  println(s"The answer to part 2 is: ${findFirstNonDuplicateIndex(14)}")
