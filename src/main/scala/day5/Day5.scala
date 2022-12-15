package day5

import util.FileUtil
import scala.collection.mutable.{Stack, Map}

// https://adventofcode.com/2022/day/5
@main
def myApp =
  val lines = FileUtil.getLinesIterator(5)
  val stackIndices = List.iterate(1, 9)(_ + 4)
  val stackIndexMap = (1 to 9).zip(stackIndices).toMap

  val startingStacks: Map[Int, Vector[Char]] =
    lines
      .take(8)
      .foldLeft(Map.empty[Int, Vector[Char]])((acc, l) =>
        stackIndexMap.foreach((index, stackNumber) =>
          val stackChar = l.charAt(stackNumber)
          if stackChar != ' '
          then
            if acc.contains(index)
            then acc.update(index, acc(index) :+ stackChar)
            else acc.getOrElseUpdate(index, Vector(stackChar))
        )
        acc
      )

  val action = raw"move (\d{1,2}) from (\d{1}) to (\d{1})".r

  def topCrates(lines: Iterator[String], keepCratesOrder: Boolean): String = {
    // Iterate over the starting stacks by reversing them and pushing them in a stack data structure
    val initializedStacks: Map[Int, Stack[Char]] = startingStacks.map((k, v) =>
      val s = Stack.empty[Char]
      v.reverse.foreach(s.push)
      (k, s)
    )

    lines
      .drop(2)
      .foreach(l =>
        l match {
          case action(amount, from, to) =>
            val moveCrates =
              (1 to amount.toInt).map(_ => initializedStacks(from.toInt).pop)

            val crates =
              if keepCratesOrder
              then moveCrates.reverse
              else moveCrates
            initializedStacks(to.toInt).pushAll(crates)
        }
      )

    // Iterate through all stacks by popping them and concatenating their values into a single string
    initializedStacks.values.map(_.pop).mkString
  }

  val (a, b) = lines.duplicate
  println(s"The answer to part 1 is: ${topCrates(a, false)}")
  println(s"The answer to part 2 is: ${topCrates(b, true)}")
