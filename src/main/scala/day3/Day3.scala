package day3

import util.FileUtil

// https://adventofcode.com/2022/day/3
@main
def myApp =
  val day3Lines = FileUtil.getLinesList(3)

  val aToZ = ('a' to 'z').zip((1 to 26))
  val capsAtoZ = ('A' to 'Z').zip((27 to 52))
  val priorityConverter = (aToZ ++ capsAtoZ).toMap
  val toCharSet = (s: String) => s.toCharArray().toSet

  val prioritySum =
    day3Lines
      .map(l =>
        val (f, s) = l.splitAt(l.length() / 2)
        val duplicate = toCharSet(f).intersect(toCharSet(s)).head
        priorityConverter(duplicate)
      )
      .sum

  println(s"Answer to part 1 is: $prioritySum")

  val prioritySum2 =
    day3Lines
      .grouped(3)
      .map(g =>
        val duplicate =
          toCharSet(g(0))
            .intersect(toCharSet(g(1)))
            .intersect(toCharSet(g(2)))
            .head
        priorityConverter(duplicate)
      )
      .sum

  println(s"Answer to part 2 is: $prioritySum2")
