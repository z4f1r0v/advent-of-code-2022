package util

import os.pwd

object FileUtil {
  def readInput(day: Int): String = os.read(pwd / "src" / "main" / "scala" / s"day${day}" / "input")
  
  def getLinesIterator(day: Int): Iterator[String] = readInput(day).linesIterator

  def getLinesList(day: Int): List[String] = getLinesIterator(day).toList
}
