package day7

import util.FileUtil
import scala.annotation.newMain
import scala.collection.mutable.Map
import scala.annotation.tailrec

// https://adventofcode.com/2022/day/7
@main
def myApp =
  case class TreeNode(
      val isDir: Boolean,
      var size: Int,
      var children: Map[String, TreeNode],
      var parent: Option[TreeNode]
  )

  val numberPattern = raw"(\d+)".r

  object TreeNode {
    def apply(input: List[String]): TreeNode =

      @tailrec
      def loop(input: List[String], treeNode: TreeNode): Unit =
        input match
          case head :: next =>
            if head.contains("cd ..")
              // the input never cd-s above root
            then
              treeNode.parent.get.size += treeNode.size
              loop(next, treeNode.parent.get)
            else if head.contains("$ cd")
            then
              val dirName = head.split(" ")(2)
              // we assume the input doesn't contain bogus "$ cd <name>"
              loop(next, treeNode.children.get(dirName).get)
            else if numberPattern.findFirstIn(head).isDefined
            then
              val size = numberPattern.findFirstIn(head).get.toInt
              treeNode.size += size
              val newNode =
                TreeNode(
                  isDir = false,
                  size = size,
                  children = Map.empty[String, TreeNode],
                  parent = Some(treeNode)
                )
              loop(next, treeNode)
            else if head.startsWith("dir")
            then
              val dirName = head.split(" ")(1)
              val newNode =
                TreeNode(
                  isDir = true,
                  size = 0,
                  children = Map.empty[String, TreeNode],
                  parent = Some(treeNode)
                )
              treeNode.children.getOrElseUpdate(dirName, newNode)
              loop(next, treeNode)
            // skip "$ ls"
            else loop(next, treeNode)
          case Nil => ()

      val root = TreeNode(
        isDir = true,
        size = 0,
        children = Map.empty[String, TreeNode],
        parent = None
      )
      loop(input, root)
      root
  }

  def findDirClosest30M(folderTree: TreeNode): Int =
    // 30000000 - (70000000 - max folder size)
    val maxFolderSize = 6552309

    def loop(folderTree: TreeNode, min: Int): Int =
      val newMin =
        if folderTree.isDir && folderTree.size >= maxFolderSize && folderTree.size < min
        then folderTree.size
        else min

      val iter = folderTree.children.values
        .map(loop(_, newMin))

      if iter.isEmpty
      then newMin
      else iter.min

    loop(folderTree, 70000000)

  val input = FileUtil
    .getLinesList(7)
    .drop(1)

  val folderTree = TreeNode(input)

  def folderSizes(folderTree: TreeNode): List[Int] =
    def loop(folderTree: TreeNode, l: List[Int]): List[Int] =
      val updatedList =
        if folderTree.isDir
        then l :+ folderTree.size
        else l

      updatedList ::: folderTree.children.values
        .flatMap(loop(_, updatedList))
        .toList

    loop(folderTree, List.empty[Int])

  println(s"The answer to part 1 is: ${folderSizes(folderTree).filter(_ <= 100000).sum}")
  println(s"The answer to part 2 is: ${findDirClosest30M(folderTree)}")
