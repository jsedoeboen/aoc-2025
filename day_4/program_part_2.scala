#!/usr/bin/env -S scala-cli -S 3

import scala.collection.mutable

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines

val grid = input.map(_.toArray).toArray
val directions = List(
  (-1, -1), (-1, 0), (-1, 1),
  (0, -1),           (0, 1),
  (1, -1), (1, 0),   (1, 1)
)

var answer = 0
val rollsToRecheck = mutable.Set[(Int, Int)]()

def checkRoll(cell: (Int, Int)): Unit = {
  val (y, x) = cell
  val ch = grid(y)(x)

  if (ch == '@') {
    var count = 0
    for (dirX, dirY) <- directions do {
      val newX = x + dirX
      val newY = y + dirY
      if (newX >= 0 && newX < grid.length && newY >= 0 && newY < grid.length) {
        if (grid(newY)(newX) == '@') {
          count += 1
        }
      }
    }
    //      println(s"At position ($x, $y) found $count dots around")
    if (count < 4) {
      // Add all neighboring rolls to recheck list
      for (dirX, dirY) <- directions do {
        val newX = x + dirX
        val newY = y + dirY
        if (newX >= 0 && newX < grid.length && newY >= 0 && newY < grid.length && grid(newY)(newX) == '@') {
          rollsToRecheck += ((newY, newX))
        }
      }

      // Remove self
      grid(y)(x) = '.'
      answer += 1
      println(s"Roll found at ($x, $y) with $count @ around char is now " + grid(y)(x))
    }
  }
}

@main def main(): Unit = {
  for
    (row, y) <- grid.zipWithIndex
    (ch, x) <- row.zipWithIndex
  do {
    // For all 8 directions, count if there is a "."
    checkRoll ((y, x) )
  }

  while (! rollsToRecheck.isEmpty) do {
//    println(s"Rolls to recheck: ${rollsToRecheck.size}")

    val roll = rollsToRecheck.head
    rollsToRecheck -= roll
    checkRoll ((roll._1, roll._2) )
  }

  println (s"Answer: $answer")
}


