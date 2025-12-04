#!/usr/bin/env -S scala-cli -S 3

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines

val grid = input.map(_.toList).toList
val directions = List(
  (-1, -1), (-1, 0), (-1, 1),
  (0 , -1),          (0 , 1),
  (1 , -1), (1 , 0), (1 , 1)
)

var answer = 0

@main def main(): Unit =
  for
   (row, y) <- grid.zipWithIndex
   (ch, x) <- row.zipWithIndex
  do
    // For all 8 directions, count if there is a "."
    if(ch == '@') {
      var count = 0
      for (dirX, dirY) <- directions do
        val newX = x + dirX
        val newY = y + dirY
        if (newX >= 0 && newX < row.length && newY >= 0 && newY < grid.length) {
          if (grid(newY)(newX) == '@') {
            count += 1
          }
        }
//      println(s"At position ($x, $y) found $count dots around")
      if(count < 4)
        answer += 1
    }

  println(s"Answer: $answer")


