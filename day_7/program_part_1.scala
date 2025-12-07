#!/usr/bin/env -S scala-cli -S 3

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines

val grid = input.map(_.toList).toList
var answer = 0L

@main def main(): Unit = {
  var start: (Int, Int) = (-1, -1)
  for (x <- 0 until grid(0).length) {
    if (grid(0)(x) == 'S') {
      start = (0, x)
    }
  }

  var beamEnds = Set[(Int, Int)]()
  beamEnds += start

  while(!beamEnds.isEmpty) {
    println(s"Beam ends: ${beamEnds}")
    for((beamEnd, i) <- beamEnds.zipWithIndex) {
      var (y, x) = beamEnd
      beamEnds -= beamEnd
      if (y + 1 < grid.length) {


        val nextPoint = grid(y + 1)(x)
        if (nextPoint == '.') {
          beamEnds += (y + 1, x)
        } else if (nextPoint == '^') {
          answer += 1
          println(s"Beam reached position and split (${y + 1}, $x)")
          // Beam splits left and right
          val leftBeam = (y + 1, x - 1)
          if (x - 1 >= 0 && !beamEnds.contains(leftBeam)) {
            beamEnds += leftBeam
          }
          val rightBeam = (y + 1, x + 1)
          if (x + 1 < grid(0).length && !beamEnds.contains(rightBeam)) {
            beamEnds += rightBeam
          }
        }
      }
    }
  }


  println(s"Answer: $answer")
}





