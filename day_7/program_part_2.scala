#!/usr/bin/env -S scala-cli -S 3

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines

val grid = input.map(_.toList).toList
var answer = 1L

@main def main(): Unit = {
  var start: (Int, Int) = (-1, -1)
  for (x <- 0 until grid(0).length) {
    if (grid(0)(x) == 'S') {
      start = (0, x)
    }
  }

  var beamEnds = Array[((Int, Int), Int)]()
  beamEnds :+= (start, 1)

  while(!beamEnds.isEmpty) {
    println(s"Beam ends: ${beamEnds.mkString(", ")}")
    for(beamEnd <- beamEnds) {
      var ((y, x), beamCount) = beamEnd
      beamEnds = beamEnds.filterNot(_ == beamEnd)
      if (y + 1 < grid.length) {

        val nextPoint = grid(y + 1)(x)
        if (nextPoint == '.') {
          if(beamEnds.exists(_._1 == (y + 1, x))) {
            beamEnds = beamEnds.map(e => if(e._1 == (y + 1, x)) (e._1, beamCount + e._2) else e)
          } else {
            beamEnds :+= ((y + 1, x), beamCount)
          }
        } else if (nextPoint == '^') {
//          answer += beamCount
          // Beam splits left and right
          val leftBeam = (y + 1, x - 1)
          if (x - 1 >= 0) {
            if(beamEnds.exists(_._1 == leftBeam)) {
              beamEnds = beamEnds.map(e => if(e._1 == leftBeam) (e._1, beamCount + e._2) else e)
            } else {
              beamEnds :+= (leftBeam, beamCount)
            }
          }
          val rightBeam = (y + 1, x + 1)
          if (x + 1 < grid(0).length) {
            if(beamEnds.exists(_._1 == rightBeam)) {
              beamEnds = beamEnds.map(e => if(e._1 == rightBeam) (e._1, beamCount + e._2) else e)
            } else {
              beamEnds :+= (rightBeam, beamCount)
            }
          }
        }
      } else {
        // reached bottom
        answer += beamCount
      }
    }
  }


  println(s"Answer: $answer")
}





