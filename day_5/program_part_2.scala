#!/usr/bin/env -S scala-cli -S 3

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines

var answer = 0L
var ranges = Array[(Long, Long)]()

@main def main(): Unit = {
  var parsingRanges = true

  for line <- input do
    println(line)
    if(line.isEmpty) {
      parsingRanges = false
    } else {
      if(parsingRanges) {
        val range = line.split("-").map(_.toLong)
        ranges = ranges.appended((range(0), range(1)))
      } else {
        ranges = ranges.sortBy(_._1).foldLeft(List[(Long, Long)]()) {
          case (Nil, curr) => List(curr)
          case (acc :+ previousRange, currentRange) =>
            if (currentRange._1 <= previousRange._2) {
              acc :+ (previousRange._1, math.max(previousRange._2, currentRange._2))
            }
            else acc :+ previousRange :+ currentRange
        }.toArray
      }
    }

  for((start, end) <- ranges) {
    answer += (end - start + 1)
  }

  println(s"Answer: $answer")
}





