#!/usr/bin/env -S scala-cli -S 3

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines

var ranges = Array[(Long, Long)]()
var answer = 0

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
        var found = false
        for((start, end) <- ranges) {
          val ingriedientId = line.toLong
          print(s"Checking $ingriedientId against range ($start, $end)")
          if(ingriedientId >= start && ingriedientId <= end && found == false) {
            print(" - in range!")
            answer += 1
            found = true
          }
          print("\n")
        }
      }
    }

  println(s"Answer: $answer")
}





