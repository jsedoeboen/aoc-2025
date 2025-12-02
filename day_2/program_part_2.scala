#!/usr/bin/env -S scala-cli -S 3

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines.next()

val ranges: Iterator[(Long, Long)] =
  input
    .split(",")                // split on commas
    .iterator
    .map(_.trim)               // remove spaces/newlines
    .filter(_.nonEmpty)        // ignore empty parts
    .map { part =>
      part.split("-") match
        case Array(start, end) => (start.toLong, end.toLong)
        case other => sys.error(s"Invalid range: '$part'")
    }

var answer = 0L

@main def main(): Unit =
  ranges.foreach { (from, to) =>
    println(s"From: $from, To: $to")

    for value <- from to to do
      val chars = value.toString.toCharArray
      var hasRepeating = false

      for groupSize <- 1L to (chars.length /2) do {
      val isRepeating = chars.grouped(groupSize.toInt)
        .sliding(2)
        .forall {
          case Seq(a, b) => a.mkString == b.mkString
          case other => false
        }

        if (isRepeating && !hasRepeating) hasRepeating = true
      }


      if(hasRepeating) {
        println(value)
        answer += value
      }
  }

  println("answer: " + answer)