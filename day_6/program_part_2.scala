#!/usr/bin/env -S scala-cli -S 3

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines

val lines = input.toSeq
val maxLength = lines.map(_.length).max
val paddedLines = lines.map(_.padTo(maxLength, ' ')) // pad because test code has trimmed trailing whitespace
val grid = paddedLines.transpose.appended(List.fill(maxLength)(' '))


var answer = 0L

@main def main(): Unit = {

  var numberList = Array[Long]()
  var operator = '+'
  for(y <- 0 until grid.length) {
    if(!grid(y).last.isWhitespace) {
      operator = grid(y).last
    }

    val numberStr = grid(y).dropRight(1).mkString.trim
    println(s"numberStr: $numberStr")
    if(numberStr.isEmpty) {
        // Do calculation

        if(operator == '+') {
          println( "Adding numbers: " + numberList.mkString(", ") + " = " + numberList.sum )
          answer += numberList.sum
        } else if(operator == '*') {
          println( "Multiplying numbers: " + numberList.mkString(", ") + " = " + numberList.product )
          answer += numberList.product
        }
      numberList = Array[Long]()
    } else {
      numberList = numberList :+ numberStr.toLong
    }
  }

  println(s"Answer: $answer")
}





