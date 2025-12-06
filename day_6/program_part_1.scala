#!/usr/bin/env -S scala-cli -S 3

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines

val grid = input.map(_.trim.split("\\s+").toList).toList
var answer = 0L

@main def main(): Unit = {
  for(x <- 0 until grid(0).length) {
    var numberList = List[Long]()
    var operator = ""

    for(y <- 0 until grid.length) {
      val cell = grid(y)(x)

      if(!cell.isBlank) {
        if (y != grid.length -1) {
          val number = cell.toString.toLong
          numberList = numberList :+ number
        } else {
          operator = cell
        }
      }
    }

    if(operator == "+") {
      println( "Adding numbers: " + numberList.mkString(", ") )
      answer += numberList.sum
    } else if(operator == "*") {
      println( "Multiplying numbers: " + numberList.mkString(", ") )
      answer += numberList.product
    }
  }

  println(s"Answer: $answer")
}





