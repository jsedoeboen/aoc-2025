#!/usr/bin/env -S scala-cli -S 3

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines
var dialPosition = 50
var answer = 0

def rotate(direction: Char, amountOfClicks: Int): Int =
  direction match
    case 'L' => ((dialPosition - amountOfClicks) % 100 + 100) % 100
    case 'R' => (dialPosition + amountOfClicks) % 100


@main def main(): Unit =
  for line <- input do
    print("Dial: " + dialPosition + " " + line)
    val (direction, amountOfClicks) = line.splitAt(1)
    val newDialPosition = rotate(direction.charAt(0), amountOfClicks.toInt)
    println(s" = $newDialPosition")

    val rotations = amountOfClicks.toInt / 100
    val remainder = amountOfClicks.toInt % 100
    answer += rotations

    if(direction == "R" && dialPosition + remainder >= 100) {
      val a = if(dialPosition != 0) 1 else 0
      answer += a
      if(a != 0 || rotations != 0) println("+= " + a + " + " + rotations + " = " + answer)
    } else if(direction == "L" && dialPosition - remainder <= 0) {
      val a = if(dialPosition != 0) 1 else 0
      answer += a
      if(a != 0 || rotations != 0) println(" += " + a + " + " + rotations + " = " + answer)
    } else {
      if(rotations != 0) println(" += " + rotations)
    }
    

    dialPosition = newDialPosition

  println("anwer: " + answer)