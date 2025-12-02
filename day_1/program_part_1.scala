#!/usr/bin/env -S scala-cli -S 3

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines
var dialPosition = (50)
var answer = 0

def rotate(direction: Char, amountOfClicks: Int): Int =
  direction match
    case 'L' => ((dialPosition - amountOfClicks) % 100 + 100) % 100
    case 'R' => (dialPosition + amountOfClicks) % 100


@main def main(): Unit =
  for line <- input do
    val (direction, amountOfClicks) = line.splitAt(1)
    dialPosition = rotate(direction.charAt(0), amountOfClicks.toInt)
    if(dialPosition == 0) answer += 1
    println(s"Dial is now at position: $dialPosition")

  println("anwer: " + answer)