#!/usr/bin/env -S scala-cli -S 3

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines

def findNextBattery(restOfBank: List[Int]): Option[Int] =
  if restOfBank.isEmpty then None
  else Some(restOfBank.max)

var combinations = Map[Int, Int]()

@main def main(): Unit =
  for (bank, bankNr) <- input.zipWithIndex do
    var largestCombination = 0

    for (battery, index) <- bank.view.zipWithIndex do
      val batteryLevel = battery.asDigit

      //split the part after the battery
      val restOfBank = bank.view.drop(index + 1).map(_.asDigit).toList

      if(!restOfBank.isEmpty) {
        val nextBattery = findNextBattery(restOfBank)

        val batteryCombination = s"$battery${nextBattery.get}".toInt
        if(batteryCombination > largestCombination) {
          largestCombination = batteryCombination
        }
      }
    combinations += (bankNr -> largestCombination)

  // Print combinations
  for ((bankNr, batteryCombination) <- combinations) {
    println(s"bank $bankNr: $batteryCombination")
  }

  println("Answer: " + combinations.values.sum)

