#!/usr/bin/env -S scala-cli -S 3

import scala.collection.mutable.Map

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines
val LENGTH_REQUIRED = 12

def toMaybeLong(s:String): Long = {
  scala.util.Try(s.toLong).getOrElse(0)
}

// Caching for (bankNr, batteryPosition) -> largestCombination
val caching = Map[(Int, Int, Long), List[Long]]()

def findNextBatteries(bankNr: Int, restOfBank: List[Long], amountLeft: Long): List[Long] = {
  if restOfBank.isEmpty || amountLeft == 0 then List()
  else if (restOfBank.length <= amountLeft) restOfBank
  else if (caching.contains((bankNr, restOfBank.length, amountLeft))) caching((bankNr, restOfBank.length, amountLeft))
  else {
    var largestCombination = List[Long]()
    for (battery, index) <- restOfBank.zipWithIndex do {
      val rest = restOfBank.drop(index + 1)
//      println(s"bankNr: $bankNr, amountLeft: $amountLeft, battery: $battery, restOfBank: $rest")
      val nextBatteries = findNextBatteries(bankNr, rest, amountLeft-1)
      val newCombination = List(battery) ++ nextBatteries

      if (newCombination.length == amountLeft && toMaybeLong(largestCombination.mkString) < toMaybeLong(newCombination.mkString)) {
        largestCombination = newCombination
      }

    }
//    println(s"bankNr: $bankNr, amountLeft: $amountLeft, largestCombination: ${largestCombination.mkString}")
//    println("saving cache for bankNr " + bankNr + " at position " + restOfBank.length + ": " + largestCombination.mkString)
    caching += ((bankNr, restOfBank.length, amountLeft) -> largestCombination)
    largestCombination
  }
}

var combinations = Map[String, Long]()

@main def main(): Unit =
  for (bank, bankNr) <- input.zipWithIndex do
    println(bankNr)
    combinations += (bank -> toMaybeLong(findNextBatteries(bankNr, bank.split("").map(_.toLong).toList, LENGTH_REQUIRED).mkString))

  for ((bank, batteryCombination) <- combinations) {
    println(s"$bank: $batteryCombination")
  }

  println("Answer: " + combinations.values.sum)

