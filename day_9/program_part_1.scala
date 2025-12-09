#!/usr/bin/env -S scala-cli -S 3

import scala.collection.mutable.ListBuffer

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines

val redTiles = ListBuffer[Tile]()

class Tile(val x: Int, val y: Int) {
  def surfaceArea(tile: Tile): Long = {
    val dx = Math.abs(x - tile.x) + 1L
    val dy = Math.abs(y - tile.y) + 1L
    dx * dy
  }
}

@main def main(): Unit = {
  for(line <- input) {
    val coords = line.split(",").map(_.toInt)
    redTiles += Tile(coords(0), coords(1))
  }

  var bigestSquare = 0L
  for((redTile, i) <- redTiles.zipWithIndex) {
    for(j <- i + 1 until redTiles.size) {
      val otherTile = redTiles.toList(j)
      val surfaceArea = redTile.surfaceArea(otherTile)

      bigestSquare = Math.max(bigestSquare, surfaceArea)
    }
  }

  println(s"Answer: $bigestSquare")
}





